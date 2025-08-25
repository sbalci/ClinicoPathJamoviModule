#' @title Modality Comparison Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import dplyr
#' @importFrom irr kappa2
#' @importFrom scales percent
#' @importFrom reshape2 melt
#'
#' @description
#' Modality Comparison Analysis for digital pathology validation studies.

modalitycomparisonClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "modalitycomparisonClass",
    inherit = modalitycomparisonBase,
    private = list(
        # Private data storage
        .modality1_data = NULL,
        .modality2_data = NULL,
        .case_ids = NULL,
        .categories = NULL,
        .n_cases = NULL,
        .agreement_matrix = NULL,
        .discordance_patterns = NULL,
        .low_end_categories = NULL,

        # Initialization
        .init = function() {
            if (is.null(self$data) || 
                is.null(self$options$modality1_var) || 
                is.null(self$options$modality2_var)) {
                todo <- "
                    <br>Welcome to Modality Comparison Analysis
                    <br><br>
                    This tool analyzes agreement between two diagnostic modalities
                    (e.g., glass slides vs digital images) with specialized metrics
                    for pathology applications.
                    <br><br>
                    To begin:
                    <ul>
                        <li>Select Modality 1 variable (e.g., glass slide scores)</li>
                        <li>Select Modality 2 variable (e.g., digital image scores)</li>
                        <li>Optionally select a case identifier variable</li>
                        <li>Choose appropriate score categories for your data</li>
                    </ul>
                    <br>
                    The analysis will provide:
                    <ul>
                        <li>Overall concordance and agreement statistics</li>
                        <li>Discordance pattern analysis</li>
                        <li>Directional bias assessment</li>
                        <li>Specialized low-end category analysis (for HER2)</li>
                    </ul>
                "
                self$results$todo$setContent(todo)
            }
        },

        # Main analysis function
        .run = function() {
            # Early return if required variables not selected
            if (is.null(self$options$modality1_var) || 
                is.null(self$options$modality2_var)) {
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
            private$.performAgreementAnalysis()

            if (self$options$show_contingency_table) {
                private$.generateContingencyTable()
            }

            if (self$options$show_discordance_analysis) {
                private$.performDiscordanceAnalysis()
            }

            if (self$options$directional_analysis) {
                private$.performDirectionalAnalysis()
            }

            if (self$options$low_end_focus) {
                private$.performLowEndAnalysis()
            }
        },

        # Data preparation
        .prepareData = function() {
            # Extract modality variables
            modality1_var <- self$options$modality1_var
            modality2_var <- self$options$modality2_var
            case_id_var <- self$options$case_id

            # Get data vectors
            private$.modality1_data <- self$data[[modality1_var]]
            private$.modality2_data <- self$data[[modality2_var]]

            # Handle case IDs
            if (!is.null(case_id_var)) {
                private$.case_ids <- as.character(self$data[[case_id_var]])
            } else {
                private$.case_ids <- paste("Case", seq_len(nrow(self$data)), sep = "_")
            }

            # Remove rows with missing values in either modality
            complete_cases <- complete.cases(private$.modality1_data, private$.modality2_data)
            
            if (sum(complete_cases) == 0) {
                stop('No complete cases found. Please check for missing values.')
            }

            private$.modality1_data <- private$.modality1_data[complete_cases]
            private$.modality2_data <- private$.modality2_data[complete_cases]
            private$.case_ids <- private$.case_ids[complete_cases]
            private$.n_cases <- sum(complete_cases)

            # Convert to character for consistent handling
            private$.modality1_data <- as.character(private$.modality1_data)
            private$.modality2_data <- as.character(private$.modality2_data)

            # Determine categories based on score_categories option
            private$.determineCategories()
        },

        # Determine score categories
        .determineCategories = function() {
            score_cat <- self$options$score_categories
            
            if (score_cat == "her2_5cat") {
                private$.categories <- c("Null", "Ultralow", "1+", "2+", "3+")
                private$.low_end_categories <- c("Null", "Ultralow", "1+")
            } else if (score_cat == "her2_4cat") {
                private$.categories <- c("0", "1+", "2+", "3+")
                private$.low_end_categories <- c("0", "1+")
            } else {
                # Auto-detect or custom
                all_values <- unique(c(private$.modality1_data, private$.modality2_data))
                private$.categories <- sort(all_values)
                
                # Try to identify low-end categories automatically
                low_patterns <- c("null", "0", "ultralow", "1+", "1\\+")
                private$.low_end_categories <- private$.categories[
                    grepl(paste(low_patterns, collapse = "|"), 
                          private$.categories, ignore.case = TRUE)
                ]
            }
        },

        # Overview analysis
        .performOverviewAnalysis = function() {
            # Calculate overall concordance
            concordant_cases <- sum(private$.modality1_data == private$.modality2_data)
            concordance_percent <- (concordant_cases / private$.n_cases) * 100
            discordant_cases <- private$.n_cases - concordant_cases

            # Populate overview table
            overview <- self$results$overviewTable
            overview$setRow(rowNo = 1, values = list(
                cases = private$.n_cases,
                modality1_name = self$options$modality1_name,
                modality2_name = self$options$modality2_name,
                categories = length(private$.categories),
                concordance_percent = concordance_percent,
                discordance_count = discordant_cases
            ))
        },

        # Agreement analysis
        .performAgreementAnalysis = function() {
            agreement_table <- self$results$agreementTable
            
            # Calculate overall agreement percentage
            concordant_cases <- sum(private$.modality1_data == private$.modality2_data)
            overall_agreement <- (concordant_cases / private$.n_cases) * 100
            
            # Calculate Cohen's kappa
            kappa_result <- tryCatch({
                data_frame <- data.frame(
                    mod1 = factor(private$.modality1_data, levels = private$.categories),
                    mod2 = factor(private$.modality2_data, levels = private$.categories)
                )
                irr::kappa2(data_frame)
            }, error = function(e) {
                list(value = NaN, p.value = NaN)
            })
            
            kappa_value <- kappa_result$value
            kappa_p <- kappa_result$p.value
            
            # Confidence intervals for kappa (if available)
            kappa_ci_lower <- NA
            kappa_ci_upper <- NA
            
            if (self$options$confidence_intervals && !is.na(kappa_value)) {
                # Try to calculate CI from kappa result
                if (!is.null(kappa_result$var.kappa) && !is.na(kappa_result$var.kappa)) {
                    se <- sqrt(kappa_result$var.kappa)
                    kappa_ci_lower <- kappa_value - 1.96 * se
                    kappa_ci_upper <- kappa_value + 1.96 * se
                }
            }
            
            # Add overall agreement
            agreement_table$addRow(rowKey = "overall", values = list(
                statistic = "Overall Agreement",
                value = overall_agreement,
                ci_lower = NA,
                ci_upper = NA,
                interpretation = private$.interpretAgreement(overall_agreement)
            ))
            
            # Add kappa
            agreement_table$addRow(rowKey = "kappa", values = list(
                statistic = "Cohen's Kappa",
                value = kappa_value,
                ci_lower = kappa_ci_lower,
                ci_upper = kappa_ci_upper,
                interpretation = private$.interpretKappa(kappa_value)
            ))
            
            # Weighted kappa if requested and appropriate
            if (self$options$calculate_weighted_kappa) {
                private$.calculateWeightedKappa(agreement_table)
            }
        },

        # Calculate weighted kappa
        .calculateWeightedKappa = function(table) {
            weighted_kappa <- tryCatch({
                data_frame <- data.frame(
                    mod1 = factor(private$.modality1_data, levels = private$.categories),
                    mod2 = factor(private$.modality2_data, levels = private$.categories)
                )
                
                # Try with linear weights first
                result <- irr::kappa2(data_frame, weight = "equal")
                result
            }, error = function(e) {
                list(value = NaN, p.value = NaN)
            })
            
            if (!is.na(weighted_kappa$value)) {
                table$addRow(rowKey = "weighted_kappa", values = list(
                    statistic = "Weighted Kappa (Linear)",
                    value = weighted_kappa$value,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretKappa(weighted_kappa$value)
                ))
            }
        },

        # Generate contingency table
        .generateContingencyTable = function() {
            contingency_table <- self$results$contingencyTable
            
            # Create cross-tabulation
            cross_tab <- table(private$.modality1_data, private$.modality2_data)
            
            # Add rows to table
            for (i in 1:nrow(cross_tab)) {
                mod1_score <- rownames(cross_tab)[i]
                
                values <- list(modality1_score = mod1_score, total = sum(cross_tab[i, ]))
                
                # Add columns based on score categories
                for (j in 1:ncol(cross_tab)) {
                    mod2_score <- colnames(cross_tab)[j]
                    
                    # Map to appropriate column names based on category system
                    if (self$options$score_categories == "her2_5cat") {
                        col_name <- switch(mod2_score,
                            "Null" = "modality2_null",
                            "Ultralow" = "modality2_ultralow", 
                            "1+" = "modality2_1plus",
                            "2+" = "modality2_2plus",
                            "3+" = "modality2_3plus",
                            paste0("modality2_", gsub("[^A-Za-z0-9]", "_", mod2_score))
                        )
                    } else if (self$options$score_categories == "her2_4cat") {
                        col_name <- switch(mod2_score,
                            "0" = "modality2_0",
                            "1+" = "modality2_1plus",
                            "2+" = "modality2_2plus", 
                            "3+" = "modality2_3plus",
                            paste0("modality2_", gsub("[^A-Za-z0-9]", "_", mod2_score))
                        )
                    } else {
                        col_name <- paste0("modality2_", gsub("[^A-Za-z0-9]", "_", mod2_score))
                    }
                    
                    values[[col_name]] <- cross_tab[i, j]
                }
                
                contingency_table$addRow(rowKey = mod1_score, values = values)
            }
        },

        # Discordance pattern analysis
        .performDiscordanceAnalysis = function() {
            discordance_table <- self$results$discordanceTable
            case_detail_table <- self$results$caseDetailTable
            
            # Find discordant cases
            discordant_indices <- which(private$.modality1_data != private$.modality2_data)
            
            if (length(discordant_indices) == 0) {
                discordance_table$addRow(rowKey = "none", values = list(
                    pattern = "No discordant cases",
                    count = 0,
                    percentage = 0,
                    direction = "N/A",
                    clinical_significance = "Perfect agreement"
                ))
                return()
            }
            
            # Analyze discordance patterns
            discordance_patterns <- data.frame(
                case_id = private$.case_ids[discordant_indices],
                mod1 = private$.modality1_data[discordant_indices],
                mod2 = private$.modality2_data[discordant_indices],
                stringsAsFactors = FALSE
            )
            
            # Create pattern descriptions
            discordance_patterns$pattern <- paste(discordance_patterns$mod1, "→", discordance_patterns$mod2)
            
            # Count pattern frequencies
            pattern_counts <- table(discordance_patterns$pattern)
            total_discordant <- length(discordant_indices)
            
            # Add to discordance table
            for (i in 1:length(pattern_counts)) {
                pattern <- names(pattern_counts)[i]
                count <- pattern_counts[i]
                percentage <- (count / total_discordant) * 100
                
                # Determine direction and clinical significance
                parts <- strsplit(pattern, " → ")[[1]]
                direction <- private$.determineDirection(parts[1], parts[2])
                clinical_sig <- private$.assessClinicalSignificance(parts[1], parts[2])
                
                discordance_table$addRow(rowKey = pattern, values = list(
                    pattern = pattern,
                    count = count,
                    percentage = percentage,
                    direction = direction,
                    clinical_significance = clinical_sig
                ))
            }
            
            # Add individual case details
            for (i in 1:nrow(discordance_patterns)) {
                case_detail_table$addRow(rowKey = discordance_patterns$case_id[i], values = list(
                    case_id = discordance_patterns$case_id[i],
                    modality1_score = discordance_patterns$mod1[i],
                    modality2_score = discordance_patterns$mod2[i],
                    discordance_type = private$.categorizeDiscordance(
                        discordance_patterns$mod1[i], 
                        discordance_patterns$mod2[i]
                    ),
                    clinical_impact = private$.assessClinicalSignificance(
                        discordance_patterns$mod1[i], 
                        discordance_patterns$mod2[i]
                    )
                ))
            }
        },

        # Directional bias analysis
        .performDirectionalAnalysis = function() {
            directional_table <- self$results$directionalBiasTable
            
            # Count higher/lower scores
            mod1_higher <- 0
            mod2_higher <- 0
            
            for (i in 1:private$.n_cases) {
                if (private$.modality1_data[i] != private$.modality2_data[i]) {
                    direction <- private$.compareScoreLevel(
                        private$.modality1_data[i], 
                        private$.modality2_data[i]
                    )
                    
                    if (direction > 0) {
                        mod1_higher <- mod1_higher + 1
                    } else if (direction < 0) {
                        mod2_higher <- mod2_higher + 1
                    }
                }
            }
            
            total_discordant <- mod1_higher + mod2_higher
            
            if (total_discordant > 0) {
                # Statistical test for systematic bias
                bias_test <- binom.test(mod2_higher, total_discordant, p = 0.5)
                
                directional_table$addRow(rowKey = "mod1_higher", values = list(
                    bias_type = paste(self$options$modality1_name, "Higher"),
                    count = mod1_higher,
                    percentage = (mod1_higher / total_discordant) * 100,
                    significance = if (bias_test$p.value < 0.05) "Significant" else "Not significant"
                ))
                
                directional_table$addRow(rowKey = "mod2_higher", values = list(
                    bias_type = paste(self$options$modality2_name, "Higher"),
                    count = mod2_higher,
                    percentage = (mod2_higher / total_discordant) * 100,
                    significance = if (bias_test$p.value < 0.05) "Significant" else "Not significant"
                ))
            }
        },

        # Low-end category analysis
        .performLowEndAnalysis = function() {
            if (length(private$.low_end_categories) == 0) return()
            
            low_end_table <- self$results$lowEndAnalysisTable
            
            # Filter data to low-end categories only
            low_end_indices <- which(
                private$.modality1_data %in% private$.low_end_categories |
                private$.modality2_data %in% private$.low_end_categories
            )
            
            if (length(low_end_indices) == 0) return()
            
            low_mod1 <- private$.modality1_data[low_end_indices]
            low_mod2 <- private$.modality2_data[low_end_indices]
            
            # Calculate agreement for low-end categories
            for (cat in private$.low_end_categories) {
                cat_indices <- which(low_mod1 == cat | low_mod2 == cat)
                
                if (length(cat_indices) == 0) next
                
                cat_mod1 <- low_mod1[cat_indices]
                cat_mod2 <- low_mod2[cat_indices]
                
                concordance <- sum(cat_mod1 == cat_mod2)
                discordance <- length(cat_indices) - concordance
                
                # Calculate kappa for this category vs others
                binary_mod1 <- ifelse(cat_mod1 == cat, 1, 0)
                binary_mod2 <- ifelse(cat_mod2 == cat, 1, 0)
                
                cat_kappa <- tryCatch({
                    irr::kappa2(data.frame(mod1 = binary_mod1, mod2 = binary_mod2))$value
                }, error = function(e) NA)
                
                low_end_table$addRow(rowKey = cat, values = list(
                    category_comparison = paste(cat, "identification"),
                    concordance = concordance,
                    discordance = discordance,
                    kappa = cat_kappa,
                    clinical_relevance = private$.assessLowEndRelevance(cat)
                ))
            }
        },

        # Helper functions
        .interpretAgreement = function(percent) {
            if (is.na(percent)) return("Cannot calculate")
            if (percent >= 95) return("Excellent")
            if (percent >= 90) return("Very good")
            if (percent >= 80) return("Good")
            if (percent >= 70) return("Moderate")
            return("Poor")
        },

        .interpretKappa = function(kappa) {
            if (is.na(kappa)) return("Cannot calculate")
            if (kappa < 0) return("Poor")
            if (kappa <= 0.20) return("Slight")
            if (kappa <= 0.40) return("Fair")
            if (kappa <= 0.60) return("Moderate")
            if (kappa <= 0.80) return("Substantial")
            return("Almost Perfect")
        },

        .determineDirection = function(score1, score2) {
            level_comp <- private$.compareScoreLevel(score1, score2)
            if (level_comp > 0) return("Modality 1 Higher")
            if (level_comp < 0) return("Modality 2 Higher")
            return("Lateral")
        },

        .compareScoreLevel = function(score1, score2) {
            # Define hierarchical order for common scoring systems
            if (self$options$score_categories == "her2_5cat") {
                order <- c("Null", "Ultralow", "1+", "2+", "3+")
            } else if (self$options$score_categories == "her2_4cat") {
                order <- c("0", "1+", "2+", "3+")
            } else {
                order <- private$.categories
            }
            
            pos1 <- match(score1, order)
            pos2 <- match(score2, order)
            
            if (is.na(pos1) || is.na(pos2)) return(0)
            return(pos1 - pos2)
        },

        .assessClinicalSignificance = function(score1, score2) {
            # HER2-specific clinical significance
            if (self$options$score_categories %in% c("her2_5cat", "her2_4cat")) {
                # Treatment-relevant transitions
                if ((score1 %in% c("0", "Null") && score2 %in% c("1+", "Ultralow")) ||
                    (score2 %in% c("0", "Null") && score1 %in% c("1+", "Ultralow"))) {
                    return("ADC treatment eligibility")
                }
                if ((score1 == "2+" && score2 == "3+") || (score1 == "3+" && score2 == "2+")) {
                    return("Anti-HER2 therapy eligibility")
                }
            }
            return("Categorical change")
        },

        .categorizeDiscordance = function(score1, score2) {
            level_diff <- abs(private$.compareScoreLevel(score1, score2))
            if (level_diff == 1) return("Adjacent category")
            if (level_diff == 2) return("Two-level difference")
            return("Major discordance")
        },

        .assessLowEndRelevance = function(category) {
            if (category %in% c("Null", "0")) {
                return("No therapy benefit")
            }
            if (category %in% c("Ultralow", "1+")) {
                return("ADC therapy candidate")
            }
            return("Clinical significance uncertain")
        },

        # Visualization functions
        .agreementPlot = function(image, ggtheme, theme, ...) {
            # Create agreement scatter plot
            plot_data <- data.frame(
                Modality1 = factor(private$.modality1_data, levels = private$.categories),
                Modality2 = factor(private$.modality2_data, levels = private$.categories),
                Agreement = private$.modality1_data == private$.modality2_data
            )
            
            p <- ggplot(plot_data, aes(x = Modality1, y = Modality2)) +
                geom_jitter(aes(color = Agreement), alpha = 0.7, width = 0.2, height = 0.2) +
                geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
                scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
                labs(
                    title = "Modality Agreement Plot",
                    x = self$options$modality1_name,
                    y = self$options$modality2_name,
                    color = "Agreement"
                ) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            print(p)
            TRUE
        },

        .discordancePlot = function(image, ggtheme, theme, ...) {
            # Create discordance pattern visualization
            if (!self$options$show_discordance_analysis) return()
            
            # Count discordance patterns
            discordant_indices <- which(private$.modality1_data != private$.modality2_data)
            
            if (length(discordant_indices) == 0) {
                p <- ggplot() +
                    annotate("text", x = 0.5, y = 0.5, label = "No discordant cases found", size = 6) +
                    xlim(0, 1) + ylim(0, 1) + theme_void()
                print(p)
                return(TRUE)
            }
            
            pattern_data <- data.frame(
                Pattern = paste(private$.modality1_data[discordant_indices], "→", 
                               private$.modality2_data[discordant_indices]),
                stringsAsFactors = FALSE
            )
            
            pattern_counts <- as.data.frame(table(pattern_data$Pattern))
            names(pattern_counts) <- c("Pattern", "Count")
            
            p <- ggplot(pattern_counts, aes(x = reorder(Pattern, Count), y = Count)) +
                geom_col(fill = "steelblue", alpha = 0.7) +
                coord_flip() +
                labs(
                    title = "Discordance Patterns",
                    x = "Pattern",
                    y = "Frequency"
                ) +
                theme_minimal()
            
            print(p)
            TRUE
        }
    )
)