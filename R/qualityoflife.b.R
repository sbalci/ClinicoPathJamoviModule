#' @title Quality of Life Analysis & Patient-Centered Outcomes
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats cor t.test wilcox.test aov kruskal.test
#' @export


qualityoflifeClass <- R6::R6Class(
    "qualityoflifeClass",
    inherit = qualityoflifeBase,
    private = list(
        .domain_data = NULL,
        .domain_scores = NULL,
        .summary_scores = NULL,
        .health_utilities = NULL,
        .quality_results = NULL,
        .clinical_interpretation = NULL,
        .group_results = NULL,
        .longitudinal_results = NULL,
        
        .init = function() {
            # Check if at least one domain has items
            domain_items <- c(
                self$options$physical_function_items,
                self$options$mental_health_items,
                self$options$role_physical_items,
                self$options$role_emotional_items,
                self$options$bodily_pain_items,
                self$options$general_health_items,
                self$options$vitality_items,
                self$options$social_function_items,
                self$options$symptom_items,
                self$options$functional_items,
                self$options$global_qol_items
            )
            
            if (length(domain_items) == 0) {
                self$results$qol_overview$setNote("note", "At least one QoL domain must have items specified")
                return()
            }
            
            # Set table titles
            self$results$qol_overview$setTitle("Quality of Life Analysis Overview")
            self$results$domain_scores$setTitle("QoL Domain Score Summary")
            self$results$summary_scores$setTitle("Summary QoL Scores")
        },
        
        .run = function() {
            # Check if we have domain items
            domain_items <- c(
                self$options$physical_function_items,
                self$options$mental_health_items,
                self$options$role_physical_items,
                self$options$role_emotional_items,
                self$options$bodily_pain_items,
                self$options$general_health_items,
                self$options$vitality_items,
                self$options$social_function_items,
                self$options$symptom_items,
                self$options$functional_items,
                self$options$global_qol_items
            )
            
            if (length(domain_items) == 0) {
                return()
            }
            
            # Check for required packages
            private$.checkPackages()
            
            # Process QoL domain data
            private$.processDomainData()
            
            # Calculate domain scores
            private$.calculateDomainScores()
            
            # Calculate summary scores
            private$.calculateSummaryScores()
            
            # Health utilities calculation
            if (self$options$health_utilities) {
                private$.calculateHealthUtilities()
            }
            
            # Quality control assessment
            if (self$options$quality_control) {
                private$.qualityControlAssessment()
            }
            
            # Clinical interpretation
            if (self$options$clinical_interpretation) {
                private$.clinicalInterpretation()
            }
            
            # Group comparisons
            if (self$options$group_comparisons && !is.null(self$options$group_var)) {
                private$.groupComparisons()
            }
            
            # Longitudinal analysis
            if (self$options$longitudinal_analysis && !is.null(self$options$time_var)) {
                private$.longitudinalAnalysis()
            }
            
            # Ceiling/floor effects
            if (self$options$ceiling_floor_analysis) {
                private$.ceilingFloorAnalysis()
            }
            
            # Domain correlations
            if (self$options$domain_correlations) {
                private$.domainCorrelations()
            }
            
            # Update tables
            private$.populateTables()
        },
        
        .checkPackages = function() {
            # Required packages for QoL analysis
            required_packages <- c("psych", "corrplot")
            
            # Additional packages for specific methods
            if (self$options$health_utilities || self$options$utility_algorithm %in% c("sf6d", "eq5d_uk", "eq5d_us")) {
                required_packages <- c(required_packages, "eq5d")
            }
            
            if (self$options$factor_analysis) {
                required_packages <- c(required_packages, "GPArotation", "lavaan")
            }
            
            if (self$options$clustering_analysis) {
                required_packages <- c(required_packages, "cluster")
            }
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    tryCatch({
                        install.packages(pkg, repos = "https://cran.rstudio.com/")
                    }, error = function(e) {
                        self$results$qol_overview$setNote("error", paste("Failed to install package:", pkg))
                    })
                }
            }
        },
        
        .processDomainData = function() {
            data <- self$data
            
            # Define QoL domains and their items
            domains <- list()
            
            if (length(self$options$physical_function_items) > 0) {
                domains[["Physical Function"]] <- self$options$physical_function_items
            }
            if (length(self$options$role_physical_items) > 0) {
                domains[["Role Physical"]] <- self$options$role_physical_items
            }
            if (length(self$options$bodily_pain_items) > 0) {
                domains[["Bodily Pain"]] <- self$options$bodily_pain_items
            }
            if (length(self$options$general_health_items) > 0) {
                domains[["General Health"]] <- self$options$general_health_items
            }
            if (length(self$options$vitality_items) > 0) {
                domains[["Vitality"]] <- self$options$vitality_items
            }
            if (length(self$options$social_function_items) > 0) {
                domains[["Social Function"]] <- self$options$social_function_items
            }
            if (length(self$options$role_emotional_items) > 0) {
                domains[["Role Emotional"]] <- self$options$role_emotional_items
            }
            if (length(self$options$mental_health_items) > 0) {
                domains[["Mental Health"]] <- self$options$mental_health_items
            }
            if (length(self$options$symptom_items) > 0) {
                domains[["Symptoms"]] <- self$options$symptom_items
            }
            if (length(self$options$functional_items) > 0) {
                domains[["Functional"]] <- self$options$functional_items
            }
            if (length(self$options$global_qol_items) > 0) {
                domains[["Global QoL"]] <- self$options$global_qol_items
            }
            
            # Extract domain data
            domain_data <- list()
            all_items <- unlist(domains)
            
            for (domain_name in names(domains)) {
                domain_items <- domains[[domain_name]]
                domain_data[[domain_name]] <- data[, domain_items, drop = FALSE]
            }
            
            private$.domain_data <- domain_data
        },
        
        .calculateDomainScores = function() {
            if (is.null(private$.domain_data)) return()
            
            domain_data <- private$.domain_data
            scoring_method <- self$options$scoring_algorithm
            missing_threshold <- self$options$missing_domain_threshold
            
            domain_scores <- list()
            
            for (domain_name in names(domain_data)) {
                domain_items <- domain_data[[domain_name]]
                n_items <- ncol(domain_items)
                scores <- numeric(nrow(domain_items))
                
                for (i in seq_len(nrow(domain_items))) {
                    row_data <- domain_items[i, ]
                    n_valid <- sum(!is.na(row_data))
                    
                    # Check missing data threshold
                    if ((n_valid / n_items) >= (1 - missing_threshold)) {
                        valid_responses <- as.numeric(row_data[!is.na(row_data)])
                        
                        if (scoring_method == "standard") {
                            # Standard 0-100 scoring for most QoL instruments
                            if (self$options$instrument_type %in% c("sf36", "sf12")) {
                                # SF scoring algorithm (simplified)
                                raw_score <- sum(valid_responses) * (n_items / n_valid)
                                min_possible <- n_items * 1  # Assuming 1 is minimum response
                                max_possible <- n_items * 5  # Assuming 5 is maximum response
                                scores[i] <- ((raw_score - min_possible) / (max_possible - min_possible)) * 100
                            } else {
                                # Generic linear transformation to 0-100
                                raw_score <- mean(valid_responses)
                                scores[i] <- (raw_score - 1) / 4 * 100  # Assuming 1-5 scale
                            }
                        } else if (scoring_method == "norm_based") {
                            # Norm-based scoring (T-scores: mean=50, SD=10)
                            raw_score <- mean(valid_responses)
                            # This would typically use population norms
                            scores[i] <- 50 + (raw_score - 3) * 10  # Simplified
                        } else {
                            # Simple mean scoring
                            scores[i] <- mean(valid_responses)
                        }
                    } else {
                        scores[i] <- NA
                    }
                }
                
                domain_scores[[domain_name]] <- scores
            }
            
            private$.domain_scores <- domain_scores
        },
        
        .calculateSummaryScores = function() {
            if (is.null(private$.domain_scores)) return()
            
            domain_scores <- private$.domain_scores
            
            # Calculate summary scores based on instrument type
            summary_scores <- list()
            
            if (self$options$instrument_type %in% c("sf36", "sf12")) {
                # SF-36/SF-12 Physical and Mental Component Summary scores
                physical_domains <- c("Physical Function", "Role Physical", "Bodily Pain", "General Health")
                mental_domains <- c("Vitality", "Social Function", "Role Emotional", "Mental Health")
                
                # Physical Component Summary (PCS)
                physical_scores <- domain_scores[names(domain_scores) %in% physical_domains]
                if (length(physical_scores) > 0) {
                    pcs_matrix <- do.call(cbind, physical_scores)
                    summary_scores[["Physical Component Summary"]] <- rowMeans(pcs_matrix, na.rm = TRUE)
                }
                
                # Mental Component Summary (MCS)
                mental_scores <- domain_scores[names(domain_scores) %in% mental_domains]
                if (length(mental_scores) > 0) {
                    mcs_matrix <- do.call(cbind, mental_scores)
                    summary_scores[["Mental Component Summary"]] <- rowMeans(mcs_matrix, na.rm = TRUE)
                }
            } else if (self$options$instrument_type == "eortc_qlq_c30") {
                # EORTC QLQ-C30 Global Health Status
                if ("Global QoL" %in% names(domain_scores)) {
                    summary_scores[["Global Health Status"]] <- domain_scores[["Global QoL"]]
                }
            } else {
                # Generic overall QoL score
                all_domain_scores <- do.call(cbind, domain_scores)
                summary_scores[["Overall QoL"]] <- rowMeans(all_domain_scores, na.rm = TRUE)
            }
            
            private$.summary_scores <- summary_scores
        },
        
        .calculateHealthUtilities = function() {
            if (!self$options$health_utilities) return()
            
            domain_scores <- private$.domain_scores
            algorithm <- self$options$utility_algorithm
            
            # Simplified utility calculation
            # In practice, this would use validated algorithms like SF-6D, EQ-5D tariffs
            
            utilities <- list()
            
            if (algorithm == "sf6d" && !is.null(private$.summary_scores)) {
                # Simplified SF-6D calculation (actual algorithm is more complex)
                summary_scores <- private$.summary_scores
                if ("Physical Component Summary" %in% names(summary_scores) && 
                    "Mental Component Summary" %in% names(summary_scores)) {
                    
                    pcs <- summary_scores[["Physical Component Summary"]]
                    mcs <- summary_scores[["Mental Component Summary"]]
                    
                    # Simplified utility mapping (0.3 to 1.0 range)
                    utility <- 0.3 + ((pcs + mcs) / 200) * 0.7
                    utilities[["SF-6D"]] <- pmax(0.3, pmin(1.0, utility))
                }
            } else if (algorithm %in% c("eq5d_uk", "eq5d_us")) {
                # Would implement EQ-5D tariffs here
                # For now, create placeholder
                if (length(domain_scores) > 0) {
                    overall_score <- rowMeans(do.call(cbind, domain_scores), na.rm = TRUE)
                    utility <- 0.2 + (overall_score / 100) * 0.8  # Map to 0.2-1.0
                    utilities[[paste0("EQ-5D-", toupper(substr(algorithm, 6, 7)))]] <- utility
                }
            }
            
            private$.health_utilities <- utilities
        },
        
        .qualityControlAssessment = function() {
            domain_data <- private$.domain_data
            
            quality_indicators <- list()
            
            # Response completeness
            for (domain_name in names(domain_data)) {
                domain_items <- domain_data[[domain_name]]
                missing_by_item <- colSums(is.na(domain_items))
                missing_by_case <- rowSums(is.na(domain_items))
                
                quality_indicators[[paste0(domain_name, "_completeness")]] <- list(
                    domain = domain_name,
                    n_items = ncol(domain_items),
                    n_cases = nrow(domain_items),
                    avg_missing_per_item = mean(missing_by_item),
                    complete_cases = sum(complete.cases(domain_items)),
                    completeness_rate = sum(complete.cases(domain_items)) / nrow(domain_items)
                )
            }
            
            private$.quality_results <- quality_indicators
        },
        
        .clinicalInterpretation = function() {
            domain_scores <- private$.domain_scores
            summary_scores <- private$.summary_scores
            
            interpretation <- list()
            
            # Domain-level interpretation
            for (domain_name in names(domain_scores)) {
                scores <- domain_scores[[domain_name]]
                valid_scores <- scores[!is.na(scores)]
                
                if (length(valid_scores) > 0) {
                    mean_score <- mean(valid_scores)
                    
                    # Clinical interpretation based on score ranges
                    if (self$options$scoring_algorithm == "standard") {
                        if (mean_score >= 75) {
                            clinical_level <- "Good"
                        } else if (mean_score >= 50) {
                            clinical_level <- "Fair"
                        } else if (mean_score >= 25) {
                            clinical_level <- "Poor"
                        } else {
                            clinical_level <- "Very Poor"
                        }
                    } else {
                        clinical_level <- "Variable"
                    }
                    
                    interpretation[[domain_name]] <- list(
                        mean_score = mean_score,
                        clinical_level = clinical_level,
                        n_valid = length(valid_scores)
                    )
                }
            }
            
            private$.clinical_interpretation <- interpretation
        },
        
        .ceilingFloorAnalysis = function() {
            domain_scores <- private$.domain_scores
            
            ceiling_floor_results <- list()
            
            for (domain_name in names(domain_scores)) {
                scores <- domain_scores[[domain_name]]
                valid_scores <- scores[!is.na(scores)]
                
                if (length(valid_scores) > 0) {
                    # Define ceiling and floor thresholds
                    if (self$options$scoring_algorithm == "standard") {
                        ceiling_threshold <- 95  # 95% of maximum
                        floor_threshold <- 5     # 5% of minimum
                    } else {
                        # Use percentile-based thresholds
                        score_range <- range(valid_scores)
                        ceiling_threshold <- score_range[2] * 0.95
                        floor_threshold <- score_range[1] + (score_range[2] - score_range[1]) * 0.05
                    }
                    
                    ceiling_effect_n <- sum(valid_scores >= ceiling_threshold)
                    floor_effect_n <- sum(valid_scores <= floor_threshold)
                    
                    ceiling_floor_results[[domain_name]] <- list(
                        domain = domain_name,
                        ceiling_effect_n = ceiling_effect_n,
                        ceiling_effect_percent = ceiling_effect_n / length(valid_scores),
                        floor_effect_n = floor_effect_n,
                        floor_effect_percent = floor_effect_n / length(valid_scores)
                    )
                }
            }
            
            private$.ceiling_floor_results <- ceiling_floor_results
        },
        
        .domainCorrelations = function() {
            domain_scores <- private$.domain_scores
            
            if (length(domain_scores) < 2) return()
            
            # Create matrix of domain scores
            score_matrix <- do.call(cbind, domain_scores)
            
            # Calculate correlation matrix
            cor_matrix <- cor(score_matrix, use = "pairwise.complete.obs")
            
            # Extract upper triangle correlations
            cor_results <- list()
            domain_names <- names(domain_scores)
            
            for (i in 1:(length(domain_names)-1)) {
                for (j in (i+1):length(domain_names)) {
                    cor_value <- cor_matrix[i, j]
                    
                    # Interpret correlation strength
                    if (abs(cor_value) >= 0.7) {
                        strength <- "Strong"
                    } else if (abs(cor_value) >= 0.5) {
                        strength <- "Moderate"
                    } else if (abs(cor_value) >= 0.3) {
                        strength <- "Weak"
                    } else {
                        strength <- "Very Weak"
                    }
                    
                    cor_results[[paste(domain_names[i], domain_names[j], sep = "_")]] <- list(
                        domain1 = domain_names[i],
                        domain2 = domain_names[j],
                        correlation = cor_value,
                        strength = strength
                    )
                }
            }
            
            private$.correlation_results <- cor_results
        },
        
        .groupComparisons = function() {
            if (is.null(self$options$group_var)) return()
            
            data <- self$data
            group_var <- self$options$group_var
            domain_scores <- private$.domain_scores
            test_method <- self$options$statistical_test
            
            group_results <- list()
            
            for (domain_name in names(domain_scores)) {
                scores <- domain_scores[[domain_name]]
                groups <- data[[group_var]]
                
                # Remove missing scores and corresponding groups
                valid_indices <- !is.na(scores)
                clean_scores <- scores[valid_indices]
                clean_groups <- groups[valid_indices]
                
                if (length(unique(clean_groups)) >= 2) {
                    # Perform statistical test
                    if (test_method == "t_test" && length(unique(clean_groups)) == 2) {
                        test_result <- t.test(clean_scores ~ clean_groups)
                        
                        # Calculate effect size (Cohen's d)
                        group_levels <- levels(as.factor(clean_groups))
                        group1_scores <- clean_scores[clean_groups == group_levels[1]]
                        group2_scores <- clean_scores[clean_groups == group_levels[2]]
                        
                        pooled_sd <- sqrt(((length(group1_scores) - 1) * var(group1_scores, na.rm = TRUE) +
                                          (length(group2_scores) - 1) * var(group2_scores, na.rm = TRUE)) /
                                         (length(group1_scores) + length(group2_scores) - 2))
                        
                        effect_size <- (mean(group1_scores) - mean(group2_scores)) / pooled_sd
                        
                        group_results[[domain_name]] <- list(
                            domain = domain_name,
                            test = "t-test",
                            group1_mean = mean(group1_scores),
                            group2_mean = mean(group2_scores),
                            mean_difference = test_result$estimate[1] - test_result$estimate[2],
                            p_value = test_result$p.value,
                            effect_size = effect_size,
                            ci_lower = test_result$conf.int[1],
                            ci_upper = test_result$conf.int[2]
                        )
                    } else if (test_method == "wilcoxon" && length(unique(clean_groups)) == 2) {
                        test_result <- wilcox.test(clean_scores ~ clean_groups)
                        
                        group_results[[domain_name]] <- list(
                            domain = domain_name,
                            test = "Wilcoxon",
                            p_value = test_result$p.value
                        )
                    } else if (test_method == "anova") {
                        test_result <- aov(clean_scores ~ clean_groups)
                        summary_result <- summary(test_result)
                        
                        group_results[[domain_name]] <- list(
                            domain = domain_name,
                            test = "ANOVA",
                            f_statistic = summary_result[[1]]$`F value`[1],
                            p_value = summary_result[[1]]$`Pr(>F)`[1]
                        )
                    }
                }
            }
            
            private$.group_results <- group_results
        },
        
        .longitudinalAnalysis = function() {
            if (is.null(self$options$time_var)) return()
            
            data <- self$data
            time_var <- self$options$time_var
            patient_id <- self$options$patient_id
            domain_scores <- private$.domain_scores
            
            longitudinal_results <- list()
            
            for (domain_name in names(domain_scores)) {
                scores <- domain_scores[[domain_name]]
                
                # Create longitudinal dataset for this domain
                long_data <- data.frame(
                    patient_id = if(!is.null(patient_id)) data[[patient_id]] else seq_len(nrow(data)),
                    time = data[[time_var]],
                    score = scores,
                    stringsAsFactors = FALSE
                )
                
                # Remove missing scores
                long_data <- long_data[!is.na(long_data$score), ]
                
                if (nrow(long_data) > 0) {
                    # Calculate descriptive statistics by time point
                    time_summary <- aggregate(score ~ time, data = long_data, 
                                            FUN = function(x) c(n = length(x), 
                                                               mean = mean(x, na.rm = TRUE),
                                                               sd = sd(x, na.rm = TRUE)))
                    
                    longitudinal_results[[domain_name]] <- list(
                        domain = domain_name,
                        data = long_data,
                        time_summary = time_summary
                    )
                }
            }
            
            private$.longitudinal_results <- longitudinal_results
        },
        
        .populateTables = function() {
            # QoL overview
            n_domains <- length(private$.domain_data)
            n_cases <- nrow(self$data)
            
            overview_data <- data.frame(
                characteristic = c("QoL Instrument", "Scoring Algorithm", "Reference Population", 
                                 "Number of Domains", "Number of Cases"),
                value = c(
                    self$options$instrument_type,
                    self$options$scoring_algorithm,
                    self$options$reference_population,
                    n_domains,
                    n_cases
                ),
                stringsAsFactors = FALSE
            )
            
            self$results$qol_overview$setData(overview_data)
            
            # Domain scores summary
            if (!is.null(private$.domain_scores)) {
                domain_scores <- private$.domain_scores
                
                domain_summary <- data.frame(
                    domain = character(0),
                    n_valid = integer(0),
                    mean = numeric(0),
                    sd = numeric(0),
                    median = numeric(0),
                    q25 = numeric(0),
                    q75 = numeric(0),
                    min = numeric(0),
                    max = numeric(0),
                    missing_percent = numeric(0),
                    stringsAsFactors = FALSE
                )
                
                for (domain_name in names(domain_scores)) {
                    scores <- domain_scores[[domain_name]]
                    valid_scores <- scores[!is.na(scores)]
                    missing_percent <- sum(is.na(scores)) / length(scores)
                    
                    if (length(valid_scores) > 0) {
                        domain_row <- data.frame(
                            domain = domain_name,
                            n_valid = length(valid_scores),
                            mean = mean(valid_scores),
                            sd = sd(valid_scores),
                            median = median(valid_scores),
                            q25 = quantile(valid_scores, 0.25),
                            q75 = quantile(valid_scores, 0.75),
                            min = min(valid_scores),
                            max = max(valid_scores),
                            missing_percent = missing_percent,
                            stringsAsFactors = FALSE
                        )
                        
                        domain_summary <- rbind(domain_summary, domain_row)
                    }
                }
                
                self$results$domain_scores$setData(domain_summary)
            }
            
            # Summary scores
            if (!is.null(private$.summary_scores)) {
                summary_scores <- private$.summary_scores
                
                summary_data <- data.frame(
                    summary_score = character(0),
                    mean = numeric(0),
                    sd = numeric(0),
                    norm_comparison = character(0),
                    clinical_interpretation = character(0),
                    stringsAsFactors = FALSE
                )
                
                for (score_name in names(summary_scores)) {
                    scores <- summary_scores[[score_name]]
                    valid_scores <- scores[!is.na(scores)]
                    
                    if (length(valid_scores) > 0) {
                        mean_score <- mean(valid_scores)
                        
                        # Clinical interpretation
                        if (mean_score >= 70) {
                            clinical_interp <- "Above Average"
                        } else if (mean_score >= 50) {
                            clinical_interp <- "Average"
                        } else if (mean_score >= 30) {
                            clinical_interp <- "Below Average"
                        } else {
                            clinical_interp <- "Well Below Average"
                        }
                        
                        summary_row <- data.frame(
                            summary_score = score_name,
                            mean = mean_score,
                            sd = sd(valid_scores),
                            norm_comparison = "Population norm comparison would go here",
                            clinical_interpretation = clinical_interp,
                            stringsAsFactors = FALSE
                        )
                        
                        summary_data <- rbind(summary_data, summary_row)
                    }
                }
                
                self$results$summary_scores$setData(summary_data)
            }
            
            # Ceiling/floor effects
            if (!is.null(private$.ceiling_floor_results) && self$options$ceiling_floor_analysis) {
                ceiling_floor_data <- data.frame(
                    domain = character(0),
                    floor_effect_n = integer(0),
                    floor_effect_percent = numeric(0),
                    ceiling_effect_n = integer(0),
                    ceiling_effect_percent = numeric(0),
                    interpretation = character(0),
                    stringsAsFactors = FALSE
                )
                
                for (domain_name in names(private$.ceiling_floor_results)) {
                    result <- private$.ceiling_floor_results[[domain_name]]
                    
                    # Interpretation
                    if (result$ceiling_effect_percent > 0.15 || result$floor_effect_percent > 0.15) {
                        interpretation <- "Significant ceiling/floor effects present"
                    } else {
                        interpretation <- "Acceptable ceiling/floor effects"
                    }
                    
                    ceiling_floor_row <- data.frame(
                        domain = result$domain,
                        floor_effect_n = result$floor_effect_n,
                        floor_effect_percent = result$floor_effect_percent,
                        ceiling_effect_n = result$ceiling_effect_n,
                        ceiling_effect_percent = result$ceiling_effect_percent,
                        interpretation = interpretation,
                        stringsAsFactors = FALSE
                    )
                    
                    ceiling_floor_data <- rbind(ceiling_floor_data, ceiling_floor_row)
                }
                
                self$results$ceiling_floor_effects$setData(ceiling_floor_data)
            }
            
            # Group comparisons
            if (!is.null(private$.group_results) && self$options$group_comparisons) {
                group_data <- data.frame(
                    domain = character(0),
                    group1_mean = numeric(0),
                    group2_mean = numeric(0),
                    mean_difference = numeric(0),
                    ci_lower = numeric(0),
                    ci_upper = numeric(0),
                    p_value = numeric(0),
                    adjusted_p = numeric(0),
                    effect_size = numeric(0),
                    exceeds_mid = character(0),
                    stringsAsFactors = FALSE
                )
                
                for (domain_name in names(private$.group_results)) {
                    result <- private$.group_results[[domain_name]]
                    
                    if (result$test == "t-test") {
                        group_row <- data.frame(
                            domain = result$domain,
                            group1_mean = result$group1_mean,
                            group2_mean = result$group2_mean,
                            mean_difference = result$mean_difference,
                            ci_lower = result$ci_lower,
                            ci_upper = result$ci_upper,
                            p_value = result$p_value,
                            adjusted_p = NA,  # Would apply multiple testing correction
                            effect_size = result$effect_size,
                            exceeds_mid = ifelse(abs(result$mean_difference) > 5, "Yes", "No"),
                            stringsAsFactors = FALSE
                        )
                        
                        group_data <- rbind(group_data, group_row)
                    }
                }
                
                if (nrow(group_data) > 0) {
                    self$results$group_comparison_domains$setData(group_data)
                }
            }
        },
        
        .plot_domain_scores = function(image, ggtheme, theme, ...) {
            if (is.null(private$.domain_scores)) {
                return()
            }
            
            domain_scores <- private$.domain_scores
            
            # Convert to long format for plotting
            plot_data <- data.frame()
            for (domain_name in names(domain_scores)) {
                scores <- domain_scores[[domain_name]]
                valid_scores <- scores[!is.na(scores)]
                
                if (length(valid_scores) > 0) {
                    domain_data <- data.frame(
                        Domain = domain_name,
                        Score = valid_scores
                    )
                    plot_data <- rbind(plot_data, domain_data)
                }
            }
            
            if (nrow(plot_data) == 0) return()
            
            p <- ggplot(plot_data, aes(x = Domain, y = Score)) +
                geom_boxplot(fill = "lightblue", alpha = 0.7) +
                geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
                labs(
                    title = "QoL Domain Score Distributions",
                    x = "QoL Domains",
                    y = "Domain Score"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title = element_text(size = 12),
                    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                    axis.text.y = element_text(size = 10)
                )
            
            print(p)
            TRUE
        },
        
        .plot_qol_radar = function(image, ggtheme, theme, ...) {
            if (is.null(private$.domain_scores)) {
                return()
            }
            
            domain_scores <- private$.domain_scores
            
            # Calculate mean scores for radar plot
            mean_scores <- sapply(domain_scores, function(x) mean(x, na.rm = TRUE))
            mean_scores <- mean_scores[!is.na(mean_scores)]
            
            if (length(mean_scores) == 0) return()
            
            # Create radar plot data
            radar_data <- data.frame(
                Domain = names(mean_scores),
                Score = as.numeric(mean_scores),
                stringsAsFactors = FALSE
            )
            
            # Simple circular plot (simplified radar chart)
            p <- ggplot(radar_data, aes(x = Domain, y = Score)) +
                geom_col(fill = "steelblue", alpha = 0.7) +
                coord_polar(theta = "x") +
                ylim(0, 100) +
                labs(
                    title = "QoL Domain Profile (Radar Plot)",
                    y = "Domain Score"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title.x = element_blank(),
                    axis.text.x = element_text(size = 10),
                    axis.text.y = element_text(size = 8)
                )
            
            print(p)
            TRUE
        }
    )
)