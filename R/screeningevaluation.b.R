#' @title Screening Program Evaluation & Performance Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats chisq.test fisher.test prop.test binom.test
#' @export


screeningevaluationClass <- R6::R6Class(
    "screeningevaluationClass",
    inherit = screeningevaluationBase,
    private = list(
        .screening_data = NULL,
        .diagnostic_accuracy = NULL,
        .coverage_results = NULL,
        .detection_results = NULL,
        .quality_indicators = NULL,
        .age_stratified_results = NULL,
        .cost_effectiveness = NULL,
        
        .init = function() {
            # Check required variables for tabular screening data analysis
            if (is.null(self$options$screening_result) || is.null(self$options$disease_status)) {
                self$results$screening_overview$setNote("note", 
                    "Both screening result and disease status variables are required for screening evaluation")
                return()
            }
            
            # Set table titles
            self$results$screening_overview$setTitle("Screening Program Overview")
            self$results$diagnostic_accuracy_summary$setTitle("Diagnostic Accuracy from Tabular Data")
            self$results$program_coverage$setTitle("Program Coverage Analysis")
        },
        
        .run = function() {
            # Check required variables
            if (is.null(self$options$screening_result) || is.null(self$options$disease_status)) {
                return()
            }
            
            # Process tabular screening data
            private$.processScreeningData()
            
            # Diagnostic accuracy analysis from tabular data
            if (self$options$diagnostic_accuracy) {
                private$.diagnosticAccuracyAnalysis()
            }
            
            # Program coverage analysis
            if (self$options$program_coverage) {
                private$.programCoverageAnalysis()
            }
            
            # Detection rate analysis
            if (self$options$detection_rates) {
                private$.detectionRateAnalysis()
            }
            
            # Age-stratified analysis
            if (self$options$age_stratified_analysis && !is.null(self$options$age_var)) {
                private$.ageStratifiedAnalysis()
            }
            
            # Quality indicators
            if (self$options$quality_indicators) {
                private$.calculateQualityIndicators()
            }
            
            # Cost-effectiveness analysis
            if (self$options$cost_effectiveness) {
                private$.costEffectivenessAnalysis()
            }
            
            # Update tables with results
            private$.populateTables()
        },
        
        .processScreeningData = function() {
            # Process tabular screening dataset
            data <- self$data
            screening_result_var <- self$options$screening_result
            disease_status_var <- self$options$disease_status
            
            # Extract screening and disease status from tabular data
            screening_result <- as.factor(data[[screening_result_var]])
            disease_status <- as.factor(data[[disease_status_var]])
            
            # Validate binary variables for 2x2 table analysis
            if (length(levels(screening_result)) != 2 || length(levels(disease_status)) != 2) {
                self$results$screening_overview$setNote("error", 
                    "Screening result and disease status must each have exactly 2 levels")
                return()
            }
            
            # Create tabular screening dataset with proper coding
            # Assume positive = level 2, negative = level 1 (or detect automatically)
            private$.screening_data <- data.frame(
                screening_positive = screening_result == levels(screening_result)[2],
                disease_present = disease_status == levels(disease_status)[2],
                data[, !names(data) %in% c(screening_result_var, disease_status_var), drop = FALSE],
                stringsAsFactors = FALSE
            )
        },
        
        .diagnosticAccuracyAnalysis = function() {
            if (is.null(private$.screening_data)) return()
            
            data <- private$.screening_data
            
            # Create 2x2 contingency table from tabular data
            # True Positive (TP): screening positive AND disease present
            tp <- sum(data$screening_positive & data$disease_present, na.rm = TRUE)
            # False Positive (FP): screening positive AND disease absent  
            fp <- sum(data$screening_positive & !data$disease_present, na.rm = TRUE)
            # False Negative (FN): screening negative AND disease present
            fn <- sum(!data$screening_positive & data$disease_present, na.rm = TRUE)
            # True Negative (TN): screening negative AND disease absent
            tn <- sum(!data$screening_positive & !data$disease_present, na.rm = TRUE)
            
            # Calculate diagnostic accuracy measures from tabular data
            sensitivity <- tp / (tp + fn)
            specificity <- tn / (tn + fp)
            ppv <- tp / (tp + fp)  # Positive Predictive Value
            npv <- tn / (tn + fn)  # Negative Predictive Value
            accuracy <- (tp + tn) / (tp + fp + fn + tn)
            
            # Likelihood ratios
            lr_positive <- sensitivity / (1 - specificity)
            lr_negative <- (1 - sensitivity) / specificity
            
            # Calculate confidence intervals using exact methods
            sens_ci <- private$.calculateProportionCI(tp, tp + fn)
            spec_ci <- private$.calculateProportionCI(tn, tn + fp)
            ppv_ci <- private$.calculateProportionCI(tp, tp + fp)
            npv_ci <- private$.calculateProportionCI(tn, tn + fn)
            acc_ci <- private$.calculateProportionCI(tp + tn, tp + fp + fn + tn)
            
            # Store diagnostic accuracy results from tabular analysis
            private$.diagnostic_accuracy <- list(
                contingency_table = matrix(c(tp, fp, fn, tn), nrow = 2, 
                                         dimnames = list(c("Test+", "Test-"), c("Disease+", "Disease-"))),
                sensitivity = list(value = sensitivity, ci_lower = sens_ci$lower, ci_upper = sens_ci$upper),
                specificity = list(value = specificity, ci_lower = spec_ci$lower, ci_upper = spec_ci$upper),
                ppv = list(value = ppv, ci_lower = ppv_ci$lower, ci_upper = ppv_ci$upper),
                npv = list(value = npv, ci_lower = npv_ci$lower, ci_upper = npv_ci$upper),
                accuracy = list(value = accuracy, ci_lower = acc_ci$lower, ci_upper = acc_ci$upper),
                lr_positive = lr_positive,
                lr_negative = lr_negative,
                prevalence = (tp + fn) / (tp + fp + fn + tn)
            )
        },
        
        .programCoverageAnalysis = function() {
            if (is.null(private$.screening_data)) return()
            
            data <- private$.screening_data
            
            # Calculate basic coverage metrics from tabular data
            total_eligible <- nrow(data)  # Total individuals in tabular dataset
            total_screened <- sum(!is.na(data$screening_positive))  # Non-missing screening results
            participation_rate <- total_screened / total_eligible
            
            # Coverage by demographic groups (if available in tabular data)
            coverage_results <- list(
                overall = list(
                    coverage_type = "Overall Program Coverage",
                    numerator = total_screened,
                    denominator = total_eligible,
                    percentage = participation_rate,
                    target_percentage = 0.70,  # Typical screening target 70%
                    performance_status = ifelse(participation_rate >= 0.70, "Meets Target", 
                                              ifelse(participation_rate >= 0.50, "Approaching Target", "Below Target"))
                )
            )
            
            # Age-specific coverage (if age variable available)
            if (!is.null(self$options$age_var) && self$options$age_var %in% names(data)) {
                age_var <- data[[self$options$age_var]]
                if (is.numeric(age_var)) {
                    # Create age groups for tabular analysis
                    age_breaks <- as.numeric(unlist(strsplit(self$options$age_group_breaks, ",")))
                    age_groups <- cut(age_var, breaks = c(0, age_breaks, 100), include.lowest = TRUE)
                    
                    for (age_group in levels(age_groups)) {
                        age_subset <- data[age_groups == age_group & !is.na(age_groups), ]
                        if (nrow(age_subset) > 0) {
                            age_screened <- sum(!is.na(age_subset$screening_positive))
                            age_eligible <- nrow(age_subset)
                            age_participation <- age_screened / age_eligible
                            
                            coverage_results[[paste0("age_", age_group)]] <- list(
                                coverage_type = paste("Coverage Age", age_group),
                                numerator = age_screened,
                                denominator = age_eligible,
                                percentage = age_participation,
                                target_percentage = 0.70,
                                performance_status = ifelse(age_participation >= 0.70, "Meets Target", 
                                                          ifelse(age_participation >= 0.50, "Approaching Target", "Below Target"))
                            )
                        }
                    }
                }
            }
            
            private$.coverage_results <- coverage_results
        },
        
        .detectionRateAnalysis = function() {
            if (is.null(private$.screening_data)) return()
            
            data <- private$.screening_data
            
            # Calculate overall detection rate from tabular data
            cases_detected <- sum(data$screening_positive & data$disease_present, na.rm = TRUE)
            total_screened <- sum(!is.na(data$screening_positive))
            overall_detection_rate <- (cases_detected / total_screened) * 1000  # Per 1000 screened
            
            detection_results <- list(
                overall = list(
                    subgroup = "Overall Population",
                    screened_n = total_screened,
                    cases_detected = cases_detected,
                    detection_rate = overall_detection_rate,
                    expected_rate = private$.getExpectedDetectionRate(),
                    ci = private$.calculateRateCI(cases_detected, total_screened, multiplier = 1000)
                )
            )
            
            # Detection rates by age groups (if available)
            if (!is.null(self$options$age_var) && self$options$age_var %in% names(data)) {
                age_var <- data[[self$options$age_var]]
                if (is.numeric(age_var)) {
                    age_breaks <- as.numeric(unlist(strsplit(self$options$age_group_breaks, ",")))
                    age_groups <- cut(age_var, breaks = c(0, age_breaks, 100), include.lowest = TRUE)
                    
                    for (age_group in levels(age_groups)) {
                        age_subset <- data[age_groups == age_group & !is.na(age_groups), ]
                        if (nrow(age_subset) > 0) {
                            age_screened <- sum(!is.na(age_subset$screening_positive))
                            age_detected <- sum(age_subset$screening_positive & age_subset$disease_present, na.rm = TRUE)
                            age_detection_rate <- (age_detected / age_screened) * 1000
                            
                            detection_results[[paste0("age_", age_group)]] <- list(
                                subgroup = paste("Age", age_group),
                                screened_n = age_screened,
                                cases_detected = age_detected,
                                detection_rate = age_detection_rate,
                                expected_rate = private$.getExpectedDetectionRate(age_group),
                                ci = private$.calculateRateCI(age_detected, age_screened, multiplier = 1000)
                            )
                        }
                    }
                }
            }
            
            # Detection rates by sex (if available)
            if (!is.null(self$options$sex_var) && self$options$sex_var %in% names(data)) {
                sex_var <- as.factor(data[[self$options$sex_var]])
                
                for (sex_level in levels(sex_var)) {
                    sex_subset <- data[sex_var == sex_level & !is.na(sex_var), ]
                    if (nrow(sex_subset) > 0) {
                        sex_screened <- sum(!is.na(sex_subset$screening_positive))
                        sex_detected <- sum(sex_subset$screening_positive & sex_subset$disease_present, na.rm = TRUE)
                        sex_detection_rate <- (sex_detected / sex_screened) * 1000
                        
                        detection_results[[paste0("sex_", sex_level)]] <- list(
                            subgroup = paste("Sex:", sex_level),
                            screened_n = sex_screened,
                            cases_detected = sex_detected,
                            detection_rate = sex_detection_rate,
                            expected_rate = private$.getExpectedDetectionRate(),
                            ci = private$.calculateRateCI(sex_detected, sex_screened, multiplier = 1000)
                        )
                    }
                }
            }
            
            private$.detection_results <- detection_results
        },
        
        .ageStratifiedAnalysis = function() {
            if (is.null(private$.screening_data) || is.null(self$options$age_var)) return()
            
            data <- private$.screening_data
            age_var <- data[[self$options$age_var]]
            
            if (!is.numeric(age_var)) return()
            
            # Create age groups for stratified analysis of tabular data
            age_breaks <- as.numeric(unlist(strsplit(self$options$age_group_breaks, ",")))
            age_groups <- cut(age_var, breaks = c(0, age_breaks, 100), include.lowest = TRUE)
            
            age_results <- list()
            
            for (age_group in levels(age_groups)) {
                age_subset <- data[age_groups == age_group & !is.na(age_groups), ]
                
                if (nrow(age_subset) > 0) {
                    # Calculate 2x2 table for this age group
                    tp <- sum(age_subset$screening_positive & age_subset$disease_present, na.rm = TRUE)
                    fp <- sum(age_subset$screening_positive & !age_subset$disease_present, na.rm = TRUE)
                    fn <- sum(!age_subset$screening_positive & age_subset$disease_present, na.rm = TRUE)
                    tn <- sum(!age_subset$screening_positive & !age_subset$disease_present, na.rm = TRUE)
                    
                    if ((tp + fn) > 0 && (tn + fp) > 0) {
                        sensitivity <- tp / (tp + fn)
                        specificity <- tn / (tn + fp)
                        ppv <- tp / (tp + fp)
                        npv <- tn / (tn + fn)
                        detection_rate <- (tp / (tp + fp + fn + tn)) * 1000
                        participation_rate <- sum(!is.na(age_subset$screening_positive)) / nrow(age_subset)
                        
                        age_results[[age_group]] <- list(
                            age_group = as.character(age_group),
                            sensitivity = sensitivity,
                            specificity = specificity,
                            ppv = ppv,
                            npv = npv,
                            detection_rate = detection_rate,
                            participation_rate = participation_rate
                        )
                    }
                }
            }
            
            private$.age_stratified_results <- age_results
        },
        
        .calculateQualityIndicators = function() {
            if (is.null(private$.screening_data)) return()
            
            data <- private$.screening_data
            
            # Calculate standard screening quality indicators from tabular data
            total_screened <- sum(!is.na(data$screening_positive))
            screen_positives <- sum(data$screening_positive, na.rm = TRUE)
            cases_detected <- sum(data$screening_positive & data$disease_present, na.rm = TRUE)
            
            # Recall rate (positive screen rate)
            recall_rate <- screen_positives / total_screened
            
            # Cancer detection rate (if applicable)
            detection_rate_per_1000 <- (cases_detected / total_screened) * 1000
            
            # Positive predictive value
            ppv <- cases_detected / screen_positives
            
            # Define quality indicators with targets based on screening type
            quality_indicators <- list(
                recall_rate = list(
                    indicator = "Recall Rate",
                    observed_value = paste0(round(recall_rate * 100, 1), "%"),
                    target_value = private$.getRecallRateTarget(),
                    performance_level = private$.assessPerformance(recall_rate, private$.getRecallRateTargetNumeric()),
                    recommendation = private$.getRecallRateRecommendation(recall_rate)
                ),
                detection_rate = list(
                    indicator = "Detection Rate (per 1000)",
                    observed_value = round(detection_rate_per_1000, 1),
                    target_value = private$.getDetectionRateTarget(),
                    performance_level = private$.assessPerformance(detection_rate_per_1000, private$.getDetectionRateTargetNumeric()),
                    recommendation = private$.getDetectionRateRecommendation(detection_rate_per_1000)
                ),
                positive_predictive_value = list(
                    indicator = "Positive Predictive Value",
                    observed_value = paste0(round(ppv * 100, 1), "%"),
                    target_value = private$.getPPVTarget(),
                    performance_level = private$.assessPerformance(ppv, private$.getPPVTargetNumeric()),
                    recommendation = private$.getPPVRecommendation(ppv)
                )
            )
            
            private$.quality_indicators <- quality_indicators
        },
        
        .costEffectivenessAnalysis = function() {
            if (!self$options$cost_effectiveness) return()
            if (is.null(private$.screening_data)) return()
            
            data <- private$.screening_data
            
            # Basic cost-effectiveness analysis if cost variables available
            cost_results <- list()
            
            if (!is.null(self$options$screening_cost_var) && self$options$screening_cost_var %in% names(data)) {
                screening_costs <- data[[self$options$screening_cost_var]]
                total_screening_cost <- sum(screening_costs, na.rm = TRUE)
                cases_detected <- sum(data$screening_positive & data$disease_present, na.rm = TRUE)
                
                cost_per_case <- if (cases_detected > 0) total_screening_cost / cases_detected else Inf
                cost_per_person_screened <- mean(screening_costs, na.rm = TRUE)
                
                cost_results <- list(
                    total_screening_cost = total_screening_cost,
                    cost_per_person_screened = cost_per_person_screened,
                    cost_per_case_detected = cost_per_case,
                    cases_detected = cases_detected
                )
            }
            
            private$.cost_effectiveness <- cost_results
        },
        
        # Helper functions for quality indicator targets (simplified)
        .getExpectedDetectionRate = function(age_group = NULL) {
            # Return expected detection rates based on screening type and age
            if (self$options$target_disease == "cancer") {
                return(5.0)  # 5 per 1000 for cancer screening
            } else if (self$options$target_disease == "diabetes") {
                return(15.0)  # 15 per 1000 for diabetes screening
            } else {
                return(10.0)  # Generic target
            }
        },
        
        .getRecallRateTarget = function() {
            if (self$options$target_disease == "cancer") {
                return("5-10%")
            } else {
                return("Variable by test")
            }
        },
        
        .getRecallRateTargetNumeric = function() {
            return(0.075)  # 7.5% as midpoint
        },
        
        .getDetectionRateTarget = function() {
            return(paste0(private$.getExpectedDetectionRate(), " per 1000"))
        },
        
        .getDetectionRateTargetNumeric = function() {
            return(private$.getExpectedDetectionRate())
        },
        
        .getPPVTarget = function() {
            if (self$options$target_disease == "cancer") {
                return("5-10%")
            } else {
                return("Variable by prevalence")
            }
        },
        
        .getPPVTargetNumeric = function() {
            return(0.075)  # 7.5% as midpoint
        },
        
        .assessPerformance = function(observed, target) {
            if (is.infinite(observed) || is.na(observed)) return("Cannot assess")
            
            ratio <- observed / target
            if (ratio >= 0.9 && ratio <= 1.1) {
                return("Meets target")
            } else if (ratio >= 0.8 && ratio <= 1.25) {
                return("Close to target")
            } else {
                return("Outside target range")
            }
        },
        
        .getRecallRateRecommendation = function(rate) {
            if (is.na(rate)) return("Insufficient data")
            if (rate < 0.03) return("Consider test sensitivity")
            if (rate > 0.15) return("Review test specificity")
            return("Within acceptable range")
        },
        
        .getDetectionRateRecommendation = function(rate) {
            if (is.na(rate)) return("Insufficient data")
            if (rate < 2) return("Review target population")
            if (rate > 20) return("Consider overdiagnosis")
            return("Appropriate detection rate")
        },
        
        .getPPVRecommendation = function(ppv) {
            if (is.na(ppv)) return("Insufficient data")
            if (ppv < 0.02) return("Consider test performance")
            if (ppv > 0.20) return("Excellent performance")
            return("Reasonable performance")
        },
        
        .calculateProportionCI = function(successes, total, confidence = 0.95) {
            if (total == 0) return(list(lower = 0, upper = 1))
            
            prop <- successes / total
            alpha <- 1 - confidence
            z <- qnorm(1 - alpha/2)
            se <- sqrt(prop * (1 - prop) / total)
            
            lower <- max(0, prop - z * se)
            upper <- min(1, prop + z * se)
            
            list(lower = lower, upper = upper)
        },
        
        .calculateRateCI = function(cases, population, multiplier = 1000, confidence = 0.95) {
            if (population == 0) return(list(lower = 0, upper = Inf))
            
            rate <- (cases / population) * multiplier
            
            # Poisson approximation for rate confidence interval
            alpha <- 1 - confidence
            lower_case <- qpois(alpha/2, cases)
            upper_case <- qpois(1 - alpha/2, cases)
            
            ci_lower <- (lower_case / population) * multiplier
            ci_upper <- (upper_case / population) * multiplier
            
            list(lower = ci_lower, upper = ci_upper)
        },
        
        .populateTables = function() {
            # Overview table
            overview_data <- data.frame(
                characteristic = c("Screening Program Type", "Target Disease", "Test Type", 
                                 "Total Participants", "Screening Positives", "Cases Detected"),
                value = c(
                    self$options$screening_type,
                    self$options$target_disease,
                    self$options$screening_test_type,
                    if(!is.null(private$.screening_data)) nrow(private$.screening_data) else "N/A",
                    if(!is.null(private$.screening_data)) sum(private$.screening_data$screening_positive, na.rm = TRUE) else "N/A",
                    if(!is.null(private$.screening_data)) 
                        sum(private$.screening_data$screening_positive & private$.screening_data$disease_present, na.rm = TRUE) 
                    else "N/A"
                ),
                stringsAsFactors = FALSE
            )
            
            self$results$screening_overview$setData(overview_data)
            
            # Diagnostic accuracy table
            if (!is.null(private$.diagnostic_accuracy) && self$options$diagnostic_accuracy) {
                accuracy <- private$.diagnostic_accuracy
                
                accuracy_data <- data.frame(
                    measure = c("Sensitivity", "Specificity", "Positive Predictive Value", 
                               "Negative Predictive Value", "Accuracy"),
                    value = c(accuracy$sensitivity$value, accuracy$specificity$value,
                             accuracy$ppv$value, accuracy$npv$value, accuracy$accuracy$value),
                    ci_lower = c(accuracy$sensitivity$ci_lower, accuracy$specificity$ci_lower,
                                accuracy$ppv$ci_lower, accuracy$npv$ci_lower, accuracy$accuracy$ci_lower),
                    ci_upper = c(accuracy$sensitivity$ci_upper, accuracy$specificity$ci_upper,
                                accuracy$ppv$ci_upper, accuracy$npv$ci_upper, accuracy$accuracy$ci_upper),
                    interpretation = c("Proportion of diseased correctly identified",
                                     "Proportion of non-diseased correctly identified",
                                     "Proportion of positives with disease",
                                     "Proportion of negatives without disease",
                                     "Overall diagnostic accuracy"),
                    stringsAsFactors = FALSE
                )
                
                self$results$diagnostic_accuracy_summary$setData(accuracy_data)
            }
            
            # Program coverage table
            if (!is.null(private$.coverage_results) && self$options$program_coverage) {
                coverage_data <- data.frame(
                    coverage_type = character(0),
                    numerator = integer(0),
                    denominator = integer(0),
                    percentage = numeric(0),
                    target_percentage = numeric(0),
                    performance_status = character(0),
                    stringsAsFactors = FALSE
                )
                
                for (result_name in names(private$.coverage_results)) {
                    result <- private$.coverage_results[[result_name]]
                    coverage_row <- data.frame(
                        coverage_type = result$coverage_type,
                        numerator = result$numerator,
                        denominator = result$denominator,
                        percentage = result$percentage,
                        target_percentage = result$target_percentage,
                        performance_status = result$performance_status,
                        stringsAsFactors = FALSE
                    )
                    coverage_data <- rbind(coverage_data, coverage_row)
                }
                
                self$results$program_coverage$setData(coverage_data)
            }
            
            # Detection rates table
            if (!is.null(private$.detection_results) && self$options$detection_rates) {
                detection_data <- data.frame(
                    subgroup = character(0),
                    screened_n = integer(0),
                    cases_detected = integer(0),
                    detection_rate = numeric(0),
                    dr_ci_lower = numeric(0),
                    dr_ci_upper = numeric(0),
                    expected_rate = numeric(0),
                    stringsAsFactors = FALSE
                )
                
                for (result_name in names(private$.detection_results)) {
                    result <- private$.detection_results[[result_name]]
                    detection_row <- data.frame(
                        subgroup = result$subgroup,
                        screened_n = result$screened_n,
                        cases_detected = result$cases_detected,
                        detection_rate = result$detection_rate,
                        dr_ci_lower = result$ci$lower,
                        dr_ci_upper = result$ci$upper,
                        expected_rate = result$expected_rate,
                        stringsAsFactors = FALSE
                    )
                    detection_data <- rbind(detection_data, detection_row)
                }
                
                self$results$detection_rates$setData(detection_data)
            }
            
            # Quality indicators table
            if (!is.null(private$.quality_indicators) && self$options$quality_indicators) {
                quality_data <- data.frame(
                    indicator = character(0),
                    observed_value = character(0),
                    target_value = character(0),
                    performance_level = character(0),
                    recommendation = character(0),
                    stringsAsFactors = FALSE
                )
                
                for (indicator_name in names(private$.quality_indicators)) {
                    indicator <- private$.quality_indicators[[indicator_name]]
                    quality_row <- data.frame(
                        indicator = indicator$indicator,
                        observed_value = indicator$observed_value,
                        target_value = indicator$target_value,
                        performance_level = indicator$performance_level,
                        recommendation = indicator$recommendation,
                        stringsAsFactors = FALSE
                    )
                    quality_data <- rbind(quality_data, quality_row)
                }
                
                self$results$quality_indicators$setData(quality_data)
            }
            
            # Age-stratified results table
            if (!is.null(private$.age_stratified_results) && self$options$age_stratified_analysis) {
                age_data <- data.frame(
                    age_group = character(0),
                    sensitivity = numeric(0),
                    specificity = numeric(0),
                    ppv = numeric(0),
                    npv = numeric(0),
                    detection_rate = numeric(0),
                    participation_rate = numeric(0),
                    stringsAsFactors = FALSE
                )
                
                for (age_group in names(private$.age_stratified_results)) {
                    result <- private$.age_stratified_results[[age_group]]
                    age_row <- data.frame(
                        age_group = result$age_group,
                        sensitivity = result$sensitivity,
                        specificity = result$specificity,
                        ppv = result$ppv,
                        npv = result$npv,
                        detection_rate = result$detection_rate,
                        participation_rate = result$participation_rate,
                        stringsAsFactors = FALSE
                    )
                    age_data <- rbind(age_data, age_row)
                }
                
                self$results$age_stratified_results$setData(age_data)
            }
        },
        
        .plot_diagnostic_performance = function(image, ggtheme, theme, ...) {
            if (is.null(private$.diagnostic_accuracy) || !self$options$performance_plots) {
                return()
            }
            
            accuracy <- private$.diagnostic_accuracy
            
            # Create performance metrics plot from tabular data analysis
            plot_data <- data.frame(
                metric = c("Sensitivity", "Specificity", "PPV", "NPV"),
                value = c(accuracy$sensitivity$value, accuracy$specificity$value,
                         accuracy$ppv$value, accuracy$npv$value),
                lower = c(accuracy$sensitivity$ci_lower, accuracy$specificity$ci_lower,
                         accuracy$ppv$ci_lower, accuracy$npv$ci_lower),
                upper = c(accuracy$sensitivity$ci_upper, accuracy$specificity$ci_upper,
                         accuracy$ppv$ci_upper, accuracy$npv$ci_upper)
            )
            
            p <- ggplot(plot_data, aes(x = metric, y = value)) +
                geom_col(fill = "steelblue", alpha = 0.7) +
                geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
                labs(
                    title = "Diagnostic Performance Metrics",
                    subtitle = "From Tabular Screening Data Analysis",
                    x = "Performance Metric",
                    y = "Value (Proportion)"
                ) +
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.text.x = element_text(angle = 45, hjust = 1)
                )
            
            print(p)
            TRUE
        }
    )
)