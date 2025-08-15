#' @title Outbreak Analysis & Epidemiological Investigation
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats chisq.test fisher.test prop.test binom.test t.test wilcox.test
#' @export


outbreakanalysisClass <- R6::R6Class(
    "outbreakanalysisClass",
    inherit = outbreakanalysisBase,
    private = list(
        .tabular_data = NULL,
        .case_data = NULL,
        .control_data = NULL,
        .descriptive_results = NULL,
        .risk_factor_results = NULL,
        .temporal_results = NULL,
        .spatial_results = NULL,
        .epidemic_curve_data = NULL,
        .attack_rates = NULL,
        .data_quality_results = NULL,

        .init = function() {
            # Check required variables for tabular data analysis
            if (is.null(self$options$case_status)) {
                self$results$outbreak_overview$setNote("note", "Case status variable is required for outbreak analysis")
                return()
            }

            # Set table titles
            self$results$outbreak_overview$setTitle("Outbreak Investigation Overview")
            self$results$descriptive_summary$setTitle("Descriptive Analysis of Tabular Outbreak Data")
            self$results$risk_factor_analysis$setTitle("Risk Factor Analysis from Case-Control Data")
        },

        .run = function() {
            # Check required variables
            if (is.null(self$options$case_status)) {
                return()
            }

            # Process tabular outbreak data
            private$.processTabularData()

            # Descriptive analysis of tabular data
            private$.descriptiveAnalysis()

            # Attack rate analysis from tabular data
            if (self$options$attack_rate_analysis) {
                private$.calculateAttackRates()
            }

            # Risk factor analysis from case-control tabular data
            if (self$options$risk_factor_analysis && length(self$options$exposure_vars) > 0) {
                private$.riskFactorAnalysis()
            }

            # Temporal analysis using onset dates from tabular data
            if (self$options$temporal_analysis && !is.null(self$options$date_onset)) {
                private$.temporalAnalysis()
            }

            # Spatial analysis using location data from tabular data
            if (self$options$spatial_analysis && !is.null(self$options$location_var)) {
                private$.spatialAnalysis()
            }

            # Dose-response analysis for quantitative exposures in tabular data
            if (self$options$dose_response_analysis) {
                private$.doseResponseAnalysis()
            }

            # Data quality assessment for tabular data
            if (self$options$data_quality_assessment) {
                private$.dataQualityAssessment()
            }

            # Update result tables with tabular analysis results
            private$.populateTables()
        },

        .processTabularData = function() {
            # Extract and process tabular outbreak data
            data <- self$data
            case_status_var <- self$options$case_status

            # Ensure case status is properly coded as factor
            case_status <- as.factor(data[[case_status_var]])

            # Identify case and control groups from tabular data
            case_levels <- levels(case_status)
            if (length(case_levels) != 2) {
                self$results$outbreak_overview$setNote("error",
                    "Case status variable must have exactly 2 levels (case/control)")
                return()
            }

            # Assume first level is control, second is case (or detect automatically)
            # This is common in epidemiological tabular data coding
            case_level <- case_levels[2]  # Usually "case" or "1"
            control_level <- case_levels[1]  # Usually "control" or "0"

            # Split tabular data into case and control datasets
            private$.case_data <- data[case_status == case_level, ]
            private$.control_data <- data[case_status == control_level, ]
            private$.tabular_data <- data.frame(
                case_status = case_status,
                data[, !names(data) %in% case_status_var, drop = FALSE],
                stringsAsFactors = FALSE
            )
        },

        .descriptiveAnalysis = function() {
            if (is.null(private$.tabular_data)) return()

            data <- private$.tabular_data
            case_status <- data$case_status

            # Analyze all variables in tabular dataset
            all_vars <- names(data)[!names(data) %in% "case_status"]
            descriptive_results <- list()

            # Basic outbreak statistics from tabular data
            total_n <- nrow(data)
            cases_n <- sum(case_status == levels(case_status)[2])
            controls_n <- sum(case_status == levels(case_status)[1])
            overall_attack_rate <- cases_n / total_n

            # Overall statistics
            descriptive_results[["Overall"]] <- list(
                variable = "Overall Population",
                cases_n = cases_n,
                cases_percent = cases_n / total_n,
                non_cases_n = controls_n,
                non_cases_percent = controls_n / total_n,
                total_n = total_n,
                attack_rate = overall_attack_rate,
                attack_rate_ci = private$.calculateCI(cases_n, total_n)
            )

            # Analyze demographic and exposure variables from tabular data
            if (!is.null(self$options$age_var) && self$options$age_var %in% names(data)) {
                age_var <- data[[self$options$age_var]]
                if (is.numeric(age_var)) {
                    # Categorize age for tabular analysis
                    age_categories <- cut(age_var,
                                        breaks = c(0, 18, 35, 50, 65, 100),
                                        labels = c("<18", "18-34", "35-49", "50-64", "65+"),
                                        include.lowest = TRUE)
                    data$age_category <- age_categories

                    # Analyze age categories in tabular format
                    for (age_cat in levels(age_categories)) {
                        subset_data <- data[age_categories == age_cat & !is.na(age_categories), ]
                        if (nrow(subset_data) > 0) {
                            cat_cases <- sum(subset_data$case_status == levels(case_status)[2])
                            cat_total <- nrow(subset_data)

                            descriptive_results[[paste("Age", age_cat)]] <- list(
                                variable = paste("Age", age_cat),
                                cases_n = cat_cases,
                                cases_percent = cat_cases / cases_n,
                                non_cases_n = cat_total - cat_cases,
                                non_cases_percent = (cat_total - cat_cases) / controls_n,
                                total_n = cat_total,
                                attack_rate = cat_cases / cat_total,
                                attack_rate_ci = private$.calculateCI(cat_cases, cat_total)
                            )
                        }
                    }
                }
            }

            # Analyze sex variable from tabular data
            if (!is.null(self$options$sex_var) && self$options$sex_var %in% names(data)) {
                sex_var <- as.factor(data[[self$options$sex_var]])
                for (sex_level in levels(sex_var)) {
                    subset_data <- data[sex_var == sex_level & !is.na(sex_var), ]
                    if (nrow(subset_data) > 0) {
                        sex_cases <- sum(subset_data$case_status == levels(case_status)[2])
                        sex_total <- nrow(subset_data)

                        descriptive_results[[paste("Sex", sex_level)]] <- list(
                            variable = paste("Sex:", sex_level),
                            cases_n = sex_cases,
                            cases_percent = sex_cases / cases_n,
                            non_cases_n = sex_total - sex_cases,
                            non_cases_percent = (sex_total - sex_cases) / controls_n,
                            total_n = sex_total,
                            attack_rate = sex_cases / sex_total,
                            attack_rate_ci = private$.calculateCI(sex_cases, sex_total)
                        )
                    }
                }
            }

            private$.descriptive_results <- descriptive_results
        },

        .calculateAttackRates = function() {
            if (is.null(private$.tabular_data)) return()

            data <- private$.tabular_data
            case_status <- data$case_status
            attack_rates <- list()

            # Calculate attack rates for exposure variables in tabular data
            if (length(self$options$exposure_vars) > 0) {
                for (exposure_var in self$options$exposure_vars) {
                    if (exposure_var %in% names(data)) {
                        exposure_data <- data[[exposure_var]]

                        if (is.factor(exposure_data) || is.character(exposure_data)) {
                            exposure_factor <- as.factor(exposure_data)

                            for (level in levels(exposure_factor)) {
                                exposed_subset <- data[exposure_factor == level & !is.na(exposure_factor), ]
                                if (nrow(exposed_subset) > 0) {
                                    exposed_cases <- sum(exposed_subset$case_status == levels(case_status)[2])
                                    exposed_total <- nrow(exposed_subset)
                                    attack_rate <- exposed_cases / exposed_total

                                    attack_rates[[paste(exposure_var, level, sep = "_")]] <- list(
                                        exposure = paste(exposure_var, "=", level),
                                        cases = exposed_cases,
                                        total = exposed_total,
                                        attack_rate = attack_rate,
                                        ci = private$.calculateCI(exposed_cases, exposed_total)
                                    )
                                }
                            }
                        } else if (is.numeric(exposure_data)) {
                            # For continuous exposures, create categories
                            exposure_tertiles <- cut(exposure_data,
                                                   breaks = quantile(exposure_data, c(0, 0.33, 0.67, 1), na.rm = TRUE),
                                                   labels = c("Low", "Medium", "High"),
                                                   include.lowest = TRUE)

                            for (tertile in levels(exposure_tertiles)) {
                                tertile_subset <- data[exposure_tertiles == tertile & !is.na(exposure_tertiles), ]
                                if (nrow(tertile_subset) > 0) {
                                    tertile_cases <- sum(tertile_subset$case_status == levels(case_status)[2])
                                    tertile_total <- nrow(tertile_subset)
                                    attack_rate <- tertile_cases / tertile_total

                                    attack_rates[[paste(exposure_var, tertile, sep = "_")]] <- list(
                                        exposure = paste(exposure_var, "=", tertile),
                                        cases = tertile_cases,
                                        total = tertile_total,
                                        attack_rate = attack_rate,
                                        ci = private$.calculateCI(tertile_cases, tertile_total)
                                    )
                                }
                            }
                        }
                    }
                }
            }

            private$.attack_rates <- attack_rates
        },

        .riskFactorAnalysis = function() {
            if (is.null(private$.tabular_data)) return()

            data <- private$.tabular_data
            case_status <- data$case_status
            exposure_vars <- self$options$exposure_vars

            risk_factor_results <- list()

            # Analyze each exposure variable from tabular data
            for (exposure_var in exposure_vars) {
                if (exposure_var %in% names(data)) {
                    exposure_data <- data[[exposure_var]]

                    # Handle binary/categorical exposures in tabular data
                    if (is.factor(exposure_data) || is.character(exposure_data)) {
                        exposure_factor <- as.factor(exposure_data)

                        if (length(levels(exposure_factor)) == 2) {
                            # Binary exposure - standard case-control analysis
                            exposed_level <- levels(exposure_factor)[2]
                            unexposed_level <- levels(exposure_factor)[1]

                            # Create 2x2 contingency table from tabular data
                            exposed_cases <- sum(case_status == levels(case_status)[2] &
                                               exposure_factor == exposed_level, na.rm = TRUE)
                            exposed_total <- sum(exposure_factor == exposed_level, na.rm = TRUE)
                            unexposed_cases <- sum(case_status == levels(case_status)[2] &
                                                 exposure_factor == unexposed_level, na.rm = TRUE)
                            unexposed_total <- sum(exposure_factor == unexposed_level, na.rm = TRUE)

                            exposed_non_cases <- exposed_total - exposed_cases
                            unexposed_non_cases <- unexposed_total - unexposed_cases

                            # Calculate epidemiological measures
                            attack_rate_exposed <- exposed_cases / exposed_total
                            attack_rate_unexposed <- unexposed_cases / unexposed_total

                            # Relative Risk calculation
                            relative_risk <- attack_rate_exposed / attack_rate_unexposed
                            rr_ci <- private$.calculateRRCI(exposed_cases, exposed_total,
                                                           unexposed_cases, unexposed_total)

                            # Odds Ratio calculation
                            odds_ratio <- (exposed_cases * unexposed_non_cases) /
                                         (exposed_non_cases * unexposed_cases)
                            or_ci <- private$.calculateORCI(exposed_cases, exposed_non_cases,
                                                           unexposed_cases, unexposed_non_cases)

                            # Statistical test
                            if (self$options$statistical_tests %in% c("chi_square", "all_tests")) {
                                if (min(c(exposed_cases, exposed_non_cases, unexposed_cases, unexposed_non_cases)) >= 5) {
                                    test_result <- chisq.test(matrix(c(exposed_cases, exposed_non_cases,
                                                                     unexposed_cases, unexposed_non_cases),
                                                                   nrow = 2))
                                    p_value <- test_result$p.value
                                } else {
                                    test_result <- fisher.test(matrix(c(exposed_cases, exposed_non_cases,
                                                                      unexposed_cases, unexposed_non_cases),
                                                                    nrow = 2))
                                    p_value <- test_result$p.value
                                }
                            } else {
                                test_result <- fisher.test(matrix(c(exposed_cases, exposed_non_cases,
                                                                  unexposed_cases, unexposed_non_cases),
                                                                nrow = 2))
                                p_value <- test_result$p.value
                            }

                            risk_factor_results[[exposure_var]] <- list(
                                exposure = exposure_var,
                                exposed_cases = exposed_cases,
                                exposed_total = exposed_total,
                                unexposed_cases = unexposed_cases,
                                unexposed_total = unexposed_total,
                                attack_rate_exposed = attack_rate_exposed,
                                attack_rate_unexposed = attack_rate_unexposed,
                                relative_risk = relative_risk,
                                rr_ci_lower = rr_ci$lower,
                                rr_ci_upper = rr_ci$upper,
                                odds_ratio = odds_ratio,
                                or_ci_lower = or_ci$lower,
                                or_ci_upper = or_ci$upper,
                                p_value = p_value
                            )
                        }
                    } else if (is.numeric(exposure_data)) {
                        # For continuous exposures, compare means between cases and controls
                        case_exposure <- exposure_data[case_status == levels(case_status)[2]]
                        control_exposure <- exposure_data[case_status == levels(case_status)[1]]

                        case_exposure <- case_exposure[!is.na(case_exposure)]
                        control_exposure <- control_exposure[!is.na(control_exposure)]

                        if (length(case_exposure) > 0 && length(control_exposure) > 0) {
                            # t-test for continuous exposure
                            t_result <- t.test(case_exposure, control_exposure)

                            # Convert to odds ratio by tertiles for interpretability
                            exposure_tertiles <- cut(exposure_data,
                                                   breaks = quantile(exposure_data, c(0, 0.67, 1), na.rm = TRUE),
                                                   labels = c("Low", "High"),
                                                   include.lowest = TRUE)

                            high_cases <- sum(case_status == levels(case_status)[2] &
                                            exposure_tertiles == "High", na.rm = TRUE)
                            high_total <- sum(exposure_tertiles == "High", na.rm = TRUE)
                            low_cases <- sum(case_status == levels(case_status)[2] &
                                           exposure_tertiles == "Low", na.rm = TRUE)
                            low_total <- sum(exposure_tertiles == "Low", na.rm = TRUE)

                            if (high_total > 0 && low_total > 0) {
                                high_non_cases <- high_total - high_cases
                                low_non_cases <- low_total - low_cases

                                attack_rate_high <- high_cases / high_total
                                attack_rate_low <- low_cases / low_total
                                relative_risk <- attack_rate_high / attack_rate_low
                                odds_ratio <- (high_cases * low_non_cases) / (high_non_cases * low_cases)

                                rr_ci <- private$.calculateRRCI(high_cases, high_total, low_cases, low_total)
                                or_ci <- private$.calculateORCI(high_cases, high_non_cases, low_cases, low_non_cases)

                                risk_factor_results[[paste0(exposure_var, "_continuous")]] <- list(
                                    exposure = paste0(exposure_var, " (High vs Low)"),
                                    exposed_cases = high_cases,
                                    exposed_total = high_total,
                                    unexposed_cases = low_cases,
                                    unexposed_total = low_total,
                                    attack_rate_exposed = attack_rate_high,
                                    attack_rate_unexposed = attack_rate_low,
                                    relative_risk = relative_risk,
                                    rr_ci_lower = rr_ci$lower,
                                    rr_ci_upper = rr_ci$upper,
                                    odds_ratio = odds_ratio,
                                    or_ci_lower = or_ci$lower,
                                    or_ci_upper = or_ci$upper,
                                    p_value = t_result$p.value
                                )
                            }
                        }
                    }
                }
            }

            # Apply multiple testing correction
            if (length(risk_factor_results) > 1) {
                p_values <- sapply(risk_factor_results, function(x) x$p_value)
                method <- self$options$multiple_testing_correction

                if (method == "bonferroni") {
                    adjusted_p <- p.adjust(p_values, method = "bonferroni")
                } else if (method == "holm") {
                    adjusted_p <- p.adjust(p_values, method = "holm")
                } else if (method == "fdr") {
                    adjusted_p <- p.adjust(p_values, method = "fdr")
                } else {
                    adjusted_p <- p_values
                }

                for (i in seq_along(risk_factor_results)) {
                    risk_factor_results[[i]]$adjusted_p <- adjusted_p[i]
                }
            }

            private$.risk_factor_results <- risk_factor_results
        },

        .temporalAnalysis = function() {
            if (is.null(self$options$date_onset)) return()

            data <- private$.tabular_data
            onset_dates <- data[[self$options$date_onset]]
            case_status <- data$case_status

            # Extract case onset dates from tabular data
            case_dates <- onset_dates[case_status == levels(case_status)[2]]
            case_dates <- case_dates[!is.na(case_dates)]

            if (length(case_dates) == 0) return()

            # Convert to Date if not already
            if (is.numeric(case_dates)) {
                # Assume numeric dates are days since some reference
                case_dates <- as.Date(case_dates, origin = "1970-01-01")
            } else if (is.character(case_dates)) {
                case_dates <- as.Date(case_dates)
            }

            # Create epidemic curve data from tabular onset dates
            time_unit <- self$options$epidemic_curve_unit
            if (time_unit == "auto") {
                date_range <- max(case_dates) - min(case_dates)
                if (date_range <= 30) {
                    time_unit <- "day"
                } else if (date_range <= 180) {
                    time_unit <- "week"
                } else {
                    time_unit <- "month"
                }
            }

            # Aggregate cases by time period
            if (time_unit == "day") {
                time_periods <- seq(from = min(case_dates), to = max(case_dates), by = "day")
                case_counts <- table(case_dates)
            } else if (time_unit == "week") {
                case_weeks <- format(case_dates, "%Y-%W")
                case_counts <- table(case_weeks)
                time_periods <- names(case_counts)
            } else if (time_unit == "month") {
                case_months <- format(case_dates, "%Y-%m")
                case_counts <- table(case_months)
                time_periods <- names(case_counts)
            }

            # Calculate epidemic curve statistics
            epidemic_stats <- list(
                first_case = min(case_dates),
                last_case = max(case_dates),
                outbreak_duration = as.numeric(max(case_dates) - min(case_dates)),
                peak_date = names(case_counts)[which.max(case_counts)],
                peak_cases = max(case_counts),
                total_cases = length(case_dates),
                median_onset = median(case_dates)
            )

            private$.epidemic_curve_data <- list(
                dates = case_dates,
                time_periods = time_periods,
                case_counts = as.numeric(case_counts),
                time_unit = time_unit,
                statistics = epidemic_stats
            )
        },

        .spatialAnalysis = function() {
            if (is.null(self$options$location_var)) return()

            data <- private$.tabular_data
            locations <- data[[self$options$location_var]]
            case_status <- data$case_status

            # Spatial analysis from tabular location data
            location_factor <- as.factor(locations)
            spatial_results <- list()

            for (location in levels(location_factor)) {
                location_subset <- data[location_factor == location & !is.na(location_factor), ]
                if (nrow(location_subset) > 0) {
                    location_cases <- sum(location_subset$case_status == levels(case_status)[2])
                    location_total <- nrow(location_subset)
                    attack_rate <- location_cases / location_total

                    # Calculate relative risk compared to other locations
                    other_cases <- sum(case_status == levels(case_status)[2] &
                                     location_factor != location, na.rm = TRUE)
                    other_total <- sum(location_factor != location, na.rm = TRUE)
                    other_attack_rate <- other_cases / other_total

                    relative_risk <- attack_rate / other_attack_rate
                    rr_ci <- private$.calculateRRCI(location_cases, location_total,
                                                   other_cases, other_total)

                    spatial_results[[location]] <- list(
                        location = location,
                        cases_n = location_cases,
                        population = location_total,
                        attack_rate = attack_rate,
                        relative_risk = relative_risk,
                        rr_ci_lower = rr_ci$lower,
                        rr_ci_upper = rr_ci$upper,
                        clustering_index = ifelse(relative_risk > 1.5, "High",
                                                ifelse(relative_risk < 0.5, "Low", "Average"))
                    )
                }
            }

            private$.spatial_results <- spatial_results
        },

        .doseResponseAnalysis = function() {
            if (length(self$options$exposure_vars) == 0) return()

            data <- private$.tabular_data
            case_status <- data$case_status

            # Dose-response analysis for continuous exposures in tabular data
            for (exposure_var in self$options$exposure_vars) {
                if (exposure_var %in% names(data)) {
                    exposure_data <- data[[exposure_var]]

                    if (is.numeric(exposure_data)) {
                        # Create dose categories (quartiles)
                        dose_quartiles <- cut(exposure_data,
                                            breaks = quantile(exposure_data, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                            labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
                                            include.lowest = TRUE)

                        # Test for trend using logistic regression
                        trend_data <- data.frame(
                            case = as.numeric(case_status == levels(case_status)[2]),
                            dose = as.numeric(dose_quartiles)
                        )
                        trend_data <- trend_data[complete.cases(trend_data), ]

                        if (nrow(trend_data) > 10) {
                            trend_model <- glm(case ~ dose, data = trend_data, family = binomial())
                            trend_p <- summary(trend_model)$coefficients[2, 4]

                            # Store dose-response results (simplified implementation)
                            private$.dose_response_results <- list(
                                exposure = exposure_var,
                                trend_p_value = trend_p,
                                model_significant = trend_p < 0.05
                            )
                        }
                    }
                }
            }
        },

        .dataQualityAssessment = function() {
            if (is.null(private$.tabular_data)) return()

            data <- private$.tabular_data
            quality_results <- list()

            # Assess quality of each variable in tabular data
            for (var_name in names(data)) {
                var_data <- data[[var_name]]
                total_n <- length(var_data)
                missing_n <- sum(is.na(var_data))
                completeness <- (total_n - missing_n) / total_n

                # Check for invalid values (implementation depends on variable type)
                invalid_n <- 0
                if (is.numeric(var_data)) {
                    invalid_n <- sum(is.infinite(var_data) | is.nan(var_data), na.rm = TRUE)
                }

                # Quality grading
                quality_grade <- if (completeness >= 0.95) {
                    "Excellent"
                } else if (completeness >= 0.9) {
                    "Good"
                } else if (completeness >= 0.8) {
                    "Fair"
                } else {
                    "Poor"
                }

                quality_results[[var_name]] <- list(
                    variable = var_name,
                    completeness = completeness,
                    missing_n = missing_n,
                    invalid_n = invalid_n,
                    quality_grade = quality_grade
                )
            }

            private$.data_quality_results <- quality_results
        },

        .calculateCI = function(cases, total, confidence = 0.95) {
            # Calculate confidence interval for proportions
            if (total == 0) return("N/A")

            prop <- cases / total
            alpha <- 1 - confidence
            z <- qnorm(1 - alpha/2)
            se <- sqrt(prop * (1 - prop) / total)

            lower <- max(0, prop - z * se)
            upper <- min(1, prop + z * se)

            paste0("(", round(lower, 3), ", ", round(upper, 3), ")")
        },

        .calculateRRCI = function(exp_cases, exp_total, unexp_cases, unexp_total) {
            # Calculate confidence interval for relative risk
            if (exp_total == 0 || unexp_total == 0 || unexp_cases == 0) {
                return(list(lower = NA, upper = NA))
            }

            rr <- (exp_cases / exp_total) / (unexp_cases / unexp_total)
            log_rr <- log(rr)
            se_log_rr <- sqrt((1/exp_cases) - (1/exp_total) + (1/unexp_cases) - (1/unexp_total))

            lower <- exp(log_rr - 1.96 * se_log_rr)
            upper <- exp(log_rr + 1.96 * se_log_rr)

            list(lower = lower, upper = upper)
        },

        .calculateORCI = function(exp_cases, exp_controls, unexp_cases, unexp_controls) {
            # Calculate confidence interval for odds ratio
            if (exp_cases == 0 || exp_controls == 0 || unexp_cases == 0 || unexp_controls == 0) {
                return(list(lower = NA, upper = NA))
            }

            or <- (exp_cases * unexp_controls) / (exp_controls * unexp_cases)
            log_or <- log(or)
            se_log_or <- sqrt((1/exp_cases) + (1/exp_controls) + (1/unexp_cases) + (1/unexp_controls))

            lower <- exp(log_or - 1.96 * se_log_or)
            upper <- exp(log_or + 1.96 * se_log_or)

            list(lower = lower, upper = upper)
        },

        .populateTables = function() {
            # Populate overview table
            overview_data <- data.frame(
                characteristic = c("Analysis Type", "Outbreak Pattern", "Time Unit",
                                 "Total Individuals", "Cases Identified", "Overall Attack Rate"),
                value = c(
                    self$options$analysis_type,
                    self$options$outbreak_type,
                    if(!is.null(private$.epidemic_curve_data)) private$.epidemic_curve_data$time_unit else "N/A",
                    if(!is.null(private$.tabular_data)) nrow(private$.tabular_data) else "N/A",
                    if(!is.null(private$.case_data)) nrow(private$.case_data) else "N/A",
                    if(!is.null(private$.descriptive_results))
                        paste0(round(private$.descriptive_results[["Overall"]]$attack_rate * 100, 1), "%")
                    else "N/A"
                ),
                stringsAsFactors = FALSE
            )

            self$results$outbreak_overview$setData(overview_data)

            # Populate descriptive summary
            if (!is.null(private$.descriptive_results)) {
                descriptive_data <- data.frame(
                    variable = character(0),
                    cases_n = integer(0),
                    cases_percent = numeric(0),
                    non_cases_n = integer(0),
                    non_cases_percent = numeric(0),
                    total_n = integer(0),
                    attack_rate = numeric(0),
                    attack_rate_ci = character(0),
                    stringsAsFactors = FALSE
                )

                for (result_name in names(private$.descriptive_results)) {
                    result <- private$.descriptive_results[[result_name]]
                    descriptive_row <- data.frame(
                        variable = result$variable,
                        cases_n = result$cases_n,
                        cases_percent = result$cases_percent,
                        non_cases_n = result$non_cases_n,
                        non_cases_percent = result$non_cases_percent,
                        total_n = result$total_n,
                        attack_rate = result$attack_rate,
                        attack_rate_ci = result$attack_rate_ci,
                        stringsAsFactors = FALSE
                    )
                    descriptive_data <- rbind(descriptive_data, descriptive_row)
                }

                self$results$descriptive_summary$setData(descriptive_data)
            }

            # Populate risk factor analysis
            if (!is.null(private$.risk_factor_results) && self$options$risk_factor_analysis) {
                risk_data <- data.frame(
                    exposure = character(0),
                    exposed_cases = integer(0),
                    exposed_total = integer(0),
                    unexposed_cases = integer(0),
                    unexposed_total = integer(0),
                    attack_rate_exposed = numeric(0),
                    attack_rate_unexposed = numeric(0),
                    relative_risk = numeric(0),
                    rr_ci_lower = numeric(0),
                    rr_ci_upper = numeric(0),
                    odds_ratio = numeric(0),
                    or_ci_lower = numeric(0),
                    or_ci_upper = numeric(0),
                    p_value = numeric(0),
                    adjusted_p = numeric(0),
                    stringsAsFactors = FALSE
                )

                for (result_name in names(private$.risk_factor_results)) {
                    result <- private$.risk_factor_results[[result_name]]
                    risk_row <- data.frame(
                        exposure = result$exposure,
                        exposed_cases = result$exposed_cases,
                        exposed_total = result$exposed_total,
                        unexposed_cases = result$unexposed_cases,
                        unexposed_total = result$unexposed_total,
                        attack_rate_exposed = result$attack_rate_exposed,
                        attack_rate_unexposed = result$attack_rate_unexposed,
                        relative_risk = result$relative_risk,
                        rr_ci_lower = result$rr_ci_lower,
                        rr_ci_upper = result$rr_ci_upper,
                        odds_ratio = result$odds_ratio,
                        or_ci_lower = result$or_ci_lower,
                        or_ci_upper = result$or_ci_upper,
                        p_value = result$p_value,
                        adjusted_p = if(!is.null(result$adjusted_p)) result$adjusted_p else result$p_value,
                        stringsAsFactors = FALSE
                    )
                    risk_data <- rbind(risk_data, risk_row)
                }

                self$results$risk_factor_analysis$setData(risk_data)
            }

            # Data quality assessment table
            if (!is.null(private$.data_quality_results) && self$options$data_quality_assessment) {
                quality_data <- data.frame(
                    variable = character(0),
                    completeness = numeric(0),
                    missing_n = integer(0),
                    invalid_n = integer(0),
                    quality_grade = character(0),
                    stringsAsFactors = FALSE
                )

                for (result_name in names(private$.data_quality_results)) {
                    result <- private$.data_quality_results[[result_name]]
                    quality_row <- data.frame(
                        variable = result$variable,
                        completeness = result$completeness,
                        missing_n = result$missing_n,
                        invalid_n = result$invalid_n,
                        quality_grade = result$quality_grade,
                        stringsAsFactors = FALSE
                    )
                    quality_data <- rbind(quality_data, quality_row)
                }

                self$results$data_quality_report$setData(quality_data)
            }
        },

        .plot_epidemic_curve = function(image, ggtheme, theme, ...) {
            if (is.null(private$.epidemic_curve_data) || !self$options$epidemic_curve_plot) {
                return()
            }

            curve_data <- private$.epidemic_curve_data

            # Create epidemic curve from tabular onset date data
            plot_data <- data.frame(
                date = curve_data$dates,
                stringsAsFactors = FALSE
            )

            p <- ggplot(plot_data, aes(x = date)) +
                geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "black") +
                labs(
                    title = "Epidemic Curve - Cases by Date of Onset",
                    subtitle = paste("Outbreak Duration:", curve_data$statistics$outbreak_duration, "days"),
                    x = "Date of Onset",
                    y = "Number of Cases"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.text.x = element_text(angle = 45, hjust = 1)
                )

            print(p)
            TRUE
        },

        .plot_attack_rates = function(image, ggtheme, theme, ...) {
            if (is.null(private$.attack_rates) || !self$options$attack_rate_plot) {
                return()
            }

            # Create attack rate plot from tabular data analysis
            attack_data <- data.frame(
                exposure = character(0),
                attack_rate = numeric(0),
                stringsAsFactors = FALSE
            )

            for (result_name in names(private$.attack_rates)) {
                result <- private$.attack_rates[[result_name]]
                attack_row <- data.frame(
                    exposure = result$exposure,
                    attack_rate = result$attack_rate,
                    stringsAsFactors = FALSE
                )
                attack_data <- rbind(attack_data, attack_row)
            }

            if (nrow(attack_data) == 0) return()

            p <- ggplot(attack_data, aes(x = exposure, y = attack_rate)) +
                geom_col(fill = "coral", alpha = 0.7) +
                labs(
                    title = "Attack Rates by Exposure",
                    x = "Exposure/Risk Factor",
                    y = "Attack Rate"
                ) +
                scale_y_continuous(labels = scales::percent_format()) +
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
