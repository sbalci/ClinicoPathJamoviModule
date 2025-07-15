#' @title Treatment Toxicity Profile
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr group_by summarise mutate arrange count
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent_format
#' @importFrom viridis viridis
#' @importFrom stats fisher.test chisq.test
#' @description Creates comprehensive visualizations of adverse event profiles with enhanced statistical analysis

toxicityprofileClass <- if(requireNamespace("jmvcore")) R6::R6Class(
    "toxicityprofileClass",
    inherit = toxicityprofileBase,
    private = list(

        # Calculate incidence rates and confidence intervals
        .calculateIncidence = function(n_events, n_total, conf_level = 0.95) {
            incidence <- n_events / n_total * 100

            # Calculate exact binomial confidence intervals
            if (n_events > 0 && n_total > 0) {
                binom_test <- binom.test(n_events, n_total, conf.level = conf_level)
                ci_lower <- binom_test$conf.int[1] * 100
                ci_upper <- binom_test$conf.int[2] * 100
            } else {
                ci_lower <- 0
                ci_upper <- 0
            }

            return(list(
                incidence = incidence,
                ci_lower = ci_lower,
                ci_upper = ci_upper
            ))
        },

        # Get color scheme for grades
        .getGradeColors = function(color_scheme, n_grades) {
            if (color_scheme == "ctcae") {
                # Standard CTCAE colors: green -> yellow -> orange -> red -> dark red
                colors <- c("#2E8B57", "#FFD700", "#FF8C00", "#FF4500", "#8B0000")
            } else if (color_scheme == "traffic") {
                colors <- c("#00FF00", "#FFFF00", "#FFA500", "#FF0000", "#800000")
            } else if (color_scheme == "viridis") {
                colors <- viridis::viridis(n_grades)
            } else if (color_scheme == "custom") {
                # Custom clinical color scheme
                colors <- c("#4CAF50", "#FF9800", "#F44336", "#9C27B0", "#000000")
            } else {
                # Default to RColorBrewer
                colors <- RColorBrewer::brewer.pal(min(n_grades, 9), "YlOrRd")
            }

            return(colors[1:n_grades])
        },

        # Calculate cumulative incidence over time
        .calculateCumulativeIncidence = function(df, patient_var, ae_var, time_var) {
            if (is.null(time_var) || !time_var %in% names(df)) {
                return(NULL)
            }

            # Create time-to-event data
            time_data <- df %>%
                dplyr::group_by(.data[[patient_var]], .data[[ae_var]]) %>%
                dplyr::summarise(
                    time_to_event = min(.data[[time_var]], na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                dplyr::arrange(time_to_event)

            # Calculate cumulative incidence
            total_patients <- length(unique(df[[patient_var]]))

            cumulative_data <- time_data %>%
                dplyr::group_by(.data[[ae_var]]) %>%
                dplyr::mutate(
                    cumulative_count = row_number(),
                    cumulative_incidence = cumulative_count / total_patients * 100
                ) %>%
                dplyr::ungroup()

            return(cumulative_data)
        },

        # Enhanced statistical tests
        .performStatisticalTests = function(df, ae_var, grade_var, treatment_var) {
            if (is.null(treatment_var) || !treatment_var %in% names(df)) {
                return(list())
            }

            results <- list()
            unique_aes <- unique(df[[ae_var]])

            for (ae in unique_aes) {
                ae_data <- df[df[[ae_var]] == ae, ]

                # Test for grade distribution differences between treatments
                if (nrow(ae_data) > 0) {
                    grade_table <- table(ae_data[[treatment_var]], ae_data[[grade_var]])

                    # Chi-square test for grade distribution
                    if (all(dim(grade_table) > 1) && sum(grade_table) > 0) {
                        tryCatch({
                            chi_test <- stats::chisq.test(grade_table)
                            results[[ae]] <- list(
                                test = "Chi-square",
                                statistic = chi_test$statistic,
                                p_value = chi_test$p.value,
                                df = chi_test$parameter
                            )
                        }, error = function(e) {
                            results[[ae]] <- list(
                                test = "Chi-square",
                                statistic = NA,
                                p_value = NA,
                                df = NA
                            )
                        })
                    }
                }
            }

            return(results)
        },

        # Validate input data
        .validateData = function(df, patient_var, ae_var, grade_var) {
            errors <- c()

            # Check required variables exist
            if (!patient_var %in% names(df)) {
                errors <- c(errors, "Patient ID variable not found in data")
            }
            if (!ae_var %in% names(df)) {
                errors <- c(errors, "Adverse event variable not found in data")
            }
            if (!grade_var %in% names(df)) {
                errors <- c(errors, "Grade variable not found in data")
            }

            if (length(errors) > 0) {
                return(errors)
            }

            # Check data types and ranges
            if (!is.numeric(df[[grade_var]]) && !is.factor(df[[grade_var]])) {
                errors <- c(errors, "Grade variable must be numeric or factor")
            }

            # Check grade values are in valid range
            grade_values <- as.numeric(as.character(df[[grade_var]]))
            if (any(!is.na(grade_values) & (grade_values < 1 | grade_values > 5))) {
                errors <- c(errors, "Grade values must be between 1 and 5")
            }

            return(errors)
        },

        # Compare groups statistically
        .compareGroups = function(df, ae_var, grade_var, treatment_var, conf_level) {
            results <- list()

            # Get unique adverse events
            unique_aes <- unique(df[[ae_var]])
            unique_aes <- unique_aes[!is.na(unique_aes)]

            # Get treatment groups
            treatment_groups <- unique(df[[treatment_var]])
            treatment_groups <- treatment_groups[!is.na(treatment_groups)]

            if (length(treatment_groups) != 2) {
                return(results)  # Only handle 2-group comparisons
            }

            for (ae in unique_aes) {
                # Count events in each group
                group1_data <- df[df[[ae_var]] == ae & df[[treatment_var]] == treatment_groups[1], ]
                group2_data <- df[df[[ae_var]] == ae & df[[treatment_var]] == treatment_groups[2], ]

                # Total patients in each group
                total_group1 <- length(unique(df[df[[treatment_var]] == treatment_groups[1], ][[self$options$patientID]]))
                total_group2 <- length(unique(df[df[[treatment_var]] == treatment_groups[2], ][[self$options$patientID]]))

                # Events in each group
                events_group1 <- length(unique(group1_data[[self$options$patientID]]))
                events_group2 <- length(unique(group2_data[[self$options$patientID]]))

                # Calculate incidences
                inc1 <- private$.calculateIncidence(events_group1, total_group1, conf_level)
                inc2 <- private$.calculateIncidence(events_group2, total_group2, conf_level)

                # Calculate risk ratio
                if (events_group2 > 0 && total_group2 > 0) {
                    rr <- (events_group1/total_group1) / (events_group2/total_group2)

                    # Fisher's exact test
                    tryCatch({
                        fisher_test <- fisher.test(matrix(c(
                            events_group1, total_group1 - events_group1,
                            events_group2, total_group2 - events_group2
                        ), nrow = 2))
                        pval <- fisher_test$p.value
                        rr_ci <- fisher_test$conf.int
                    }, error = function(e) {
                        pval <- NA
                        rr_ci <- c(NA, NA)
                    })
                } else {
                    rr <- NA
                    pval <- NA
                    rr_ci <- c(NA, NA)
                }

                results[[ae]] <- list(
                    adverse_event = ae,
                    group1_incidence = inc1$incidence,
                    group2_incidence = inc2$incidence,
                    risk_ratio = rr,
                    ci_lower = rr_ci[1],
                    ci_upper = rr_ci[2],
                    pvalue = pval
                )
            }

            return(results)
        },

        .run = function() {
            # Check required variables
            if (is.null(self$options$patientID) ||
                is.null(self$options$adverseEvent) ||
                is.null(self$options$grade)) {

                todo <- "
                <br>Welcome to ClinicoPath Enhanced Toxicity Profile Analysis
                <br><br>
                This tool creates comprehensive visualizations and statistical analysis of adverse event profiles
                for clinical trials and safety monitoring.
                <br><br>
                <b>Required variables:</b>
                <br>- Patient ID: Unique identifier for each patient
                <br>- Adverse Event: Name or category of adverse events
                <br>- Toxicity Grade: CTCAE grade or severity level (1-5)
                <br><br>
                <b>Optional variables:</b>
                <br>- Treatment Group: For comparing toxicity between treatment arms
                <br>- System Organ Class: For grouping related adverse events (MedDRA SOC)
                <br>- Time to Event: For time-based analysis and cumulative incidence
                <br><br>
                <b>Enhanced visualization options:</b>
                <br>- Stacked bar charts with grade distribution and confidence intervals
                <br>- Dot plots comparing incidence rates between groups
                <br>- Heatmaps for comprehensive overview with statistical significance
                <br>- Time-to-event plots with cumulative incidence curves
                <br>- System organ class summaries for regulatory reporting
                <br><br>
                <b>Statistical analysis features:</b>
                <br>- Exact binomial confidence intervals for incidence rates
                <br>- Fisher's exact test for group comparisons
                <br>- Chi-square tests for grade distribution differences
                <br>- Risk ratios with confidence intervals
                <br>- Cumulative incidence analysis over time
                <br><br>
                <b>Clinical applications:</b>
                <br>- Safety profile comparison between treatments
                <br>- Identification of dose-limiting toxicities
                <br>- Regulatory safety reporting (FDA, EMA)
                <br>- Real-world evidence studies
                <br>- Post-marketing surveillance
                <hr>
                "
                self$results$todo$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")

            data <- self$data
            patient_var <- self$options$patientID
            ae_var <- self$options$adverseEvent
            grade_var <- self$options$grade
            treatment_var <- self$options$treatment
            soc_var <- self$options$systemOrganClass
            time_var <- self$options$timeToEvent
            conf_level <- as.numeric(self$options$confidenceLevel)

            # Validate input data
            validation_errors <- private$.validateData(data, patient_var, ae_var, grade_var)
            if (length(validation_errors) > 0) {
                error_msg <- paste("Data validation errors:", paste(validation_errors, collapse = "; "))
                stop(error_msg)
            }

            # Clean and prepare data
            clean_data <- data[!is.na(data[[ae_var]]) & !is.na(data[[grade_var]]), ]

            if (nrow(clean_data) == 0) {
                stop("No complete cases found")
            }

            # Ensure grade is numeric
            clean_data[[grade_var]] <- as.numeric(as.character(clean_data[[grade_var]]))

            # Filter by minimum incidence
            total_patients <- length(unique(clean_data[[patient_var]]))
            ae_counts <- table(clean_data[[ae_var]])
            min_count <- ceiling(self$options$minIncidence / 100 * total_patients)
            included_aes <- names(ae_counts)[ae_counts >= min_count]

            if (self$options$showHighGradeOnly) {
                clean_data <- clean_data[clean_data[[grade_var]] >= 3, ]
            }

            clean_data <- clean_data[clean_data[[ae_var]] %in% included_aes, ]

            # Calculate summary statistics for each adverse event
            ae_summary <- list()
            for (ae in included_aes) {
                ae_data <- clean_data[clean_data[[ae_var]] == ae, ]
                n_patients_with_ae <- length(unique(ae_data[[patient_var]]))
                n_events <- nrow(ae_data)

                incidence_result <- private$.calculateIncidence(n_patients_with_ae, total_patients, conf_level)

                # Grade distribution
                grade_1_2 <- sum(ae_data[[grade_var]] %in% 1:2, na.rm = TRUE)
                grade_3_plus <- sum(ae_data[[grade_var]] >= 3, na.rm = TRUE)

                grade_1_2_pct <- grade_1_2 / n_patients_with_ae * 100
                grade_3_plus_pct <- grade_3_plus / n_patients_with_ae * 100

                # Median time to event if available
                median_time <- if (!is.null(time_var)) {
                    median(ae_data[[time_var]], na.rm = TRUE)
                } else {
                    NA
                }

                ae_summary[[ae]] <- list(
                    adverse_event = ae,
                    n_patients = n_patients_with_ae,
                    n_events = n_events,
                    incidence = incidence_result$incidence,
                    grade_1_2 = grade_1_2_pct,
                    grade_3_plus = grade_3_plus_pct,
                    median_time = median_time
                )
            }

            # Populate summary table
            for (ae in names(ae_summary)) {
                summary <- ae_summary[[ae]]
                self$results$summary$addRow(rowKey = ae, values = list(
                    adverse_event = summary$adverse_event,
                    n_patients = summary$n_patients,
                    n_events = summary$n_events,
                    incidence = summary$incidence,
                    grade_1_2 = summary$grade_1_2,
                    grade_3_plus = summary$grade_3_plus,
                    median_time = summary$median_time
                ))
            }

            # Grade distribution table
            for (ae in included_aes) {
                ae_data <- clean_data[clean_data[[ae_var]] == ae, ]
                grade_dist <- table(factor(ae_data[[grade_var]], levels = 1:5))

                self$results$gradeDistribution$addRow(rowKey = ae, values = list(
                    adverse_event = ae,
                    grade_1 = grade_dist[1],
                    grade_2 = grade_dist[2],
                    grade_3 = grade_dist[3],
                    grade_4 = grade_dist[4],
                    grade_5 = grade_dist[5]
                ))
            }

            # Group comparison if treatment variable provided
            if (!is.null(treatment_var) && self$options$groupComparison) {
                group_results <- private$.compareGroups(clean_data, ae_var, grade_var, treatment_var, conf_level)

                for (ae in names(group_results)) {
                    result <- group_results[[ae]]
                    self$results$groupComparison$addRow(rowKey = ae, values = result)
                }
            }

            # System organ class summary if provided
            if (!is.null(soc_var)) {
                soc_data <- clean_data[!is.na(clean_data[[soc_var]]), ]
                unique_socs <- unique(soc_data[[soc_var]])

                for (soc in unique_socs) {
                    soc_subset <- soc_data[soc_data[[soc_var]] == soc, ]
                    n_patients_soc <- length(unique(soc_subset[[patient_var]]))
                    n_events_soc <- nrow(soc_subset)

                    incidence_soc <- private$.calculateIncidence(n_patients_soc, total_patients, conf_level)

                    # High grade events in this SOC
                    high_grade_events <- sum(soc_subset[[grade_var]] >= 3, na.rm = TRUE)
                    high_grade_incidence <- high_grade_events / total_patients * 100

                    self$results$socSummary$addRow(rowKey = soc, values = list(
                        soc = soc,
                        n_events = n_events_soc,
                        n_patients = n_patients_soc,
                        incidence = incidence_soc$incidence,
                        high_grade_incidence = high_grade_incidence
                    ))
                }
            }

            # Prepare plot data
            plot_data <- list(
                data = clean_data,
                included_aes = included_aes,
                ae_summary = ae_summary,
                options = list(
                    plot_type = self$options$plotType,
                    sort_by = self$options$sortBy,
                    grade_colors = self$options$gradeColors,
                    show_percentages = self$options$showPercentages,
                    show_high_grade_only = self$options$showHighGradeOnly,
                    treatment_var = treatment_var,
                    soc_var = soc_var,
                    time_var = time_var
                ),
                variable_names = list(
                    patient = patient_var,
                    ae = ae_var,
                    grade = grade_var
                )
            )

            self$results$plot$setState(plot_data)
        },

        .plot = function(image, ggtheme, theme, ...) {
            plot_data <- image$state
            if (is.null(plot_data)) return()

            df <- plot_data$data
            included_aes <- plot_data$included_aes
            opts <- plot_data$options
            var_names <- plot_data$variable_names

            # Sort adverse events
            if (opts$sort_by == "frequency") {
                ae_order <- names(sort(table(df[[var_names$ae]]), decreasing = TRUE))
            } else if (opts$sort_by == "high_grade") {
                high_grade_counts <- tapply(df[[var_names$grade]] >= 3, df[[var_names$ae]], sum, na.rm = TRUE)
                ae_order <- names(sort(high_grade_counts, decreasing = TRUE))
            } else {
                ae_order <- sort(included_aes)
            }

            # Filter to included AEs and reorder
            df <- df[df[[var_names$ae]] %in% included_aes, ]
            df[[var_names$ae]] <- factor(df[[var_names$ae]], levels = ae_order)

            # Get colors for grades
            unique_grades <- sort(unique(df[[var_names$grade]]))
            grade_colors <- private$.getGradeColors(opts$grade_colors, length(unique_grades))
            names(grade_colors) <- unique_grades

            # Create different plot types
            if (opts$plot_type == "stacked_bar") {
                # Stacked bar chart by grade
                p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_names$ae]], fill = factor(.data[[var_names$grade]]))) +
                    ggplot2::geom_bar(position = "fill") +
                    ggplot2::scale_fill_manual(values = grade_colors, name = "Grade") +
                    ggplot2::coord_flip()

                if (opts$show_percentages) {
                    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format())
                }

            } else if (opts$plot_type == "dot_plot") {
                # Calculate incidence rates for dot plot with confidence intervals
                incidence_data <- data.frame()
                for (ae in ae_order) {
                    ae_patients <- length(unique(df[df[[var_names$ae]] == ae, var_names$patient]))
                    total_patients <- length(unique(df[[var_names$patient]]))
                    incidence <- ae_patients / total_patients * 100

                    # Calculate confidence intervals
                    if (ae_patients > 0) {
                        binom_test <- binom::binom.test(ae_patients, total_patients, conf.level = 0.95)
                        ci_lower <- binom_test$conf.int[1] * 100
                        ci_upper <- binom_test$conf.int[2] * 100
                    } else {
                        ci_lower <- 0
                        ci_upper <- 0
                    }

                    incidence_data <- rbind(incidence_data, data.frame(
                        ae = ae,
                        incidence = incidence,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper
                    ))
                }

                p <- ggplot2::ggplot(incidence_data, ggplot2::aes(x = incidence, y = factor(ae, levels = rev(ae_order)))) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = incidence, yend = factor(ae, levels = rev(ae_order)))) +
                    ggplot2::labs(x = "Incidence (%)", y = "Adverse Event")

                # Add confidence intervals if requested
                if (self$options$showConfidenceIntervals) {
                    p <- p + ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.2)
                }

            } else if (opts$plot_type == "heatmap") {
                # Create summary data for heatmap
                heatmap_data <- df %>%
                    dplyr::group_by(.data[[var_names$ae]], .data[[var_names$grade]]) %>%
                    dplyr::summarise(count = dplyr::n(), .groups = "drop")

                p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = factor(.data[[var_names$grade]]),
                                                               y = factor(.data[[var_names$ae]], levels = rev(ae_order)),
                                                               fill = count)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_gradient(low = "white", high = "red", name = "Count") +
                    ggplot2::labs(x = "Grade", y = "Adverse Event")

            } else if (opts$plot_type == "time_to_event") {
                # Time-to-event plot with cumulative incidence
                if (!is.null(opts$time_var) && opts$time_var %in% names(df)) {
                    # Calculate cumulative incidence data
                    cumulative_data <- private$.calculateCumulativeIncidence(df, var_names$patient, var_names$ae, opts$time_var)

                    if (!is.null(cumulative_data)) {
                        p <- ggplot2::ggplot(cumulative_data, ggplot2::aes(x = time_to_event, y = cumulative_incidence, color = .data[[var_names$ae]])) +
                            ggplot2::geom_step() +
                            ggplot2::labs(x = "Time to Event", y = "Cumulative Incidence (%)", color = "Adverse Event") +
                            ggplot2::theme(legend.position = "right")
                    } else {
                        # Fallback to stacked bar if time data not available
                        p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_names$ae]], fill = factor(.data[[var_names$grade]]))) +
                            ggplot2::geom_bar() +
                            ggplot2::scale_fill_manual(values = grade_colors, name = "Grade") +
                            ggplot2::coord_flip()
                    }
                } else {
                    # Fallback to stacked bar if time variable not provided
                    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_names$ae]], fill = factor(.data[[var_names$grade]]))) +
                        ggplot2::geom_bar() +
                        ggplot2::scale_fill_manual(values = grade_colors, name = "Grade") +
                        ggplot2::coord_flip()
                }

            } else {
                # Default to stacked bar
                p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_names$ae]], fill = factor(.data[[var_names$grade]]))) +
                    ggplot2::geom_bar() +
                    ggplot2::scale_fill_manual(values = grade_colors, name = "Grade") +
                    ggplot2::coord_flip()
            }

            # Apply theme and labels
            p <- p + ggplot2::labs(
                title = "Toxicity Profile Analysis",
                subtitle = paste("Plot type:", opts$plot_type)
            ) +
                ggtheme +
                ggplot2::theme(
                    axis.text.y = ggplot2::element_text(size = 8),
                    legend.position = "right"
                )

            print(p)
            TRUE
        }
    )
)
