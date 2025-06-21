#' @title Grouped Hazard Forest Plot for Subgroup Analysis
#' @return Grouped forest plot showing treatment effects across subgroups
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom survival coxph Surv
#' @importFrom survminer ggforest
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh geom_vline
#' @importFrom ggplot2 labs theme_minimal theme_classic scale_x_log10 coord_cartesian
#' @importFrom ggplot2 theme element_text element_blank geom_text
#' @importFrom dplyr group_by summarise mutate arrange filter
#' @importFrom broom tidy
#' @importFrom htmltools HTML

groupedforestClass <- if (requireNamespace("jmvcore")) R6::R6Class("groupedforestClass",
    inherit = groupedforestBase,
    private = list(

        .run = function() {

            # Check if required variables have been selected
            if (is.null(self$options$time_var) || is.null(self$options$event_var) || 
                is.null(self$options$treatment_var) || is.null(self$options$grouping_var)) {
                intro_msg <- "
                <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #2e7d32; margin-top: 0;'>🌲 Welcome to Grouped Hazard Forest Plot!</h3>
                <p><strong>Compare treatment effects across subgroups</strong> with grouped Cox regression forest plots</p>
                <p>Addresses GitHub Issue #88: Create grouped forest plots showing treatment vs control for each variant</p>
                
                <h4 style='color: #2e7d32;'>Required Variables:</h4>
                <ol>
                <li><strong>Time Variable:</strong> Follow-up duration (continuous)</li>
                <li><strong>Event Variable:</strong> Event indicator (0=censored, 1=event)</li>
                <li><strong>Treatment Variable:</strong> Treatment vs control comparison</li>
                <li><strong>Grouping Variable:</strong> Subgroups/variants for separate analyses</li>
                </ol>
                
                <h4 style='color: #2e7d32;'>What This Provides:</h4>
                <ul>
                <li><strong>Grouped Analysis:</strong> Separate Cox regression for each subgroup</li>
                <li><strong>Forest Plot:</strong> Visual comparison of hazard ratios across groups</li>
                <li><strong>Statistical Tests:</strong> Individual and interaction testing</li>
                <li><strong>Clinical Application:</strong> Treatment effectiveness by patient variant</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Perfect For:</h4>
                <ul>
                <li><strong>Precision Medicine:</strong> Treatment effects by genetic variants</li>
                <li><strong>Subgroup Analysis:</strong> Treatment response across patient types</li>
                <li><strong>Clinical Trials:</strong> Efficacy in different populations</li>
                <li><strong>Biomarker Studies:</strong> Treatment interaction with biomarkers</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>This module performs separate Cox regressions for each subgroup and presents unified results</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
            }

            # Get data and variables
            dataset <- self$data
            time_var <- self$options$time_var
            event_var <- self$options$event_var
            treatment_var <- self$options$treatment_var
            grouping_var <- self$options$grouping_var
            covariates <- self$options$covariates

            # Prepare analysis data
            required_vars <- c(time_var, event_var, treatment_var, grouping_var)
            if (length(covariates) > 0) {
                required_vars <- c(required_vars, covariates)
            }
            
            analysis_data <- dataset[required_vars]
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                stop("Error: No complete cases found for the selected variables.")
            }

            # Convert variables to appropriate types
            analysis_data[[time_var]] <- as.numeric(analysis_data[[time_var]])
            analysis_data[[event_var]] <- as.numeric(analysis_data[[event_var]])
            analysis_data[[treatment_var]] <- as.factor(analysis_data[[treatment_var]])
            analysis_data[[grouping_var]] <- as.factor(analysis_data[[grouping_var]])

            # Set reference level for treatment if specified
            if (self$options$reference_treatment != "") {
                if (self$options$reference_treatment %in% levels(analysis_data[[treatment_var]])) {
                    analysis_data[[treatment_var]] <- relevel(analysis_data[[treatment_var]], 
                                                              ref = self$options$reference_treatment)
                }
            }

            # Perform grouped Cox regression analysis
            grouped_results <- private$.perform_grouped_analysis(analysis_data, time_var, event_var, 
                                                                treatment_var, grouping_var, covariates)
            
            # Generate statistics table
            if (self$options$show_statistics && !is.null(grouped_results)) {
                stats_html <- private$.generate_statistics_table(grouped_results)
                self$results$statistics_table$setContent(stats_html)
            }
            
            # Generate overall analysis if requested
            if (self$options$show_overall && !is.null(grouped_results)) {
                overall_html <- private$.generate_overall_analysis(analysis_data, time_var, event_var, 
                                                                  treatment_var, covariates)
                self$results$overall_stats$setContent(overall_html)
            }
            
            # Generate sample sizes summary
            if (self$options$show_counts && !is.null(grouped_results)) {
                counts_html <- private$.generate_sample_sizes(analysis_data, treatment_var, grouping_var)
                self$results$sample_sizes$setContent(counts_html)
            }
            
            # Generate interaction test if requested
            if (self$options$interaction_test && !is.null(grouped_results)) {
                interaction_html <- private$.generate_interaction_test(analysis_data, time_var, event_var,
                                                                      treatment_var, grouping_var, covariates)
                self$results$interaction_test$setContent(interaction_html)
            }
            
            # Generate interpretation guide
            interpretation_html <- private$.generate_interpretation_guide(grouped_results, treatment_var, grouping_var)
            self$results$interpretation$setContent(interpretation_html)

            # Store results for plotting
            private$.grouped_results <- grouped_results

        },

        .plot_forest = function(image, ggtheme, theme, ...) {
            
            # Check if analysis was performed
            if (is.null(private$.grouped_results)) {
                return()
            }
            
            grouped_results <- private$.grouped_results
            
            # Create forest plot data
            plot_data <- private$.prepare_forest_plot_data(grouped_results)
            
            if (is.null(plot_data) || nrow(plot_data) == 0) {
                return()
            }
            
            # Sort by HR if requested
            if (self$options$sort_by_hr) {
                plot_data <- plot_data %>% dplyr::arrange(hr)
            }
            
            # Set HR range
            hr_limits <- private$.get_hr_limits(plot_data)
            
            # Create the forest plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = hr, y = reorder(group, hr))) +
                ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", size = 0.8) +
                ggplot2::geom_point(size = 3, color = "#2e7d32") +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf_low, xmax = conf_high), 
                                       height = 0.2, color = "#2e7d32", size = 0.8) +
                ggplot2::scale_x_log10(limits = hr_limits, 
                                      breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                                      labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10")) +
                ggplot2::labs(
                    title = if (self$options$plot_title != "") self$options$plot_title else "Grouped Hazard Forest Plot",
                    x = "Hazard Ratio (95% CI)",
                    y = "Subgroups"
                ) +
                private$.get_forest_theme()
            
            # Add HR text annotations
            if (self$options$show_statistics) {
                p <- p + ggplot2::geom_text(
                    ggplot2::aes(x = hr_limits[2] * 0.7, 
                                label = paste0("HR: ", sprintf("%.2f", hr), 
                                              " (", sprintf("%.2f", conf_low), 
                                              "-", sprintf("%.2f", conf_high), ")")),
                    hjust = 0, size = 3
                )
            }
            
            # Add sample sizes if requested
            if (self$options$show_counts && !is.null(plot_data$n_total)) {
                p <- p + ggplot2::geom_text(
                    ggplot2::aes(x = hr_limits[1] * 1.5, label = paste0("n=", n_total)),
                    hjust = 0, size = 3, color = "gray40"
                )
            }
            
            print(p)
            TRUE
        },

        .perform_grouped_analysis = function(data, time_var, event_var, treatment_var, grouping_var, covariates) {
            # Perform Cox regression for each subgroup
            
            groups <- levels(data[[grouping_var]])
            results_list <- list()
            
            for (group in groups) {
                group_data <- data[data[[grouping_var]] == group, ]
                
                if (nrow(group_data) < 5) {
                    next  # Skip groups with too few observations
                }
                
                # Build formula
                if (length(covariates) > 0) {
                    formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var, " + ", 
                                         paste(covariates, collapse = " + "))
                } else {
                    formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var)
                }
                
                cox_formula <- as.formula(formula_str)
                
                tryCatch({
                    # Fit Cox model
                    cox_model <- survival::coxph(cox_formula, data = group_data)
                    
                    # Extract results for treatment variable
                    model_summary <- broom::tidy(cox_model, conf.int = TRUE, 
                                               conf.level = self$options$confidence_level)
                    
                    # Find treatment coefficient
                    treatment_coef <- model_summary[grepl(treatment_var, model_summary$term), ]
                    
                    if (nrow(treatment_coef) > 0) {
                        results_list[[group]] <- list(
                            group = group,
                            n_total = nrow(group_data),
                            n_events = sum(group_data[[event_var]]),
                            hr = treatment_coef$estimate[1],
                            hr_exp = exp(treatment_coef$estimate[1]),
                            conf_low = exp(treatment_coef$conf.low[1]),
                            conf_high = exp(treatment_coef$conf.high[1]),
                            p_value = treatment_coef$p.value[1],
                            model = cox_model
                        )
                    }
                    
                }, error = function(e) {
                    # Skip problematic groups
                    warning(paste("Cox regression failed for group", group, ":", e$message))
                })
            }
            
            if (length(results_list) == 0) {
                stop("No valid Cox regression results obtained for any subgroup")
            }
            
            return(results_list)
        },

        .prepare_forest_plot_data = function(grouped_results) {
            # Convert results to data frame for plotting
            
            plot_data <- do.call(rbind, lapply(names(grouped_results), function(group) {
                result <- grouped_results[[group]]
                data.frame(
                    group = result$group,
                    hr = result$hr_exp,
                    conf_low = result$conf_low,
                    conf_high = result$conf_high,
                    p_value = result$p_value,
                    n_total = result$n_total,
                    n_events = result$n_events,
                    stringsAsFactors = FALSE
                )
            }))
            
            return(plot_data)
        },

        .get_hr_limits = function(plot_data) {
            # Determine appropriate HR limits for x-axis
            
            hr_range <- self$options$hr_range
            
            if (hr_range == "auto") {
                min_hr <- min(plot_data$conf_low, na.rm = TRUE)
                max_hr <- max(plot_data$conf_high, na.rm = TRUE)
                
                # Add some padding
                min_limit <- max(0.05, min_hr * 0.8)
                max_limit <- min(20, max_hr * 1.2)
                
                return(c(min_limit, max_limit))
            } else if (hr_range == "wide") {
                return(c(0.1, 10))
            } else if (hr_range == "narrow") {
                return(c(0.5, 2))
            } else if (hr_range == "custom") {
                return(c(self$options$custom_hr_min, self$options$custom_hr_max))
            }
            
            return(c(0.1, 10))  # Default
        },

        .get_forest_theme = function() {
            # Get plot theme based on selection
            
            theme_name <- self$options$plot_theme
            
            base_theme <- switch(theme_name,
                "clinical" = ggplot2::theme_minimal() + 
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.border = ggplot2::element_rect(fill = NA, color = "gray80"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10),
                        legend.position = "none"
                    ),
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "publication" = ggplot2::theme_minimal() +
                    ggplot2::theme(
                        panel.grid = ggplot2::element_blank(),
                        panel.border = ggplot2::element_rect(fill = NA, color = "black"),
                        plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.text = ggplot2::element_text(size = 12),
                        legend.position = "none"
                    ),
                ggplot2::theme_minimal()  # fallback
            )
            
            return(base_theme)
        },

        .generate_statistics_table = function(grouped_results) {
            # Generate detailed statistics table
            
            stats_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>📊 Hazard Ratios by Subgroup</h3>",
                "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>",
                "<thead><tr style='background-color: #6c757d; color: white;'>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>Subgroup</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>N (Events)</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>HR</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>95% CI</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>P-value</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>Significance</th>",
                "</tr></thead><tbody>"
            )
            
            i <- 0
            for (group_name in names(grouped_results)) {
                i <- i + 1
                result <- grouped_results[[group_name]]
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                
                significance <- if (result$p_value < 0.001) "***" else 
                               if (result$p_value < 0.01) "**" else
                               if (result$p_value < 0.05) "*" else ""
                
                stats_html <- paste0(stats_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6;'><strong>", group_name, "</strong></td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", result$n_total, " (", result$n_events, ")</td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", sprintf("%.3f", result$hr_exp), "</td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", 
                    sprintf("%.3f", result$conf_low), " - ", sprintf("%.3f", result$conf_high), "</td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", sprintf("%.4f", result$p_value), "</td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", significance, "</td>",
                    "</tr>"
                )
            }
            
            stats_html <- paste0(stats_html, 
                "</tbody></table>",
                "<p style='font-size: 12px; color: #6c757d; margin-top: 15px;'>",
                "<em>* p < 0.05, ** p < 0.01, *** p < 0.001</em>",
                "</p></div>"
            )
            
            return(stats_html)
        },

        .generate_overall_analysis = function(data, time_var, event_var, treatment_var, covariates) {
            # Perform overall analysis (all groups combined)
            
            if (length(covariates) > 0) {
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var, " + ", 
                                     paste(covariates, collapse = " + "))
            } else {
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var)
            }
            
            cox_formula <- as.formula(formula_str)
            cox_model <- survival::coxph(cox_formula, data = data)
            
            model_summary <- broom::tidy(cox_model, conf.int = TRUE, conf.level = self$options$confidence_level)
            treatment_coef <- model_summary[grepl(treatment_var, model_summary$term), ]
            
            overall_html <- paste0(
                "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #1976d2; margin-top: 0;'>📈 Overall Analysis (All Groups Combined)</h3>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Total Sample Size:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", nrow(data), "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Total Events:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", sum(data[[event_var]]), "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Overall HR:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", sprintf("%.3f", exp(treatment_coef$estimate[1])), "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>95% CI:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", 
                sprintf("%.3f", exp(treatment_coef$conf.low[1])), " - ", sprintf("%.3f", exp(treatment_coef$conf.high[1])), "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>P-value:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", sprintf("%.4f", treatment_coef$p.value[1]), "</td></tr>",
                "</table></div>"
            )
            
            return(overall_html)
        },

        .generate_sample_sizes = function(data, treatment_var, grouping_var) {
            # Generate sample sizes by group and treatment
            
            counts_table <- data %>%
                dplyr::group_by(.data[[grouping_var]], .data[[treatment_var]]) %>%
                dplyr::summarise(n = dplyr::n(), .groups = 'drop')
            
            counts_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #f57c00; margin-top: 0;'>👥 Sample Sizes by Group and Treatment</h3>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr style='background-color: #e0e0e0;'>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Subgroup</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Treatment Level</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>N</th>",
                "</tr>"
            )
            
            for (i in 1:nrow(counts_table)) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#fff3e0"
                counts_html <- paste0(counts_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", counts_table[[grouping_var]][i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", counts_table[[treatment_var]][i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", counts_table$n[i], "</td>",
                    "</tr>"
                )
            }
            
            counts_html <- paste0(counts_html, "</table></div>")
            return(counts_html)
        },

        .generate_interaction_test = function(data, time_var, event_var, treatment_var, grouping_var, covariates) {
            # Test for interaction between treatment and grouping variable
            
            # Build formula with interaction
            if (length(covariates) > 0) {
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var, " * ", grouping_var, " + ", 
                                     paste(covariates, collapse = " + "))
            } else {
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var, " * ", grouping_var)
            }
            
            cox_formula <- as.formula(formula_str)
            cox_model <- survival::coxph(cox_formula, data = data)
            
            # Extract interaction terms
            model_summary <- broom::tidy(cox_model)
            interaction_terms <- model_summary[grepl(":", model_summary$term), ]
            
            interaction_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>🔗 Treatment × Subgroup Interaction Test</h3>",
                "<p><strong>Interaction Terms:</strong></p>",
                "<table style='width: 100%; border-collapse: collapse;'>"
            )
            
            if (nrow(interaction_terms) > 0) {
                interaction_html <- paste0(interaction_html,
                    "<tr style='background-color: #e0e0e0;'>",
                    "<th style='padding: 8px; border: 1px solid #ddd;'>Interaction Term</th>",
                    "<th style='padding: 8px; border: 1px solid #ddd;'>Coefficient</th>",
                    "<th style='padding: 8px; border: 1px solid #ddd;'>P-value</th>",
                    "</tr>"
                )
                
                for (i in 1:nrow(interaction_terms)) {
                    interaction_html <- paste0(interaction_html,
                        "<tr>",
                        "<td style='padding: 8px; border: 1px solid #ddd;'>", interaction_terms$term[i], "</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", sprintf("%.4f", interaction_terms$estimate[i]), "</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", sprintf("%.4f", interaction_terms$p.value[i]), "</td>",
                        "</tr>"
                    )
                }
                
                # Overall interaction p-value
                min_p <- min(interaction_terms$p.value)
                interaction_significant <- min_p < 0.05
                
                interaction_html <- paste0(interaction_html,
                    "</table>",
                    "<p style='margin-top: 15px;'><strong>Interpretation:</strong> ",
                    if (interaction_significant) {
                        paste0("Significant interaction detected (min p = ", sprintf("%.4f", min_p), "). Treatment effects differ significantly across subgroups.")
                    } else {
                        paste0("No significant interaction detected (min p = ", sprintf("%.4f", min_p), "). Treatment effects are relatively consistent across subgroups.")
                    },
                    "</p>"
                )
            } else {
                interaction_html <- paste0(interaction_html,
                    "<p>No interaction terms found in the model.</p>"
                )
            }
            
            interaction_html <- paste0(interaction_html, "</div>")
            return(interaction_html)
        },

        .generate_interpretation_guide = function(grouped_results, treatment_var, grouping_var) {
            # Generate interpretation guide
            
            if (is.null(grouped_results)) {
                return("<div>No results available for interpretation.</div>")
            }
            
            n_groups <- length(grouped_results)
            
            interpretation_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📋 Grouped Forest Plot Interpretation</h3>",
                
                "<h4 style='color: #2e7d32;'>Analysis Summary:</h4>",
                "<ul>",
                "<li><strong>Treatment Variable:</strong> ", treatment_var, "</li>",
                "<li><strong>Grouping Variable:</strong> ", grouping_var, "</li>",
                "<li><strong>Number of Subgroups:</strong> ", n_groups, "</li>",
                "<li><strong>Analysis Method:</strong> Separate Cox regression for each subgroup</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>How to Read the Forest Plot:</h4>",
                "<ul>",
                "<li><strong>Vertical Line (HR = 1):</strong> No treatment effect</li>",
                "<li><strong>Points Left of Line (HR < 1):</strong> Treatment reduces hazard (beneficial)</li>",
                "<li><strong>Points Right of Line (HR > 1):</strong> Treatment increases hazard (harmful)</li>",
                "<li><strong>Horizontal Lines:</strong> 95% confidence intervals</li>",
                "<li><strong>Point Size:</strong> Represents precision of estimate</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Clinical Applications:</h4>",
                "<ul>",
                "<li><strong>Precision Medicine:</strong> Identify which patient subgroups benefit most from treatment</li>",
                "<li><strong>Biomarker Stratification:</strong> Treatment efficacy by molecular subtypes</li>",
                "<li><strong>Clinical Decision Making:</strong> Personalized treatment recommendations</li>",
                "<li><strong>Trial Design:</strong> Inform future studies and patient selection</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Statistical Considerations:</h4>",
                "<ul>",
                "<li><strong>Multiple Comparisons:</strong> Consider adjusting p-values for multiple subgroups</li>",
                "<li><strong>Sample Size:</strong> Smaller subgroups have wider confidence intervals</li>",
                "<li><strong>Interaction Testing:</strong> Use formal interaction tests to assess differences</li>",
                "<li><strong>Clinical Significance:</strong> Consider both statistical and clinical significance</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #2e7d32; margin-top: 15px;'>",
                "<em>💡 This grouped forest plot addresses the need for subgroup-specific treatment effect visualization as requested in GitHub Issue #88.</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        }

    )
)

# Store results for plotting
.grouped_results <- NULL