#' @title Grouped Bar Chart Comparison
#' @description 
#' Creates grouped bar charts to compare measurements across different groups,
#' inspired by Pew Research Center and similar professional visualizations.
#' Supports clinical lab values, survey responses, and any measurements that vary by group.
#'
#' @details
#' This module provides flexible grouped bar chart visualization with:
#' - Multiple plot layouts (grouped, stacked, horizontal)
#' - Statistical comparisons between groups
#' - Error bars and confidence intervals
#' - Professional styling options
#' - Reference lines for clinical cutoffs
#' - Faceting by additional variables
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_bar geom_col geom_point stat_summary
#' @importFrom ggplot2 position_dodge position_stack position_fill coord_flip
#' @importFrom ggplot2 theme_minimal theme_classic labs scale_fill_brewer
#' @importFrom ggplot2 scale_fill_viridis_d scale_fill_manual geom_text
#' @importFrom ggplot2 facet_wrap facet_grid theme element_text geom_hline
#' @importFrom ggplot2 geom_errorbar element_rect geom_ribbon guides guide_legend
#' @importFrom dplyr group_by summarise mutate arrange filter select distinct
#' @importFrom dplyr case_when if_else n left_join across all_of
#' @importFrom tidyr pivot_longer pivot_wider gather spread
#' @importFrom scales percent comma label_number
#' @importFrom stats aov t.test wilcox.test chisq.test kruskal.test
#' @importFrom stats fisher.test anova lm glm pairwise.t.test TukeyHSD
#' @importFrom stats aggregate median sd qt quantile IQR
#' @importFrom stringr str_to_title str_wrap str_detect str_split
#' @importFrom rlang sym syms .data !! !!!
#' @importFrom RColorBrewer brewer.pal
#' @export

groupedbarClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "groupedbarClass",
    inherit = groupedbarBase,
    private = list(
        
        # Internal data storage
        .processed_data = NULL,
        .summary_stats = NULL,
        .statistical_results = NULL,
        .plot_data = NULL,
        .package_availability = NULL,
        
        .init = function() {
            # Check package availability
            private$.check_package_availability()
            
            # Initialize instructions
            instructions_html <- paste0(
                "<div style='background-color: #e8f4fd; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #0277bd; margin-top: 0;'>📊 Grouped Bar Chart Comparison</h3>",
                "<div style='margin: 15px 0;'>",
                "<p><strong>Create professional grouped bar charts to compare measurements across groups:</strong></p>",
                "<ul style='margin: 10px 0; padding-left: 25px; line-height: 1.8;'>",
                "<li><strong>Clinical Applications:</strong> Lab values by disease groups, biomarkers by treatment arms</li>",
                "<li><strong>Survey Research:</strong> Response patterns by demographics (age, gender, location)</li>",
                "<li><strong>Quality Control:</strong> Performance metrics by hospital, department, or time period</li>",
                "<li><strong>Comparative Studies:</strong> Outcomes across multiple conditions or interventions</li>",
                "</ul>",
                "</div>",
                "<div style='background-color: #fff8e1; padding: 12px; border-radius: 5px; margin: 15px 0;'>",
                "<h4 style='color: #ff8f00; margin: 0 0 8px 0;'>🚀 Quick Start:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Select Items:</strong> Choose variables/measurements to compare</li>",
                "<li><strong>Choose Groups:</strong> Select grouping variable (e.g., disease_group, age_category)</li>",
                "<li><strong>Add Values:</strong> Optional numeric values (if not calculating from data)</li>",
                "<li><strong>Customize:</strong> Choose colors, statistics, and layout options</li>",
                "</ol>",
                "</div>",
                "<div style='background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #7b1fa2;'><strong>💡 Pro Tip:</strong> Use reference lines to show normal ranges or clinical cutoffs for better interpretation.</p>",
                "</div>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
            
            # Set initial plot dimensions
            plot_width <- as.numeric(self$options$width) * 100
            plot_height <- as.numeric(self$options$height) * 100
            self$results$plot$setSize(plot_width, plot_height)
        },
        
        .check_package_availability = function() {
            private$.package_availability <- list(
                ggplot2 = requireNamespace("ggplot2", quietly = TRUE),
                dplyr = requireNamespace("dplyr", quietly = TRUE),
                tidyr = requireNamespace("tidyr", quietly = TRUE),
                scales = requireNamespace("scales", quietly = TRUE),
                RColorBrewer = requireNamespace("RColorBrewer", quietly = TRUE),
                stringr = requireNamespace("stringr", quietly = TRUE),
                rlang = requireNamespace("rlang", quietly = TRUE)
            )
        },
        
        .validate_inputs = function() {
            errors <- character(0)
            
            # Check required variables
            if (length(self$options$items) == 0) {
                errors <- c(errors, "At least one item/variable is required")
            }
            
            if (is.null(self$options$groups) || self$options$groups == "") {
                errors <- c(errors, "Grouping variable is required")
            }
            
            # Validate data availability
            all_vars <- c(self$options$items, self$options$groups)
            if (!is.null(self$options$values)) {
                all_vars <- c(all_vars, self$options$values)
            }
            if (!is.null(self$options$facetby)) {
                all_vars <- c(all_vars, self$options$facetby)
            }
            
            missing_vars <- all_vars[!all_vars %in% names(self$data)]
            if (length(missing_vars) > 0) {
                errors <- c(errors, paste("Variables not found:", paste(missing_vars, collapse = ", ")))
            }
            
            # Check for sufficient data
            if (nrow(self$data) < 2) {
                errors <- c(errors, "At least 2 rows of data required")
            }
            
            # Validate group count for certain statistical tests
            if (self$options$showstatistics) {
                if (self$options$testtype %in% c("ttest", "wilcoxon")) {
                    n_groups <- length(unique(self$data[[self$options$groups]]))
                    if (n_groups != 2) {
                        errors <- c(errors, paste("Selected test requires exactly 2 groups, found", n_groups))
                    }
                }
            }
            
            if (length(errors) > 0) {
                stop(paste(errors, collapse = "; "))
            }
        },
        
        .run = function() {
            # Clear instructions if analysis is ready
            if (length(self$options$items) > 0 && !is.null(self$options$groups) && 
                self$options$groups != "") {
                self$results$instructions$setContent("")
            }
            
            # Early validation
            if (length(self$options$items) == 0) {
                return()
            }
            
            if (is.null(self$options$groups) || self$options$groups == "") {
                return()
            }
            
            # Input validation
            tryCatch({
                private$.validate_inputs()
            }, error = function(e) {
                self$results$plotnotes$setContent(
                    paste0("<div style='color: #d32f2f; background-color: #ffebee; padding: 10px; border-radius: 5px;'>",
                           "<strong>⚠️ Input Error:</strong> ", e$message, "</div>")
                )
                return()
            })
            
            # Process data
            tryCatch({
                private$.process_data()
                private$.generate_summary_statistics()
                
                if (self$options$showstatistics) {
                    private$.perform_statistical_tests()
                }
                
                # Set plot state
                self$results$plot$setState(list(
                    data = private$.plot_data,
                    options = self$options
                ))
                
            }, error = function(e) {
                self$results$plotnotes$setContent(
                    paste0("<div style='color: #d32f2f; background-color: #ffebee; padding: 10px; border-radius: 5px;'>",
                           "<strong>⚠️ Processing Error:</strong> ", e$message, "</div>")
                )
            })
        },
        
        .process_data = function() {
            # Get clean data
            data <- self$data
            items <- self$options$items
            groups <- self$options$groups
            values <- self$options$values
            facetby <- self$options$facetby
            
            # Remove missing data
            complete_vars <- c(items, groups)
            if (!is.null(values)) complete_vars <- c(complete_vars, values)
            if (!is.null(facetby)) complete_vars <- c(complete_vars, facetby)
            
            data <- data[complete.cases(data[complete_vars]), ]
            
            # Convert to long format for processing
            if (is.null(values)) {
                # Calculate statistics from the data
                plot_data <- data %>%
                    select(all_of(c(items, groups, if (!is.null(facetby)) facetby))) %>%
                    pivot_longer(cols = all_of(items), names_to = "Item", values_to = "Value") %>%
                    group_by(Item, !!sym(groups), 
                            if (!is.null(facetby)) !!sym(facetby)) %>%
                    summarise(
                        n = n(),
                        statistic_value = switch(self$options$statistic,
                            "mean" = mean(Value, na.rm = TRUE),
                            "median" = median(Value, na.rm = TRUE),
                            "sum" = sum(Value, na.rm = TRUE),
                            "count" = n(),
                            "percentage" = n() / nrow(data) * 100,
                            "proportion" = n() / nrow(data),
                            "sd" = sd(Value, na.rm = TRUE),
                            "se" = sd(Value, na.rm = TRUE) / sqrt(n()),
                            "min" = min(Value, na.rm = TRUE),
                            "max" = max(Value, na.rm = TRUE)
                        ),
                        se = if (self$options$statistic %in% c("mean", "median")) {
                            sd(Value, na.rm = TRUE) / sqrt(n())
                        } else NA_real_,
                        sd = if (self$options$statistic %in% c("mean", "median")) {
                            sd(Value, na.rm = TRUE)
                        } else NA_real_,
                        .groups = 'drop'
                    )
            } else {
                # Use provided values
                plot_data <- data %>%
                    select(all_of(c(items, groups, values, if (!is.null(facetby)) facetby))) %>%
                    pivot_longer(cols = all_of(items), names_to = "Item", values_to = "Category") %>%
                    group_by(Item, Category, !!sym(groups),
                            if (!is.null(facetby)) !!sym(facetby)) %>%
                    summarise(
                        n = n(),
                        statistic_value = switch(self$options$statistic,
                            "mean" = mean(!!sym(values), na.rm = TRUE),
                            "median" = median(!!sym(values), na.rm = TRUE),
                            "sum" = sum(!!sym(values), na.rm = TRUE),
                            mean(!!sym(values), na.rm = TRUE)
                        ),
                        se = sd(!!sym(values), na.rm = TRUE) / sqrt(n()),
                        sd = sd(!!sym(values), na.rm = TRUE),
                        .groups = 'drop'
                    )
            }
            
            # Apply sorting
            plot_data <- private$.apply_sorting(plot_data)
            
            # Store processed data
            private$.plot_data <- plot_data
            private$.processed_data <- data
        },
        
        .apply_sorting = function(data) {
            group_col <- self$options$groups
            
            # Sort items based on selection
            if (self$options$sortorder != "none" && self$options$sortorder != "alphabetical") {
                
                item_order <- switch(self$options$sortorder,
                    "highest_first" = {
                        # Get first group level
                        first_group <- sort(unique(data[[group_col]]))[1]
                        data %>%
                            filter(!!sym(group_col) == first_group) %>%
                            arrange(desc(statistic_value)) %>%
                            pull(Item)
                    },
                    "highest_last" = {
                        # Get last group level
                        last_group <- sort(unique(data[[group_col]], decreasing = TRUE))[1]
                        data %>%
                            filter(!!sym(group_col) == last_group) %>%
                            arrange(desc(statistic_value)) %>%
                            pull(Item)
                    },
                    "average" = {
                        data %>%
                            group_by(Item) %>%
                            summarise(avg = mean(statistic_value, na.rm = TRUE), .groups = 'drop') %>%
                            arrange(desc(avg)) %>%
                            pull(Item)
                    },
                    "maximum" = {
                        data %>%
                            group_by(Item) %>%
                            summarise(max_val = max(statistic_value, na.rm = TRUE), .groups = 'drop') %>%
                            arrange(desc(max_val)) %>%
                            pull(Item)
                    },
                    "range" = {
                        data %>%
                            group_by(Item) %>%
                            summarise(range_val = max(statistic_value, na.rm = TRUE) - 
                                     min(statistic_value, na.rm = TRUE), .groups = 'drop') %>%
                            arrange(desc(range_val)) %>%
                            pull(Item)
                    }
                )
                
                data$Item <- factor(data$Item, levels = item_order)
            } else if (self$options$sortorder == "alphabetical") {
                data$Item <- factor(data$Item, levels = sort(unique(data$Item)))
            }
            
            # Order groups
            if (self$options$grouporder != "data") {
                group_order <- switch(self$options$grouporder,
                    "alphabetical" = sort(unique(data[[group_col]])),
                    "mean" = {
                        data %>%
                            group_by(!!sym(group_col)) %>%
                            summarise(avg = mean(statistic_value, na.rm = TRUE), .groups = 'drop') %>%
                            arrange(desc(avg)) %>%
                            pull(!!sym(group_col))
                    },
                    "median" = {
                        data %>%
                            group_by(!!sym(group_col)) %>%
                            summarise(med = median(statistic_value, na.rm = TRUE), .groups = 'drop') %>%
                            arrange(desc(med)) %>%
                            pull(!!sym(group_col))
                    },
                    "custom" = {
                        if (self$options$customgrouporder != "") {
                            trimws(strsplit(self$options$customgrouporder, ",")[[1]])
                        } else {
                            unique(data[[group_col]])
                        }
                    }
                )
                
                data[[group_col]] <- factor(data[[group_col]], levels = group_order)
            }
            
            return(data)
        },
        
        .generate_summary_statistics = function() {
            if (!self$options$showTable) return()
            
            # Create descriptives table
            desc_data <- private$.plot_data %>%
                mutate(
                    item = as.character(Item),
                    group = as.character(!!sym(self$options$groups))
                ) %>%
                select(item, group, n, statistic_value, se, sd)
            
            # Add confidence intervals if needed
            if (self$options$showerrorbars && self$options$errortype %in% c("ci95", "ci99")) {
                alpha <- if (self$options$errortype == "ci95") 0.05 else 0.01
                desc_data <- desc_data %>%
                    mutate(
                        ci_lower = statistic_value - qt(1 - alpha/2, n - 1) * se,
                        ci_upper = statistic_value + qt(1 - alpha/2, n - 1) * se
                    )
            }
            
            # Populate table
            for (i in seq_len(nrow(desc_data))) {
                row <- desc_data[i, ]
                self$results$descriptives$addRow(rowKey = i, values = as.list(row))
            }
        },
        
        .perform_statistical_tests = function() {
            if (!self$options$showstatistics) return()
            
            test_results <- list()
            items <- unique(private$.plot_data$Item)
            
            for (item in items) {
                item_data <- private$.processed_data %>%
                    filter(if (!is.null(self$options$values)) {
                        !!sym(item) == item
                    } else {
                        TRUE
                    })
                
                # Perform appropriate test
                test_result <- private$.perform_single_test(item_data, item)
                test_results[[item]] <- test_result
                
                # Add to results table
                self$results$statistics$addRow(
                    rowKey = item,
                    values = list(
                        item = item,
                        test = test_result$test_name,
                        statistic = test_result$statistic,
                        df = test_result$df,
                        p = test_result$p_value,
                        effect_size = test_result$effect_size,
                        interpretation = test_result$interpretation
                    )
                )
            }
        },
        
        .perform_single_test = function(data, item_name) {
            group_var <- self$options$groups
            test_type <- self$options$testtype
            
            if (test_type == "auto") {
                # Auto-select test based on data
                n_groups <- length(unique(data[[group_var]]))
                if (n_groups == 2) {
                    test_type <- if (is.numeric(data[[item_name]])) "ttest" else "chisq"
                } else {
                    test_type <- if (is.numeric(data[[item_name]])) "anova" else "chisq"
                }
            }
            
            # Perform the test
            result <- switch(test_type,
                "anova" = {
                    model <- aov(as.formula(paste(item_name, "~", group_var)), data = data)
                    summary_model <- summary(model)
                    list(
                        test_name = "One-way ANOVA",
                        statistic = summary_model[[1]]["F value"][[1]][1],
                        df = paste(summary_model[[1]]["Df"][[1]][1], 
                                  summary_model[[1]]["Df"][[1]][2], sep = ", "),
                        p_value = summary_model[[1]]["Pr(>F)"][[1]][1],
                        effect_size = NA,
                        interpretation = if (summary_model[[1]]["Pr(>F)"][[1]][1] < 0.05) 
                                       "Significant difference" else "No significant difference"
                    )
                },
                "kruskal" = {
                    test_result <- kruskal.test(as.formula(paste(item_name, "~", group_var)), data = data)
                    list(
                        test_name = "Kruskal-Wallis",
                        statistic = test_result$statistic,
                        df = test_result$parameter,
                        p_value = test_result$p.value,
                        effect_size = NA,
                        interpretation = if (test_result$p.value < 0.05) 
                                       "Significant difference" else "No significant difference"
                    )
                },
                "ttest" = {
                    groups <- unique(data[[group_var]])
                    test_result <- t.test(data[data[[group_var]] == groups[1], item_name],
                                         data[data[[group_var]] == groups[2], item_name])
                    list(
                        test_name = "Two-sample t-test",
                        statistic = test_result$statistic,
                        df = test_result$parameter,
                        p_value = test_result$p.value,
                        effect_size = NA,
                        interpretation = if (test_result$p.value < 0.05) 
                                       "Significant difference" else "No significant difference"
                    )
                },
                "wilcoxon" = {
                    groups <- unique(data[[group_var]])
                    test_result <- wilcox.test(data[data[[group_var]] == groups[1], item_name],
                                              data[data[[group_var]] == groups[2], item_name])
                    list(
                        test_name = "Wilcoxon rank sum",
                        statistic = test_result$statistic,
                        df = NA,
                        p_value = test_result$p.value,
                        effect_size = NA,
                        interpretation = if (test_result$p.value < 0.05) 
                                       "Significant difference" else "No significant difference"
                    )
                }
            )
            
            return(result)
        },
        
        .plot = function(image, ...) {
            if (is.null(private$.plot_data)) return()
            
            # Load required packages
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                stop("ggplot2 package is required for plotting")
            }
            
            library(ggplot2)
            library(dplyr)
            
            plot_data <- private$.plot_data
            groups_var <- self$options$groups
            
            # Create base plot
            p <- ggplot(plot_data, aes(x = Item, y = statistic_value, 
                                      fill = !!sym(groups_var))) +
                theme_minimal() +
                labs(
                    title = if (self$options$title != "") self$options$title else NULL,
                    subtitle = if (self$options$subtitle != "") self$options$subtitle else NULL,
                    x = if (self$options$xlabel != "") self$options$xlabel else "Items",
                    y = if (self$options$ylabel != "") self$options$ylabel else 
                        paste(str_to_title(self$options$statistic), "Value"),
                    fill = if (self$options$legendtitle != "") self$options$legendtitle else groups_var
                )
            
            # Add bars based on plot type
            if (self$options$plottype == "grouped") {
                p <- p + geom_col(position = position_dodge(width = 0.8), alpha = 0.8)
            } else if (self$options$plottype == "stacked") {
                p <- p + geom_col(position = position_stack(), alpha = 0.8)
            } else if (self$options$plottype == "stacked_percent") {
                p <- p + geom_col(position = position_fill(), alpha = 0.8)
            } else if (self$options$plottype == "horizontal") {
                p <- p + geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
                    coord_flip()
            } else if (self$options$plottype == "cleveland") {
                p <- ggplot(plot_data, aes(x = statistic_value, y = Item, 
                                          color = !!sym(groups_var))) +
                    geom_point(size = 3) +
                    theme_minimal() +
                    labs(color = groups_var)
            }
            
            # Add values if requested
            if (self$options$showvalues && self$options$plottype != "cleveland") {
                value_pos <- if (self$options$valueposition == "inside") {
                    position_dodge(width = 0.8)
                } else {
                    position_dodge(width = 0.8)
                }
                
                p <- p + geom_text(aes(label = round(statistic_value, self$options$decimals)),
                                  position = value_pos,
                                  vjust = if (self$options$valueposition == "outside") -0.5 else 0.5,
                                  size = 3.5)
            }
            
            # Add error bars
            if (self$options$showerrorbars && !is.null(plot_data$se)) {
                error_val <- switch(self$options$errortype,
                    "se" = plot_data$se,
                    "sd" = plot_data$sd,
                    plot_data$se
                )
                
                p <- p + geom_errorbar(aes(ymin = statistic_value - error_val,
                                          ymax = statistic_value + error_val),
                                      position = position_dodge(width = 0.8),
                                      width = 0.2)
            }
            
            # Apply color scheme
            p <- private$.apply_color_scheme(p)
            
            # Add reference line
            if (!is.null(self$options$referenceline) && !is.na(self$options$referenceline)) {
                p <- p + geom_hline(yintercept = self$options$referenceline, 
                                   linetype = "dashed", color = "red", alpha = 0.7) +
                    annotate("text", x = Inf, y = self$options$referenceline,
                            label = self$options$referencelinelabel, 
                            hjust = 1.1, vjust = -0.5, color = "red")
            }
            
            # Configure legend
            if (self$options$showlegend) {
                p <- p + theme(legend.position = self$options$legendposition)
            } else {
                p <- p + theme(legend.position = "none")
            }
            
            # Add faceting if requested
            if (!is.null(self$options$facetby) && self$options$facetby != "") {
                p <- p + facet_wrap(vars(!!sym(self$options$facetby)), scales = "free")
            }
            
            # Final theme adjustments
            p <- p + theme(
                plot.title = element_text(size = 14, face = "bold"),
                plot.subtitle = element_text(size = 12, color = "gray50"),
                axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid = if (self$options$gridlines) element_line(color = "gray90") else element_blank()
            )
            
            print(p)
            TRUE
        },
        
        .apply_color_scheme = function(p) {
            n_colors <- length(unique(private$.plot_data[[self$options$groups]]))
            
            colors <- switch(self$options$colorscheme,
                "default" = NULL, # ggplot2 default
                "clinical" = c("#2E8B57", "#FFD700", "#FF6347")[1:n_colors], # Green-Yellow-Red
                "blues" = RColorBrewer::brewer.pal(max(3, min(n_colors, 9)), "Blues")[1:n_colors],
                "reds" = RColorBrewer::brewer.pal(max(3, min(n_colors, 9)), "Reds")[1:n_colors],
                "diverging" = RColorBrewer::brewer.pal(max(3, min(n_colors, 11)), "RdYlBu")[1:n_colors],
                "viridis" = viridis::viridis(n_colors),
                "colorblind" = RColorBrewer::brewer.pal(max(3, min(n_colors, 8)), "Dark2")[1:n_colors],
                "grayscale" = gray.colors(n_colors, start = 0.2, end = 0.8),
                "age_gradient" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")[1:n_colors],
                "custom" = {
                    if (self$options$customcolors != "") {
                        trimws(strsplit(self$options$customcolors, ",")[[1]])[1:n_colors]
                    } else NULL
                }
            )
            
            if (!is.null(colors)) {
                p <- p + scale_fill_manual(values = colors) +
                     scale_color_manual(values = colors)
            }
            
            return(p)
        }
    )
)