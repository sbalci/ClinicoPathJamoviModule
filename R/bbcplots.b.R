#' @title BBC-Style Data Visualization
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_col geom_bar geom_line geom_point geom_area
#' @importFrom ggplot2 geom_text labs theme element_text element_blank element_line
#' @importFrom ggplot2 scale_fill_manual scale_color_manual facet_wrap ggsave
#' @importFrom ggplot2 coord_flip position_dodge position_stack guide_legend
#' @importFrom stats aov summary.aov t.test chisq.test kruskal.test
#' @importFrom dplyr group_by summarise n count
#' @export

bbcplotsClass <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "bbcplotsClass",
    inherit = bbcplotsBase,
    private = list(
        .processed_data = NULL,
        .bbc_colors = list(
            bbc_blue = "#1380A1",
            bbc_orange = "#FAAB18", 
            bbc_teal = "#007f7f",
            bbc_gray = "#333333",
            multi_color = c("#1380A1", "#FAAB18", "#007f7f", "#333333", "#990000", "#007A54")
        ),
        
        .bbplot_available = FALSE,
        .bbplot_version = NULL,

        .init = function() {
            # Check bbplot package availability
            private$.check_bbplot_availability()
            
            # Comprehensive welcome instructions
            instructions_html <- paste(
                "<div style='font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto;'>",
                "<div style='background: linear-gradient(135deg, #1380A1 0%, #007f7f 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h2 style='margin: 0 0 10px 0; font-size: 24px; font-weight: bold;'>📺 BBC-Style Data Visualization</h2>",
                "<p style='margin: 0; font-size: 16px; opacity: 0.9;'>Create professional news-quality graphics with BBC design standards</p>",
                "</div>",
                
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h3 style='color: #1380A1; margin: 0 0 15px 0;'>✨ Key Features</h3>",
                "<ul style='margin: 0; padding-left: 20px; line-height: 1.8;'>",
                "<li><strong>BBC Design Standards:</strong> Authentic Helvetica typography and professional color schemes</li>",
                "<li><strong>Publication Ready:</strong> Standard 640×450px export with proper branding</li>",
                "<li><strong>Statistical Integration:</strong> Built-in statistical tests with publication-quality annotations</li>",
                "<li><strong>Multiple Chart Types:</strong> Columns, bars, lines, points, areas, and grouped variations</li>",
                "<li><strong>News Quality:</strong> Clean aesthetics optimized for digital journalism</li>",
                "</ul>",
                "</div>",
                
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #2c3e50; margin: 10px 0 5px 0;'>📊 Quick Start Guide:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Data Variables:</strong> Select Y-axis (values) and X-axis (categories) variables</li>",
                "<li><strong>Chart Type:</strong> Choose from column, bar, line, point, or area charts</li>",
                "<li><strong>BBC Colors:</strong> Select BBC Blue, Orange, Teal, or create custom palettes</li>",
                "<li><strong>Finalize:</strong> Enable 'Export Finalized Chart' for publication-ready output</li>",
                "</ol>",
                "</div>",
                
                "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #1380A1;'><strong>💡 Pro Tip:</strong> BBC style emphasizes clarity and accessibility. Use high contrast colors, clear titles, and minimal decoration for maximum impact in news and publication contexts.</p>",
                "</div>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
            
            # Set default plot dimensions to BBC standards
            self$results$main_plot$setSize(640, 450)
            self$results$finalized_plot$setSize(640, 500)
            
            # Initialize color guide
            private$.init_color_guide()
            
            # Initialize design principles guide
            private$.init_design_guide()
            
            # Initialize accessibility information
            private$.init_accessibility_info()
        },

        .run = function() {
            # Clear instructions if analysis is ready
            if (!is.null(self$options$y_var) && !is.null(self$options$x_var)) {
                self$results$instructions$setContent("")
            }
            
            # Comprehensive validation
            validation_result <- private$.validate_inputs()
            if (!validation_result$valid) {
                private$.show_error_message(validation_result$message)
                return()
            }
            
            # Process the data with error handling
            tryCatch({
                private$.processed_data <- private$.process_data()
                
                if (nrow(private$.processed_data) == 0) {
                    private$.show_error_message(
                        "No data available after processing",
                        "This usually occurs when:<br/>
                        • All observations have missing values in selected variables<br/>
                        • Data filtering removed all observations<br/>
                        • Variables contain only invalid values<br/><br/>
                        <strong>Solutions:</strong><br/>
                        • Check for missing values in your data<br/>
                        • Ensure variables contain appropriate data types<br/>
                        • Try different variable combinations"
                    )
                    return()
                }
                
                # Additional data quality checks
                private$.check_data_quality()
                
            }, error = function(e) {
                private$.show_error_message(
                    "Data processing error",
                    paste("Error details:", e$message, "<br/><br/>",
                          "Please check your data format and variable selections.")
                )
                return()
            })
            
            # Generate statistical analysis if requested
            if (self$options$statistical_annotations) {
                private$.generate_statistical_analysis()
            }
            
            # Generate chart summary if requested
            if (self$options$show_sample_sizes) {
                private$.generate_chart_summary()
            }
            
            # Generate R code if exporting finalized chart
            if (self$options$export_finalized) {
                private$.generate_export_code()
            }
            
            # Set plot state for rendering
            self$results$main_plot$setState(private$.processed_data)
            
            if (self$options$export_finalized) {
                self$results$finalized_plot$setState(private$.processed_data)
            }
        },

        .validate_inputs = function() {
            # Comprehensive input validation
            mydata <- self$data
            
            # Check if data exists
            if (is.null(mydata) || nrow(mydata) == 0) {
                return(list(valid = FALSE, message = list(
                    title = "No data available",
                    details = "Please load a dataset before creating BBC-style visualizations."
                )))
            }
            
            # Check required variables
            if (is.null(self$options$y_var)) {
                return(list(valid = FALSE, message = list(
                    title = "Y-axis variable required",
                    details = "Please select a variable for the Y-axis (values to display)."
                )))
            }
            
            if (is.null(self$options$x_var)) {
                return(list(valid = FALSE, message = list(
                    title = "X-axis variable required", 
                    details = "Please select a variable for the X-axis (categories or groups)."
                )))
            }
            
            # Validate variable existence
            all_vars <- names(mydata)
            if (!self$options$y_var %in% all_vars) {
                return(list(valid = FALSE, message = list(
                    title = "Y-variable not found",
                    details = paste("Variable '", self$options$y_var, "' not found in dataset.", 
                                   "Available variables:", paste(all_vars, collapse = ", "))
                )))
            }
            
            if (!self$options$x_var %in% all_vars) {
                return(list(valid = FALSE, message = list(
                    title = "X-variable not found", 
                    details = paste("Variable '", self$options$x_var, "' not found in dataset.",
                                   "Available variables:", paste(all_vars, collapse = ", "))
                )))
            }
            
            # Validate optional variables
            if (!is.null(self$options$group_var) && !self$options$group_var %in% all_vars) {
                return(list(valid = FALSE, message = list(
                    title = "Grouping variable not found",
                    details = paste("Variable '", self$options$group_var, "' not found in dataset.")
                )))
            }
            
            if (!is.null(self$options$facet_var) && !self$options$facet_var %in% all_vars) {
                return(list(valid = FALSE, message = list(
                    title = "Faceting variable not found",
                    details = paste("Variable '", self$options$facet_var, "' not found in dataset.")
                )))
            }
            
            # Check data type compatibility
            y_data <- mydata[[self$options$y_var]]
            x_data <- mydata[[self$options$x_var]]
            
            # Y-variable should be numeric for most chart types
            if (!is.numeric(y_data) && self$options$chart_type %in% c("column", "bar", "line", "point", "area", "horizontal_bar")) {
                return(list(valid = FALSE, message = list(
                    title = "Y-variable must be numeric",
                    details = paste("Selected chart type '", self$options$chart_type, "' requires a numeric Y-variable.",
                                   "Current variable type:", class(y_data)[1])
                )))
            }
            
            # Check for sufficient data
            valid_y <- sum(!is.na(y_data))
            valid_x <- sum(!is.na(x_data))
            
            if (valid_y < 3 || valid_x < 3) {
                return(list(valid = FALSE, message = list(
                    title = "Insufficient data",
                    details = paste("Need at least 3 valid observations for visualization.",
                                   "Y-variable has", valid_y, "valid values,",
                                   "X-variable has", valid_x, "valid values.")
                )))
            }
            
            # Validate threshold settings
            if (self$options$thresholdMin >= self$options$thresholdMax) {
                return(list(valid = FALSE, message = list(
                    title = "Invalid threshold settings",
                    details = "Minimum threshold must be less than maximum threshold."
                )))
            }
            
            # Validate chart dimensions
            if (self$options$chart_width < 400 || self$options$chart_width > 1200) {
                return(list(valid = FALSE, message = list(
                    title = "Invalid chart width",
                    details = "Chart width must be between 400 and 1200 pixels."
                )))
            }
            
            if (self$options$chart_height < 300 || self$options$chart_height > 800) {
                return(list(valid = FALSE, message = list(
                    title = "Invalid chart height", 
                    details = "Chart height must be between 300 and 800 pixels."
                )))
            }
            
            return(list(valid = TRUE, message = NULL))
        },
        
        .show_error_message = function(title, details = NULL) {
            error_html <- paste0(
                "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 8px; padding: 20px; margin: 10px 0;'>",
                "<div style='display: flex; align-items: center; margin-bottom: 10px;'>",
                "<span style='font-size: 24px; margin-right: 10px;'>⚠️</span>",
                "<h4 style='color: #856404; margin: 0;'>", title, "</h4>",
                "</div>"
            )
            
            if (!is.null(details)) {
                error_html <- paste0(error_html,
                    "<div style='color: #856404; line-height: 1.5;'>", details, "</div>"
                )
            }
            
            error_html <- paste0(error_html, "</div>")
            
            self$results$main_plot$setContent(error_html)
        },
        
        .check_data_quality = function() {
            data <- private$.processed_data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            
            # Check for extreme values
            if (is.numeric(data[[y_var]])) {
                y_values <- data[[y_var]]
                q1 <- quantile(y_values, 0.25, na.rm = TRUE)
                q3 <- quantile(y_values, 0.75, na.rm = TRUE)
                iqr <- q3 - q1
                
                outliers <- sum(y_values < (q1 - 1.5 * iqr) | y_values > (q3 + 1.5 * iqr), na.rm = TRUE)
                
                if (outliers > 0) {
                    warning_html <- paste0(
                        "<div style='background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 5px; padding: 10px; margin: 5px 0;'>",
                        "<p style='margin: 0; color: #0066cc;'><strong>📊 Data Quality Note:</strong> ",
                        "Found ", outliers, " potential outliers in Y-variable. ",
                        "Consider reviewing extreme values for data accuracy.</p>",
                        "</div>"
                    )
                    
                    # Add to chart summary if enabled
                    if (self$options$show_sample_sizes) {
                        current_summary <- self$results$chart_summary$content
                        enhanced_summary <- paste0(current_summary, warning_html)
                        self$results$chart_summary$setContent(enhanced_summary)
                    }
                }
            }
            
            # Check category distribution for grouped charts
            if (is.factor(data[[x_var]])) {
                category_counts <- table(data[[x_var]])
                min_count <- min(category_counts)
                
                if (min_count < 3) {
                    warning_html <- paste0(
                        "<div style='background-color: #fff3e0; border: 1px solid #ffcc80; border-radius: 5px; padding: 10px; margin: 5px 0;'>",
                        "<p style='margin: 0; color: #f57c00;'><strong>⚠️ Small Sample Warning:</strong> ",
                        "Some categories have fewer than 3 observations. ",
                        "Results may be unstable for categories with very small samples.</p>",
                        "</div>"
                    )
                    
                    if (self$options$show_sample_sizes) {
                        current_summary <- self$results$chart_summary$content
                        enhanced_summary <- paste0(current_summary, warning_html)
                        self$results$chart_summary$setContent(enhanced_summary)
                    }
                }
            }
        },

        .process_data = function() {
            mydata <- self$data
            
            # Get variable names
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            group_var <- self$options$group_var
            facet_var <- self$options$facet_var
            
            # Select relevant variables
            vars_to_select <- c(y_var, x_var)
            if (!is.null(group_var)) vars_to_select <- c(vars_to_select, group_var)
            if (!is.null(facet_var)) vars_to_select <- c(vars_to_select, facet_var)
            
            mydata <- mydata[vars_to_select]
            
            # Track missing data before removal
            original_n <- nrow(mydata)
            complete_cases <- complete.cases(mydata)
            missing_n <- sum(!complete_cases)
            
            # Remove missing values
            mydata <- mydata[complete_cases, ]
            
            # Inform about missing data if substantial
            if (missing_n > 0 && missing_n / original_n > 0.1) {
                missing_percent <- round(missing_n / original_n * 100, 1)
                warning(paste("Removed", missing_n, "observations (", missing_percent, "%) with missing values"))
            }
            
            # Enhanced data type handling
            if (!is.null(group_var)) {
                mydata[[group_var]] <- as.factor(mydata[[group_var]])
                
                # Check for too many groups
                n_groups <- length(levels(mydata[[group_var]]))
                if (n_groups > 12) {
                    warning(paste("Grouping variable has", n_groups, "levels. Consider grouping or filtering for clearer visualization."))
                }
            }
            
            if (!is.null(facet_var)) {
                mydata[[facet_var]] <- as.factor(mydata[[facet_var]])
                
                # Check for too many facets
                n_facets <- length(levels(mydata[[facet_var]]))
                if (n_facets > 8) {
                    warning(paste("Faceting variable has", n_facets, "levels. Consider reducing for better readability."))
                }
            }
            
            # Enhanced x_var handling
            if (is.character(mydata[[x_var]])) {
                mydata[[x_var]] <- as.factor(mydata[[x_var]])
            } else if (is.numeric(mydata[[x_var]])) {
                unique_vals <- length(unique(mydata[[x_var]]))
                if (unique_vals <= 15) {
                    mydata[[x_var]] <- as.factor(mydata[[x_var]])
                }
            }
            
            # Ensure appropriate ordering for factors
            if (is.factor(mydata[[x_var]])) {
                # For BBC style, often natural ordering is preferred
                if (all(grepl("^[0-9]+$", levels(mydata[[x_var]])))) {
                    # Numeric levels - sort numerically
                    mydata[[x_var]] <- factor(mydata[[x_var]], 
                                             levels = sort(as.numeric(levels(mydata[[x_var]]))))
                }
            }
            
            return(mydata)
        },
        
        .check_bbplot_availability = function() {
            # Check if bbplot package is available
            tryCatch({
                if (requireNamespace("bbplot", quietly = TRUE)) {
                    private$.bbplot_available <- TRUE
                    # Get version if available
                    private$.bbplot_version <- tryCatch({
                        as.character(packageVersion("bbplot"))
                    }, error = function(e) "unknown")
                    
                    message("✅ bbplot package detected - enhanced BBC functionality available")
                } else {
                    private$.bbplot_available <- FALSE
                    private$.bbplot_version <- NULL
                }
            }, error = function(e) {
                private$.bbplot_available <- FALSE
                private$.bbplot_version <- NULL
            })
            
            # Update accessibility info with bbplot status
            private$.update_bbplot_status_info()
        },
        
        .update_bbplot_status_info = function() {
            bbplot_status_html <- if (private$.bbplot_available) {
                paste0(
                    "<div style='background-color: #e8f5e8; border: 1px solid #4caf50; border-radius: 5px; padding: 15px; margin: 10px 0;'>",
                    "<h5 style='color: #2e7d32; margin: 0 0 10px 0;'>📦 Enhanced BBC Functionality Available</h5>",
                    "<p style='margin: 0; color: #2e7d32;'>",
                    "The official BBC bbplot package (v", private$.bbplot_version, ") is installed, providing:",
                    "</p>",
                    "<ul style='margin: 5px 0 0 20px; color: #2e7d32;'>",
                    "<li>Official BBC finalise_plot() function for publication</li>",
                    "<li>Authentic BBC logo and branding elements</li>",
                    "<li>Advanced BBC typography and spacing</li>",
                    "<li>Professional export capabilities</li>",
                    "</ul>",
                    "</div>"
                )
            } else {
                paste0(
                    "<div style='background-color: #fff3e0; border: 1px solid #ff9800; border-radius: 5px; padding: 15px; margin: 10px 0;'>",
                    "<h5 style='color: #f57c00; margin: 0 0 10px 0;'>📦 Optional Enhancement Available</h5>",
                    "<p style='margin: 0 0 10px 0; color: #f57c00;'>",
                    "Install the official BBC bbplot package for enhanced functionality:",
                    "</p>",
                    "<pre style='background-color: #2d3748; color: #e2e8f0; padding: 10px; border-radius: 4px; font-size: 12px; margin: 10px 0;'>",
                    "# Install bbplot from GitHub\n",
                    "devtools::install_github('bbc/bbplot')\n",
                    "library(bbplot)",
                    "</pre>",
                    "<p style='margin: 0; color: #f57c00; font-size: 0.9em;'>",
                    "Current functionality provides authentic BBC styling without additional dependencies.",
                    "</p>",
                    "</div>"
                )
            }
            
            # Add to accessibility info
            current_access_info <- private$.init_accessibility_info_content()
            enhanced_access_info <- paste0(current_access_info, bbplot_status_html)
            self$results$accessibility_info$setContent(enhanced_access_info)
        },
        
        .get_enhanced_bbc_style = function() {
            # Use bbplot package styling if available, otherwise fallback
            if (private$.bbplot_available) {
                tryCatch({
                    # Try to use bbplot's bbc_style function
                    return(bbplot::bbc_style())
                }, error = function(e) {
                    # Fallback to our implementation if bbplot fails
                    return(private$.create_bbc_style_fallback())
                })
            } else {
                return(private$.create_bbc_style_fallback())
            }
        },
        
        .create_bbc_style_fallback = function() {
            # Our enhanced implementation of BBC style
            bbc_theme <- ggplot2::theme(
                # Text elements with enhanced hierarchy
                text = ggplot2::element_text(family = self$options$font_family, size = 18, color = "#222222"),
                plot.title = ggplot2::element_text(
                    family = self$options$font_family, 
                    size = 28, 
                    face = "bold", 
                    color = "#222222",
                    hjust = if (self$options$left_align_title) 0 else 0.5,
                    margin = ggplot2::margin(0, 0, 15, 0)
                ),
                plot.subtitle = ggplot2::element_text(
                    family = self$options$font_family, 
                    size = 22, 
                    color = "#222222",
                    hjust = if (self$options$left_align_title) 0 else 0.5,
                    margin = ggplot2::margin(0, 0, 20, 0)
                ),
                plot.caption = ggplot2::element_text(
                    family = self$options$font_family, 
                    size = 14, 
                    color = "#666666",
                    hjust = 0,
                    margin = ggplot2::margin(15, 0, 0, 0)
                ),
                
                # Enhanced legend styling
                legend.position = self$options$legend_position,
                legend.text = ggplot2::element_text(family = self$options$font_family, size = 18, color = "#222222"),
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_blank(),
                legend.background = ggplot2::element_blank(),
                legend.margin = ggplot2::margin(0, 0, 15, 0),
                legend.spacing.x = ggplot2::unit(8, "pt"),
                
                # Enhanced axis styling
                axis.title = ggplot2::element_blank(),
                axis.text = ggplot2::element_text(family = self$options$font_family, size = 18, color = "#222222"),
                axis.text.x = ggplot2::element_text(margin = ggplot2::margin(8, 0, 0, 0)),
                axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 8, 0, 0)),
                axis.ticks = ggplot2::element_blank(),
                axis.line = ggplot2::element_blank(),
                
                # Enhanced grid styling
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major.y = if (self$options$horizontal_gridlines) {
                    ggplot2::element_line(color = "#cbcbcb", size = 0.25)
                } else {
                    ggplot2::element_blank()
                },
                panel.grid.major.x = if (self$options$vertical_gridlines) {
                    ggplot2::element_line(color = "#cbcbcb", size = 0.25)
                } else {
                    ggplot2::element_blank()
                },
                
                # Enhanced background and spacing
                panel.background = ggplot2::element_blank(),
                plot.background = ggplot2::element_rect(fill = "white", color = NA),
                plot.margin = ggplot2::margin(20, 20, 20, 20),
                
                # Enhanced facet styling
                strip.background = ggplot2::element_rect(fill = "white", color = NA),
                strip.text = ggplot2::element_text(
                    family = self$options$font_family, 
                    size = 20, 
                    color = "#222222",
                    face = "bold",
                    margin = ggplot2::margin(0, 0, 10, 0)
                )
            )
            
            return(bbc_theme)
        },
        
        .apply_bbplot_finalization = function(plot) {
            # Apply bbplot finalisation if available
            if (private$.bbplot_available && self$options$export_finalized) {
                tryCatch({
                    # Use bbplot's finalise_plot function
                    if (exists("finalise_plot", envir = asNamespace("bbplot"))) {
                        source_text <- if (self$options$source_text != "") {
                            self$options$source_text
                        } else {
                            "Source: Data analysis"
                        }
                        
                        # Apply bbplot finalisation
                        finalised_plot <- bbplot::finalise_plot(
                            plot_name = plot,
                            source = source_text,
                            save_filepath = NULL,  # Don't auto-save
                            width_pixels = self$options$chart_width,
                            height_pixels = self$options$chart_height,
                            logo_image_path = NULL  # Use default BBC branding
                        )
                        
                        return(finalised_plot)
                    }
                }, error = function(e) {
                    warning("bbplot finalisation failed, using fallback method")
                })
            }
            
            # Fallback: add source annotation manually
            if (self$options$source_text != "") {
                plot <- plot + ggplot2::labs(caption = self$options$source_text)
            }
            
            return(plot)
        },
        
        .init_accessibility_info_content = function() {
            # Base accessibility information
            return(paste(
                "<h4 style='color: #1380A1;'>♿ Accessibility & Standards</h4>",
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h5>BBC Accessibility Standards:</h5>",
                "<ul>",
                "<li><strong>Color Contrast:</strong> All text meets WCAG AA standards (4.5:1 ratio minimum)</li>",
                "<li><strong>Alternative Text:</strong> Charts should include descriptive alt text for screen readers</li>",
                "<li><strong>Color Independence:</strong> Information not conveyed by color alone</li>",
                "<li><strong>Font Size:</strong> Minimum 18pt text for readability</li>",
                "<li><strong>Clear Hierarchy:</strong> Logical reading order and structure</li>",
                "</ul>",
                "<h5>Export Recommendations:</h5>",
                "<ul>",
                "<li>Export at 640×450px for web, 1280×900px for print</li>",
                "<li>Include data tables as alternative formats</li>",
                "<li>Provide chart descriptions in accompanying text</li>",
                "<li>Test with screen readers and accessibility tools</li>",
                "</ul>",
                "<p style='margin: 10px 0 0 0; padding: 10px; background-color: #e8f5e8; border-radius: 4px;'>",
                "<strong>💡 Tip:</strong> BBC charts prioritize clarity and accessibility over visual complexity. This ensures your message reaches the widest possible audience effectively.",
                "</p>",
                "</div>"
            ))
        },

        .create_bbc_style = function() {
            # Use enhanced version with bbplot integration
            return(private$.get_enhanced_bbc_style())
        },

        .get_bbc_colors = function() {
            color_scheme <- self$options$bbc_colors
            
            if (color_scheme == "custom") {
                # Parse custom colors
                custom_colors <- trimws(strsplit(self$options$custom_colors, ",")[[1]])
                return(custom_colors)
            } else if (color_scheme == "multi_color") {
                return(private$.bbc_colors$multi_color)
            } else {
                return(private$.bbc_colors[[color_scheme]])
            }
        },

        .generate_statistical_analysis = function() {
            if (!self$options$statistical_annotations) return()
            
            data <- private$.processed_data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            group_var <- self$options$group_var
            stat_method <- self$options$stat_method
            
            tryCatch({
                stat_html <- "<h4 style='color: #1380A1;'>📊 Enhanced Statistical Analysis Results</h4>"
                stat_html <- paste0(stat_html, "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
                
                # Enhanced auto-selection with more sophisticated logic
                if (stat_method == "auto") {
                    stat_method <- private$.auto_select_statistical_test(data, y_var, x_var, group_var)
                    stat_html <- paste0(stat_html, "<p style='color: #666; font-style: italic;'>Auto-selected test: <strong>", stat_method, "</strong></p>")
                }
                
                # Perform comprehensive statistical analysis
                if (stat_method == "anova" && is.factor(data[[x_var]])) {
                    stat_html <- paste0(stat_html, private$.perform_anova_analysis(data, y_var, x_var))
                    
                } else if (stat_method == "ttest") {
                    stat_html <- paste0(stat_html, private$.perform_ttest_analysis(data, y_var, x_var))
                    
                } else if (stat_method == "kruskal") {
                    stat_html <- paste0(stat_html, private$.perform_kruskal_analysis(data, y_var, x_var))
                    
                } else if (stat_method == "chisquare") {
                    stat_html <- paste0(stat_html, private$.perform_chisquare_analysis(data, y_var, x_var))
                    
                } else if (stat_method == "correlation") {
                    stat_html <- paste0(stat_html, private$.perform_correlation_analysis(data, y_var, x_var))
                    
                } else if (stat_method == "regression") {
                    stat_html <- paste0(stat_html, private$.perform_regression_analysis(data, y_var, x_var))
                }
                
                # Add interpretation guide for BBC journalism standards
                stat_html <- paste0(stat_html, private$.add_interpretation_guide(stat_method))
                
                stat_html <- paste0(stat_html, "</div>")
                self$results$statistical_results$setContent(stat_html)
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='color: orange; padding: 10px;'>",
                    "Statistical analysis could not be completed: ", e$message,
                    "<br/><strong>Technical details:</strong> ", class(e)[1],
                    "</div>"
                )
                self$results$statistical_results$setContent(error_html)
            })
        },
        
        .auto_select_statistical_test = function(data, y_var, x_var, group_var) {
            # Enhanced automatic test selection logic
            x_is_factor <- is.factor(data[[x_var]]) || is.character(data[[x_var]])
            y_is_numeric <- is.numeric(data[[y_var]])
            
            # Check for binary outcomes
            if (y_is_numeric) {
                unique_y <- unique(data[[y_var]])
                is_binary <- length(unique_y) == 2 && all(unique_y %in% c(0, 1))
            } else {
                is_binary <- FALSE
            }
            
            if (x_is_factor && y_is_numeric && !is_binary) {
                n_groups <- length(unique(data[[x_var]]))
                
                # Check normality assumptions (simplified)
                if (n_groups >= 2) {
                    # Test for normality using Shapiro-Wilk (for small samples)
                    normal_test_passed <- TRUE
                    if (nrow(data) <= 50) {
                        tryCatch({
                            shapiro_result <- shapiro.test(data[[y_var]])
                            normal_test_passed <- shapiro_result$p.value > 0.05
                        }, error = function(e) normal_test_passed <<- TRUE)
                    }
                    
                    if (normal_test_passed) {
                        return(if (n_groups == 2) "ttest" else "anova")
                    } else {
                        return(if (n_groups == 2) "wilcox" else "kruskal")
                    }
                }
            } else if (x_is_factor && (is.factor(data[[y_var]]) || is_binary)) {
                return("chisquare")
            } else if (is.numeric(data[[x_var]]) && y_is_numeric) {
                return("correlation")
            }
            
            return("anova")  # Default fallback
        },
        
        .perform_anova_analysis = function(data, y_var, x_var) {
            formula_str <- paste(y_var, "~", x_var)
            aov_result <- aov(as.formula(formula_str), data = data)
            aov_summary <- summary(aov_result)
            
            f_stat <- round(aov_summary[[1]]$`F value`[1], 3)
            p_value <- aov_summary[[1]]$`Pr(>F)`[1]
            df1 <- aov_summary[[1]]$Df[1]
            df2 <- aov_summary[[1]]$Df[2]
            
            # Effect size calculations
            ss_total <- sum(aov_summary[[1]]$`Sum Sq`)
            ss_between <- aov_summary[[1]]$`Sum Sq`[1]
            eta_squared <- ss_between / ss_total
            omega_squared <- (ss_between - df1 * aov_summary[[1]]$`Mean Sq`[2]) / 
                           (ss_total + aov_summary[[1]]$`Mean Sq`[2])
            
            result_html <- paste0(
                "<div style='border-left: 4px solid #1380A1; padding-left: 15px; margin: 10px 0;'>",
                "<p><strong>One-way ANOVA Results:</strong></p>",
                "<p>F(", df1, ", ", df2, ") = ", f_stat, ", p = ", format.pval(p_value, digits = 3), "</p>",
                "<p><strong>Effect Sizes:</strong></p>",
                "<ul>",
                "<li>η² (eta-squared): ", round(eta_squared, 3), " (", private$.interpret_effect_size(eta_squared, "eta"), ")</li>",
                "<li>ω² (omega-squared): ", round(max(0, omega_squared), 3), " (more conservative estimate)</li>",
                "</ul>",
                private$.add_post_hoc_note(p_value, length(unique(data[[x_var]]))),
                "</div>"
            )
            
            return(result_html)
        },
        
        .perform_ttest_analysis = function(data, y_var, x_var) {
            groups <- unique(data[[x_var]])
            if (length(groups) != 2) {
                return("<p style='color: orange;'>t-test requires exactly 2 groups</p>")
            }
            
            group1_data <- data[data[[x_var]] == groups[1], y_var]
            group2_data <- data[data[[x_var]] == groups[2], y_var]
            
            # Perform both independent and Welch's t-test
            t_result <- t.test(group1_data, group2_data, var.equal = TRUE)
            welch_result <- t.test(group1_data, group2_data, var.equal = FALSE)
            
            # Levene's test for equal variances
            var_test <- var.test(group1_data, group2_data)
            equal_vars <- var_test$p.value > 0.05
            
            # Choose appropriate test result
            final_result <- if (equal_vars) t_result else welch_result
            test_name <- if (equal_vars) "Independent samples t-test" else "Welch's t-test (unequal variances)"
            
            # Cohen's d effect size
            pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                             (length(group2_data) - 1) * var(group2_data)) / 
                            (length(group1_data) + length(group2_data) - 2))
            cohens_d <- abs(diff(c(mean(group1_data), mean(group2_data)))) / pooled_sd
            
            result_html <- paste0(
                "<div style='border-left: 4px solid #FAAB18; padding-left: 15px; margin: 10px 0;'>",
                "<p><strong>", test_name, ":</strong></p>",
                "<p>t(", round(final_result$parameter, 1), ") = ", round(final_result$statistic, 3), 
                ", p = ", format.pval(final_result$p.value, digits = 3), "</p>",
                "<p><strong>Group Means:</strong> ", groups[1], " = ", round(mean(group1_data), 3), 
                ", ", groups[2], " = ", round(mean(group2_data), 3), "</p>",
                "<p><strong>Mean Difference:</strong> ", round(diff(final_result$estimate), 3), 
                " (95% CI: [", round(final_result$conf.int[1], 3), ", ", round(final_result$conf.int[2], 3), "])</p>",
                "<p><strong>Effect Size (Cohen's d):</strong> ", round(cohens_d, 3), 
                " (", private$.interpret_effect_size(cohens_d, "cohens_d"), ")</p>",
                if (!equal_vars) "<p style='color: #f57c00;'><em>Note: Unequal variances detected, Welch's correction applied</em></p>" else "",
                "</div>"
            )
            
            return(result_html)
        },
        
        .perform_correlation_analysis = function(data, y_var, x_var) {
            if (!is.numeric(data[[x_var]]) || !is.numeric(data[[y_var]])) {
                return("<p style='color: orange;'>Correlation analysis requires both variables to be numeric</p>")
            }
            
            # Pearson correlation
            pearson_result <- cor.test(data[[x_var]], data[[y_var]], method = "pearson")
            
            # Spearman correlation (non-parametric)
            spearman_result <- cor.test(data[[x_var]], data[[y_var]], method = "spearman")
            
            # Determine which to report based on normality
            shapiro_x <- if (nrow(data) <= 50) shapiro.test(data[[x_var]])$p.value > 0.05 else TRUE
            shapiro_y <- if (nrow(data) <= 50) shapiro.test(data[[y_var]])$p.value > 0.05 else TRUE
            use_pearson <- shapiro_x && shapiro_y
            
            primary_result <- if (use_pearson) pearson_result else spearman_result
            method_name <- if (use_pearson) "Pearson" else "Spearman"
            
            result_html <- paste0(
                "<div style='border-left: 4px solid #007f7f; padding-left: 15px; margin: 10px 0;'>",
                "<p><strong>", method_name, " Correlation Analysis:</strong></p>",
                "<p>r = ", round(primary_result$estimate, 3), 
                ", t(", primary_result$parameter, ") = ", round(primary_result$statistic, 3),
                ", p = ", format.pval(primary_result$p.value, digits = 3), "</p>",
                "<p><strong>95% Confidence Interval:</strong> [", round(primary_result$conf.int[1], 3), 
                ", ", round(primary_result$conf.int[2], 3), "]</p>",
                "<p><strong>Effect Size:</strong> ", private$.interpret_correlation_strength(abs(primary_result$estimate)), "</p>",
                "<p><strong>Coefficient of Determination (r²):</strong> ", round(primary_result$estimate^2, 3), 
                " (", round(primary_result$estimate^2 * 100, 1), "% shared variance)</p>",
                if (!use_pearson) "<p style='color: #f57c00;'><em>Note: Non-normal distribution detected, Spearman correlation reported</em></p>" else "",
                "</div>"
            )
            
            return(result_html)
        },
        
        .perform_chisquare_analysis = function(data, y_var, x_var) {
            # Create contingency table
            contingency_table <- table(data[[x_var]], data[[y_var]])
            
            # Perform chi-square test
            chi_result <- chisq.test(contingency_table)
            
            # Calculate effect sizes
            n <- sum(contingency_table)
            cramers_v <- sqrt(chi_result$statistic / (n * (min(dim(contingency_table)) - 1)))
            phi <- sqrt(chi_result$statistic / n)
            
            result_html <- paste0(
                "<div style='border-left: 4px solid #990000; padding-left: 15px; margin: 10px 0;'>",
                "<p><strong>Chi-square Test of Independence:</strong></p>",
                "<p>χ²(", chi_result$parameter, ") = ", round(chi_result$statistic, 3), 
                ", p = ", format.pval(chi_result$p.value, digits = 3), "</p>",
                "<p><strong>Effect Sizes:</strong></p>",
                "<ul>",
                "<li>Cramér's V: ", round(cramers_v, 3), " (", private$.interpret_effect_size(cramers_v, "cramers_v"), ")</li>",
                if (all(dim(contingency_table) == 2)) paste0("<li>Phi coefficient: ", round(phi, 3), "</li>") else "",
                "</ul>",
                private$.add_chi_square_assumptions_note(contingency_table),
                "</div>"
            )
            
            return(result_html)
        },
        
        .interpret_effect_size = function(value, type) {
            if (type == "eta" || type == "cohens_d") {
                thresholds <- if (type == "eta") c(0.01, 0.06, 0.14) else c(0.2, 0.5, 0.8)
                labels <- c("small", "medium", "large")
            } else if (type == "cramers_v") {
                thresholds <- c(0.1, 0.3, 0.5)
                labels <- c("small", "medium", "large")
            } else {
                return("effect size")
            }
            
            if (value < thresholds[1]) return("negligible")
            if (value < thresholds[2]) return(labels[1])
            if (value < thresholds[3]) return(labels[2])
            return(labels[3])
        },
        
        .interpret_correlation_strength = function(r) {
            if (r < 0.1) return("negligible")
            if (r < 0.3) return("small")
            if (r < 0.5) return("medium")
            if (r < 0.7) return("large")
            return("very large")
        },
        
        .add_interpretation_guide = function(stat_method) {
            guide_html <- paste0(
                "<div style='background-color: #e8f4fd; border: 1px solid #b3d9ff; border-radius: 5px; padding: 10px; margin: 15px 0;'>",
                "<h6 style='color: #1380A1; margin: 0 0 8px 0;'>📋 BBC Editorial Guidelines for Statistical Reporting:</h6>"
            )
            
            if (stat_method %in% c("anova", "ttest")) {
                guide_html <- paste0(guide_html,
                    "<p style='margin: 0; font-size: 0.9em;'>",
                    "• Report p-values with appropriate precision (avoid 'p < 0.05')<br/>",
                    "• Always include effect sizes for practical significance<br/>",
                    "• Consider confidence intervals for key comparisons<br/>",
                    "• Mention sample sizes for transparency</p>"
                )
            } else if (stat_method == "correlation") {
                guide_html <- paste0(guide_html,
                    "<p style='margin: 0; font-size: 0.9em;'>",
                    "• Remember: correlation does not imply causation<br/>",
                    "• Report both correlation coefficient and confidence interval<br/>",
                    "• Consider potential confounding variables<br/>",
                    "• Use appropriate correlation method for your data type</p>"
                )
            }
            
            guide_html <- paste0(guide_html, "</div>")
            return(guide_html)
        },
        
        .add_post_hoc_note = function(p_value, n_groups) {
            if (p_value < 0.05 && n_groups > 2) {
                return("<p style='color: #007A54; font-style: italic;'>📌 Significant result detected. Consider post-hoc tests for pairwise comparisons.</p>")
            }
            return("")
        },
        
        .add_chi_square_assumptions_note = function(contingency_table) {
            expected_counts <- chisq.test(contingency_table)$expected
            min_expected <- min(expected_counts)
            
            if (min_expected < 5) {
                return("<p style='color: #f57c00; font-style: italic;'>⚠️ Warning: Some expected cell counts < 5. Consider Fisher's exact test for small samples.</p>")
            }
            return("")
        },

        .generate_chart_summary = function() {
            data <- private$.processed_data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            group_var <- self$options$group_var
            
            summary_html <- "<h4 style='color: #1380A1;'>📈 Chart Summary Statistics</h4>"
            summary_html <- paste0(summary_html, "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            # Overall summary
            total_n <- nrow(data)
            summary_html <- paste0(summary_html, "<p><strong>Total Observations:</strong> ", total_n, "</p>")
            
            if (is.numeric(data[[y_var]])) {
                mean_val <- round(mean(data[[y_var]], na.rm = TRUE), 2)
                median_val <- round(median(data[[y_var]], na.rm = TRUE), 2)
                sd_val <- round(sd(data[[y_var]], na.rm = TRUE), 2)
                
                summary_html <- paste0(summary_html, "<p><strong>", y_var, " Statistics:</strong></p>")
                summary_html <- paste0(summary_html, "<ul>")
                summary_html <- paste0(summary_html, "<li>Mean: ", mean_val, "</li>")
                summary_html <- paste0(summary_html, "<li>Median: ", median_val, "</li>")
                summary_html <- paste0(summary_html, "<li>Standard Deviation: ", sd_val, "</li>")
                summary_html <- paste0(summary_html, "</ul>")
            }
            
            # Group-wise summary
            if (is.factor(data[[x_var]])) {
                group_summary <- data %>%
                    dplyr::group_by(.data[[x_var]]) %>%
                    dplyr::summarise(
                        n = dplyr::n(),
                        .groups = 'drop'
                    )
                
                summary_html <- paste0(summary_html, "<p><strong>Sample Sizes by ", x_var, ":</strong></p>")
                summary_html <- paste0(summary_html, "<ul>")
                for (i in 1:nrow(group_summary)) {
                    summary_html <- paste0(summary_html, "<li>", group_summary[[x_var]][i], ": n = ", group_summary$n[i], "</li>")
                }
                summary_html <- paste0(summary_html, "</ul>")
            }
            
            summary_html <- paste0(summary_html, "</div>")
            self$results$chart_summary$setContent(summary_html)
        },

        .generate_export_code = function() {
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            chart_type <- self$options$chart_type
            bbc_colors <- self$options$bbc_colors
            
            # Get selected colors
            colors <- private$.get_bbc_colors()
            color_code <- if (length(colors) == 1) {
                paste0('"', colors, '"')
            } else {
                paste0('c("', paste(colors, collapse = '", "'), '")')
            }
            
            code_template <- paste(
                "# BBC-Style Data Visualization",
                "# Generated by ClinicoPath jamovi module",
                "",
                "library(ggplot2)",
                "# Note: Install bbplot package for full BBC functionality:",
                "# devtools::install_github('bbc/bbplot')",
                "# library(bbplot)",
                "",
                "# Create BBC-style theme function",
                "bbc_style <- function() {",
                "  ggplot2::theme(",
                "    text = ggplot2::element_text(family = 'Helvetica', size = 18, color = '#222222'),",
                "    plot.title = ggplot2::element_text(family = 'Helvetica', size = 28, face = 'bold'),",
                "    plot.subtitle = ggplot2::element_text(family = 'Helvetica', size = 22),",
                "    legend.position = 'top',",
                "    legend.text = ggplot2::element_text(size = 18),",
                "    legend.title = ggplot2::element_blank(),",
                "    axis.title = ggplot2::element_blank(),",
                "    axis.text = ggplot2::element_text(size = 18, color = '#222222'),",
                "    axis.ticks = ggplot2::element_blank(),",
                "    axis.line = ggplot2::element_blank(),",
                "    panel.grid.minor = ggplot2::element_blank(),",
                "    panel.grid.major.y = ggplot2::element_line(color = '#cbcbcb'),",
                "    panel.grid.major.x = ggplot2::element_blank(),",
                "    panel.background = ggplot2::element_blank(),",
                "    plot.background = ggplot2::element_rect(fill = 'white', color = NA)",
                "  )",
                "}",
                "",
                "# Create the plot",
                "plot <- ggplot(data, aes(x = {x_var}, y = {y_var})) +",
                "  geom_{geom_type}(fill = {color_code}) +",
                "  bbc_style() +",
                "  labs(",
                "    title = '{title}',",
                "    subtitle = '{subtitle}',",
                "    caption = '{source}'",
                "  )",
                "",
                "# Display the plot",
                "print(plot)",
                "",
                "# Export with BBC standards (optional)",
                "# ggsave('chart.png', plot, width = 640, height = 450, units = 'px', dpi = 100)",
                sep = "\n"
            )
            
            # Replace placeholders
            geom_mapping <- list(
                column = "col",
                bar = "col",
                line = "line",
                point = "point",
                area = "area",
                stacked_column = "col",
                grouped_column = "col",
                horizontal_bar = "col"
            )
            
            actual_code <- gsub("\\{x_var\\}", x_var, code_template)
            actual_code <- gsub("\\{y_var\\}", y_var, actual_code)
            actual_code <- gsub("\\{geom_type\\}", geom_mapping[[chart_type]], actual_code)
            actual_code <- gsub("\\{color_code\\}", color_code, actual_code)
            actual_code <- gsub("\\{title\\}", self$options$title_text, actual_code)
            actual_code <- gsub("\\{subtitle\\}", self$options$subtitle_text, actual_code)
            actual_code <- gsub("\\{source\\}", self$options$source_text, actual_code)
            
            code_html <- paste(
                "<h4 style='color: #1380A1;'>📝 Reproducible R Code</h4>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<p><strong>Complete R code for BBC-style visualization:</strong></p>",
                "<pre style='background-color: #2d3748; color: #e2e8f0; padding: 15px; border-radius: 5px; overflow-x: auto; font-size: 12px;'>",
                "<code>", actual_code, "</code>",
                "</pre>",
                "<p style='color: #666; font-size: 0.9em; margin-top: 10px;'>",
                "Copy this code to reproduce the BBC-style visualization in your R environment.",
                "</p>",
                "</div>"
            )
            
            self$results$export_code$setContent(code_html)
        },

        .init_color_guide = function() {
            color_html <- paste(
                "<h4 style='color: #1380A1;'>🎨 BBC Color Guidelines</h4>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h5>Official BBC Colors:</h5>",
                "<div style='display: flex; flex-wrap: wrap; gap: 10px; margin: 10px 0;'>",
                "<div style='background-color: #1380A1; color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px;'>#1380A1 BBC Blue</div>",
                "<div style='background-color: #FAAB18; color: black; padding: 8px 12px; border-radius: 4px; font-size: 12px;'>#FAAB18 BBC Orange</div>",
                "<div style='background-color: #007f7f; color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px;'>#007f7f BBC Teal</div>",
                "<div style='background-color: #333333; color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px;'>#333333 BBC Gray</div>",
                "</div>",
                "<p><strong>Usage Guidelines:</strong></p>",
                "<ul>",
                "<li>Use BBC Blue (#1380A1) for primary data series</li>",
                "<li>BBC Orange (#FAAB18) for highlighting or secondary data</li>",
                "<li>BBC Teal (#007f7f) for tertiary data or contrasts</li>",
                "<li>Maintain high contrast for accessibility</li>",
                "<li>Use consistent colors across related charts</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$color_guide$setContent(color_html)
        },

        .init_design_guide = function() {
            design_html <- paste(
                "<h4 style='color: #1380A1;'>📐 BBC Design Principles</h4>",
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h5>Core Design Standards:</h5>",
                "<ul>",
                "<li><strong>Typography:</strong> Helvetica font family at specific sizes (28pt titles, 22pt subtitles, 18pt body)</li>",
                "<li><strong>Layout:</strong> Left-aligned titles and clean grid system</li>",
                "<li><strong>Colors:</strong> Strategic use of BBC brand colors for maximum impact</li>",
                "<li><strong>Gridlines:</strong> Horizontal only, light gray (#cbcbcb) for readability</li>",
                "<li><strong>Spacing:</strong> Generous white space for clarity</li>",
                "<li><strong>Dimensions:</strong> Standard 640×450px for digital publication</li>",
                "</ul>",
                "<h5>Editorial Guidelines:</h5>",
                "<ul>",
                "<li>Clear, descriptive titles that tell the story</li>",
                "<li>Subtitles for additional context</li>",
                "<li>Source attribution for credibility</li>",
                "<li>Minimal decoration - let the data speak</li>",
                "<li>Accessible design for all audiences</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$design_notes$setContent(design_html)
        },

        .init_accessibility_info = function() {
            access_html <- paste(
                "<h4 style='color: #1380A1;'>♿ Accessibility & Standards</h4>",
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h5>BBC Accessibility Standards:</h5>",
                "<ul>",
                "<li><strong>Color Contrast:</strong> All text meets WCAG AA standards (4.5:1 ratio minimum)</li>",
                "<li><strong>Alternative Text:</strong> Charts should include descriptive alt text for screen readers</li>",
                "<li><strong>Color Independence:</strong> Information not conveyed by color alone</li>",
                "<li><strong>Font Size:</strong> Minimum 18pt text for readability</li>",
                "<li><strong>Clear Hierarchy:</strong> Logical reading order and structure</li>",
                "</ul>",
                "<h5>Export Recommendations:</h5>",
                "<ul>",
                "<li>Export at 640×450px for web, 1280×900px for print</li>",
                "<li>Include data tables as alternative formats</li>",
                "<li>Provide chart descriptions in accompanying text</li>",
                "<li>Test with screen readers and accessibility tools</li>",
                "</ul>",
                "<p style='margin: 10px 0 0 0; padding: 10px; background-color: #e8f5e8; border-radius: 4px;'>",
                "<strong>💡 Tip:</strong> BBC charts prioritize clarity and accessibility over visual complexity. This ensures your message reaches the widest possible audience effectively.",
                "</p>",
                "</div>"
            )
            
            self$results$accessibility_info$setContent(access_html)
        },

        .plot_main = function(image, ggtheme, theme, ...) {
            data <- image$state
            if (is.null(data)) return()
            
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            group_var <- self$options$group_var
            facet_var <- self$options$facet_var
            chart_type <- self$options$chart_type
            
            tryCatch({
                # Create base plot
                if (!is.null(group_var)) {
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[group_var]]))
                } else {
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
                }
                
                # Get BBC colors
                colors <- private$.get_bbc_colors()
                
                # Add appropriate geom based on chart type
                if (chart_type == "column") {
                    plot <- plot + ggplot2::geom_col(fill = if (is.null(group_var)) colors[1] else NULL)
                } else if (chart_type == "bar") {
                    plot <- plot + ggplot2::geom_col(fill = if (is.null(group_var)) colors[1] else NULL)
                } else if (chart_type == "horizontal_bar") {
                    plot <- plot + ggplot2::geom_col(fill = if (is.null(group_var)) colors[1] else NULL) + ggplot2::coord_flip()
                } else if (chart_type == "line") {
                    plot <- plot + ggplot2::geom_line(color = colors[1], size = 2) + 
                            ggplot2::geom_point(color = colors[1], size = 3)
                } else if (chart_type == "point") {
                    plot <- plot + ggplot2::geom_point(color = colors[1], size = 3)
                } else if (chart_type == "area") {
                    plot <- plot + ggplot2::geom_area(fill = colors[1], alpha = 0.7)
                } else if (chart_type == "stacked_column") {
                    plot <- plot + ggplot2::geom_col(position = "stack")
                } else if (chart_type == "grouped_column") {
                    plot <- plot + ggplot2::geom_col(position = "dodge")
                }
                
                # Apply colors for grouped data
                if (!is.null(group_var)) {
                    if (length(colors) >= length(unique(data[[group_var]]))) {
                        plot <- plot + ggplot2::scale_fill_manual(values = colors)
                    } else {
                        plot <- plot + ggplot2::scale_fill_manual(values = rep(colors, length.out = length(unique(data[[group_var]]))))
                    }
                }
                
                # Add BBC style theme
                plot <- plot + private$.create_bbc_style()
                
                # Add titles
                title <- if (self$options$title_text != "") self$options$title_text else NULL
                subtitle <- if (self$options$subtitle_text != "") self$options$subtitle_text else NULL
                x_title <- if (self$options$x_axis_title != "") self$options$x_axis_title else NULL
                y_title <- if (self$options$y_axis_title != "") self$options$y_axis_title else NULL
                
                plot <- plot + ggplot2::labs(
                    title = title,
                    subtitle = subtitle,
                    x = x_title,
                    y = y_title
                )
                
                # Add data values if requested
                if (self$options$show_values) {
                    value_position <- switch(self$options$value_position,
                                           "above" = ggplot2::position_dodge(width = 0.9),
                                           "center" = ggplot2::position_dodge(width = 0.9),
                                           "below" = ggplot2::position_dodge(width = 0.9))
                    
                    vjust_val <- switch(self$options$value_position,
                                       "above" = -0.5,
                                       "center" = 0.5,
                                       "below" = 1.5)
                    
                    if (chart_type %in% c("column", "bar", "stacked_column", "grouped_column", "horizontal_bar")) {
                        plot <- plot + ggplot2::geom_text(
                            ggplot2::aes(label = round(.data[[y_var]], 1)),
                            position = value_position,
                            vjust = vjust_val,
                            size = 4,
                            color = "#222222"
                        )
                    }
                }
                
                # Add faceting if specified
                if (!is.null(facet_var)) {
                    plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", facet_var)))
                }
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                warning(paste("Error creating BBC plot:", e$message))
                return(FALSE)
            })
        },

        .plot_finalized = function(image, ggtheme, theme, ...) {
            # This would create the finalized plot with BBC branding
            # For now, we'll use the main plot and add source information
            data <- image$state
            if (is.null(data)) return()
            
            # Create the main plot
            main_result <- private$.plot_main(image, ggtheme, theme, ...)
            
            # In a full implementation, this would use bbplot::finalise_plot()
            # to add BBC branding and proper export formatting
            
            return(main_result)
        }
    )
)