
#' @title Complex UpSet Plot Visualization
#' @importFrom jmvcore .
#' @export

jcomplexupsetClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jcomplexupsetClass",
    inherit = jcomplexupsetBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$set_vars) || length(self$options$set_vars) < 2) {
                self$results$instructions$setVisible(visible = TRUE)
                self$results$plot$setVisible(visible = FALSE)
                self$results$statistics$setVisible(visible = FALSE)
                self$results$interpretation$setVisible(visible = FALSE)
                return()
            }
            
            private$.checkData()
            
            self$results$instructions$setVisible(visible = FALSE)
            self$results$plot$setVisible(visible = TRUE)
            self$results$statistics$setVisible(visible = self$options$show_statistics)
            self$results$interpretation$setVisible(visible = self$options$show_interpretation)
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$set_vars) || length(self$options$set_vars) < 2) {
                return()
            }
            
            # Check if ComplexUpset is available
            if (!requireNamespace("ComplexUpset", quietly = TRUE)) {
                stop("The ComplexUpset package is required but not installed.")
            }
            
            # Prepare data
            data <- private$.prepareData()
            
            # Create upset plot
            plot <- private$.createUpsetPlot(data)
            
            # Set the plot
            self$results$plot$setState(plot)
            
            # Generate statistics
            if (self$options$show_statistics) {
                stats <- private$.generateStatistics(data)
                self$results$statistics$setContent(stats)
            }
            
            # Generate interpretation
            if (self$options$show_interpretation) {
                interpretation <- private$.generateInterpretation(data)
                self$results$interpretation$setContent(interpretation)
            }
        },
        
        .prepareData = function() {
            data <- self$data
            set_vars <- self$options$set_vars
            
            # Create working data frame with set variables
            plot_data <- data[, set_vars, drop = FALSE]
            
            # Convert to logical/binary if needed
            for (var in set_vars) {
                if (is.factor(plot_data[[var]]) || is.character(plot_data[[var]])) {
                    # Convert to binary based on non-missing, non-empty values
                    plot_data[[var]] <- !is.na(plot_data[[var]]) & 
                                       plot_data[[var]] != "" & 
                                       plot_data[[var]] != "0" &
                                       plot_data[[var]] != "FALSE"
                } else if (is.numeric(plot_data[[var]])) {
                    # Convert numeric to logical (non-zero = TRUE)
                    plot_data[[var]] <- !is.na(plot_data[[var]]) & plot_data[[var]] != 0
                }
            }
            
            # Add value variable if specified
            if (!is.null(self$options$value_var)) {
                plot_data$value_weight <- data[[self$options$value_var]]
            }
            
            # Remove rows with all FALSE values
            has_membership <- rowSums(plot_data[, set_vars, drop = FALSE]) > 0
            plot_data <- plot_data[has_membership, , drop = FALSE]
            
            return(plot_data)
        },
        
        .createUpsetPlot = function(data) {
            set_vars <- self$options$set_vars
            
            # Create base upset plot
            p <- ComplexUpset::upset(
                data = data,
                intersect = set_vars,
                min_size = self$options$min_size,
                max_degree = self$options$max_degree,
                keep_empty_groups = self$options$keep_empty_groups,
                sort_by = self$options$sort_by,
                sort_order = self$options$sort_order,
                width_ratio = self$options$width_ratio,
                height_ratio = self$options$height_ratio
            )
            
            # Apply customizations
            p <- private$.applyCustomizations(p, data)
            
            # Add annotations if specified
            if (self$options$annotations != "none") {
                p <- private$.addAnnotations(p, data)
            }
            
            # Apply theme and colors
            p <- private$.applyThemeAndColors(p)
            
            # Add labels
            p <- private$.addLabels(p)
            
            return(p)
        },
        
        .applyCustomizations = function(plot, data) {
            # Apply size customizations
            if (!self$options$set_size_show) {
                plot <- plot + ComplexUpset::upset_set_size(show = FALSE)
            }
            
            if (!self$options$intersection_size_show) {
                plot <- plot + ComplexUpset::upset_intersection_size(show = FALSE)
            }
            
            # Apply percentages if requested
            if (self$options$show_percentages) {
                plot <- plot + ComplexUpset::upset_intersection_size(
                    mapping = ggplot2::aes(label = !!ComplexUpset::aes_percentage(
                        relative_to = 'intersection'
                    ))
                )
            }
            
            return(plot)
        },
        
        .addAnnotations = function(plot, data) {
            annotation_type <- self$options$annotations
            height <- self$options$base_annotations_height
            
            if (annotation_type == "intersection_size") {
                plot <- plot + ComplexUpset::upset_intersection_size(
                    height = height,
                    text = list(size = self$options$text_size * 0.8)
                )
            } else if (annotation_type == "intersection_ratio") {
                plot <- plot + ComplexUpset::upset_intersection_ratio(
                    height = height,
                    text = list(size = self$options$text_size * 0.8)
                )
            } else if (annotation_type == "union_size") {
                plot <- plot + ComplexUpset::upset_union_size(
                    height = height,
                    text = list(size = self$options$text_size * 0.8)
                )
            }
            
            return(plot)
        },
        
        .applyThemeAndColors = function(plot) {
            theme_style <- self$options$theme_style
            
            # Apply theme
            if (theme_style == "theme_minimal") {
                plot <- plot + ggplot2::theme_minimal()
            } else if (theme_style == "theme_classic") {
                plot <- plot + ggplot2::theme_classic()
            } else if (theme_style == "theme_gray") {
                plot <- plot + ggplot2::theme_gray()
            } else if (theme_style == "theme_bw") {
                plot <- plot + ggplot2::theme_bw()
            } else if (theme_style == "theme_void") {
                plot <- plot + ggplot2::theme_void()
            }
            
            # Apply text size
            plot <- plot + ggplot2::theme(
                text = ggplot2::element_text(size = self$options$text_size),
                axis.text = ggplot2::element_text(size = self$options$text_size * 0.9),
                strip.text = ggplot2::element_text(size = self$options$text_size * 0.9)
            )
            
            # Apply color palette
            palette <- self$options$color_palette
            if (palette %in% c("viridis", "plasma", "inferno", "magma")) {
                plot <- plot + ggplot2::scale_fill_viridis_d(option = palette)
            } else {
                plot <- plot + ggplot2::scale_fill_brewer(palette = palette)
            }
            
            return(plot)
        },
        
        .addLabels = function(plot) {
            plot <- plot + ggplot2::labs(
                title = self$options$plot_title,
                subtitle = self$options$plot_subtitle
            )
            
            return(plot)
        },
        
        .generateStatistics = function(data) {
            set_vars <- self$options$set_vars
            
            # Calculate set sizes
            set_sizes <- sapply(set_vars, function(var) {
                sum(data[[var]], na.rm = TRUE)
            })
            
            # Calculate intersection statistics
            intersections <- private$.calculateIntersections(data, set_vars)
            
            # Create HTML table for set sizes
            html <- "<h3>Set Statistics</h3>"
            html <- paste0(html, "<table class='table table-striped'>")
            html <- paste0(html, "<thead><tr><th>Set</th><th>Size</th><th>Percentage</th></tr></thead><tbody>")
            
            total_elements <- nrow(data)
            for (i in seq_along(set_vars)) {
                var <- set_vars[i]
                size <- set_sizes[[var]]
                percentage <- round((size / total_elements) * 100, 1)
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td>", var, "</td>")
                html <- paste0(html, "<td>", size, "</td>")
                html <- paste0(html, "<td>", percentage, "%</td>")
                html <- paste0(html, "</tr>")
            }
            html <- paste0(html, "</tbody></table>")
            
            # Add intersection summary
            html <- paste0(html, "<h3>Intersection Summary</h3>")
            html <- paste0(html, "<p><strong>Total elements:</strong> ", total_elements, "</p>")
            html <- paste0(html, "<p><strong>Number of sets:</strong> ", length(set_vars), "</p>")
            html <- paste0(html, "<p><strong>Largest intersection:</strong> ", max(intersections$size), "</p>")
            html <- paste0(html, "<p><strong>Number of intersections:</strong> ", nrow(intersections), "</p>")
            
            return(html)
        },
        
        .calculateIntersections = function(data, set_vars) {
            # Generate all possible intersections
            n_sets <- length(set_vars)
            intersections <- list()
            
            for (i in 1:(2^n_sets - 1)) {
                # Convert to binary representation
                binary <- as.logical(intToBits(i)[1:n_sets])
                
                # Get sets in this intersection
                sets_in_intersection <- set_vars[binary]
                
                # Calculate intersection size
                if (length(sets_in_intersection) == 1) {
                    # Single set
                    intersection_data <- data[[sets_in_intersection]]
                } else {
                    # Multiple sets - all must be TRUE
                    intersection_data <- rowSums(data[, sets_in_intersection, drop = FALSE]) == length(sets_in_intersection)
                }
                
                size <- sum(intersection_data, na.rm = TRUE)
                
                if (size > 0) {
                    intersections[[length(intersections) + 1]] <- data.frame(
                        intersection = paste(sets_in_intersection, collapse = " & "),
                        degree = length(sets_in_intersection),
                        size = size,
                        stringsAsFactors = FALSE
                    )
                }
            }
            
            # Combine results
            if (length(intersections) > 0) {
                result <- do.call(rbind, intersections)
                # Sort by size
                result <- result[order(-result$size), ]
            } else {
                result <- data.frame(intersection = character(0), degree = numeric(0), size = numeric(0))
            }
            
            return(result)
        },
        
        .generateInterpretation = function(data) {
            set_vars <- self$options$set_vars
            n_sets <- length(set_vars)
            total_elements <- nrow(data)
            
            # Calculate basic statistics
            set_sizes <- sapply(set_vars, function(var) sum(data[[var]], na.rm = TRUE))
            largest_set <- names(set_sizes)[which.max(set_sizes)]
            smallest_set <- names(set_sizes)[which.min(set_sizes)]
            
            # Calculate intersections
            intersections <- private$.calculateIntersections(data, set_vars)
            
            # Create interpretation
            interpretation <- paste0(
                "<h3>UpSet Plot Interpretation</h3>",
                "<p><strong>Dataset Overview:</strong> The UpSet plot visualizes intersections among ", n_sets, " sets ",
                "with a total of ", total_elements, " elements.</p>",
                "<p><strong>Set Sizes:</strong> The largest set is <em>", largest_set, "</em> with ", max(set_sizes), " elements, ",
                "while the smallest set is <em>", smallest_set, "</em> with ", min(set_sizes), " elements.</p>"
            )
            
            if (nrow(intersections) > 0) {
                largest_intersection <- intersections[1, ]
                interpretation <- paste0(interpretation,
                    "<p><strong>Intersection Analysis:</strong> There are ", nrow(intersections), " non-empty intersections. ",
                    "The largest intersection is <em>", largest_intersection$intersection, "</em> with ", largest_intersection$size, " elements.</p>"
                )
            }
            
            interpretation <- paste0(interpretation,
                "<p><strong>Visual Insights:</strong> UpSet plots are superior to Venn diagrams for visualizing ",
                "complex set relationships, especially with more than 3 sets. The matrix layout clearly shows ",
                "which sets participate in each intersection, while the bar charts quantify intersection sizes.</p>"
            )
            
            return(interpretation)
        },
        
        .checkData = function() {
            set_vars <- self$options$set_vars
            
            if (length(set_vars) < 2) {
                stop("At least 2 set variables are required for UpSet plots")
            }
            
            if (length(set_vars) > 10) {
                stop("Maximum of 10 set variables supported for readability")
            }
            
            # Check if variables exist and have valid data
            for (var in set_vars) {
                if (!var %in% names(self$data)) {
                    stop(paste("Variable", var, "not found in data"))
                }
                
                var_data <- self$data[[var]]
                if (all(is.na(var_data))) {
                    stop(paste("Variable", var, "contains only missing values"))
                }
            }
        },
        
        .plot = function(image, ...) {
            if (is.null(self$data) || is.null(self$options$set_vars) || length(self$options$set_vars) < 2) {
                return()
            }
            
            plot <- self$results$plot$state
            if (is.null(plot)) {
                return()
            }
            
            print(plot)
        }
    )
)
