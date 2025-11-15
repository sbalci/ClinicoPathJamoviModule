#' @title Violin Plot
#' @description Create professional violin plots for distribution visualization
#' @return A violin plot showing distribution of continuous variables across groups
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr %>%

jviolinClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "jviolinClass",
    inherit = jviolinBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .cached_plot = NULL,
        .cached_plotdata = NULL,
        
        .init = function() {
            # Initialize results visibility
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                self$results$plot$setVisible(FALSE)
                self$results$todo$setVisible(TRUE)
            } else {
                self$results$plot$setVisible(TRUE)
                self$results$todo$setVisible(FALSE)
            }
        },
        
        # Performance optimization methods
        .calculateDataHash = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(NULL)
            }
            
            # Create hash based on relevant data
            dep <- self$options$dep
            group <- self$options$group
            col <- self$options$col
            fill <- self$options$fill
            
            # Determine relevant variables
            relevant_vars <- c(dep, group)
            if (!is.null(col)) relevant_vars <- c(relevant_vars, col)
            if (!is.null(fill)) relevant_vars <- c(relevant_vars, fill)
            
            # Remove NULLs and ensure variables exist
            relevant_vars <- relevant_vars[!sapply(relevant_vars, is.null)]
            relevant_vars <- relevant_vars[relevant_vars %in% names(self$data)]
            
            if (length(relevant_vars) == 0) {
                return(NULL)
            }
            
            # Create hash string including data summary and exclude missing option
            data_summary <- paste(
                nrow(self$data),
                ncol(self$data),
                paste(relevant_vars, collapse = "_"),
                paste(sapply(relevant_vars, function(var) {
                    if (is.numeric(self$data[[var]])) {
                        paste(range(self$data[[var]], na.rm = TRUE), collapse = "_")
                    } else {
                        paste(length(unique(self$data[[var]])), "levels")
                    }
                }), collapse = "_"),
                self$options$excl,  # Include exclude missing option in hash
                sep = "_"
            )
            
            return(data_summary)
        },
        
        .calculateOptionsHash = function() {
            # Create hash of all relevant options
            options_list <- list(
                dep = self$options$dep,
                group = self$options$group,
                col = self$options$col,
                fill = self$options$fill,
                excl = self$options$excl,
                flip = self$options$flip,
                themex = self$options$themex,
                usexlabel = self$options$usexlabel,
                xlabel = self$options$xlabel,
                useylabel = self$options$useylabel,
                ylabel = self$options$ylabel,
                add_boxplot = self$options$add_boxplot,
                add_points = self$options$add_points,
                add_mean = self$options$add_mean,
                draw_quantiles = self$options$draw_quantiles,
                quantile_lines = paste(self$options$quantile_lines, collapse = "_"),
                trim_violin = self$options$trim_violin,
                scale_violin = self$options$scale_violin,
                violin_width = self$options$violin_width,
                violin_alpha = self$options$violin_alpha,
                boxplot_width = self$options$boxplot_width,
                boxplot_alpha = self$options$boxplot_alpha,
                point_size = self$options$point_size,
                point_alpha = self$options$point_alpha,
                point_jitter = self$options$point_jitter,
                color_palette = self$options$color_palette,
                manual_colors = paste(self$options$manual_colors, collapse = "_")
            )
            
            return(paste(options_list, collapse = "_"))
        },
        
        .canUseCache = function() {
            current_data_hash <- private$.calculateDataHash()
            current_options_hash <- private$.calculateOptionsHash()
            
            return(!is.null(private$.cached_plot) &&
                   !is.null(private$.data_hash) &&
                   !is.null(private$.options_hash) &&
                   !is.null(current_data_hash) &&
                   !is.null(current_options_hash) &&
                   current_data_hash == private$.data_hash &&
                   current_options_hash == private$.options_hash)
        },
        
        .prepareData = function() {
            current_hash <- private$.calculateDataHash()
            
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                # Data has changed, prepare new data
                mydata <- self$data
                
                if (is.null(mydata) || nrow(mydata) == 0) {
                    private$.prepared_data <- NULL
                    private$.data_hash <- current_hash
                    return(NULL)
                }
                
                # Exclude NA if requested
                if (self$options$excl) {
                    mydata <- jmvcore::naOmit(mydata)
                }
                
                private$.prepared_data <- mydata
                private$.data_hash <- current_hash
            }
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            current_hash <- private$.calculateOptionsHash()
            
            if (is.null(private$.options_hash) || private$.options_hash != current_hash) {
                private$.prepared_options <- self$options
                private$.options_hash <- current_hash
                
                # Clear cached plot when options change
                private$.cached_plot <- NULL
                private$.cached_plotdata <- NULL
            }
            
            return(private$.prepared_options)
        },
        
        .run = function() {
            # Check required variables
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                todo <- paste(
                    "<br>Welcome to ClinicoPath",
                    "<br><br>",
                    "This tool will help you create professional violin plots.",
                    "<br><br>",
                    "<b>Violin plots</b> are ideal for visualizing the distribution of continuous data across groups.",
                    "<br><br>",
                    "<b>Required:</b>",
                    "<br>• Dependent Variable (continuous)",
                    "<br>• Grouping Variable (categorical)",
                    "<br><hr>"
                )
                
                self$results$todo$setContent(todo)
                return()
            }
            
            # Prepare data and options with caching
            mydata <- private$.prepareData()
            options <- private$.prepareOptions()
            
            if (is.null(mydata) || nrow(mydata) == 0) {
                jmvcore::reject("Data contains no (complete) rows")
                return()
            }
            
            # Check if dependent variable is numeric
            dep_var <- mydata[[self$options$dep]]
            if (!is.numeric(dep_var)) {
                jmvcore::reject("Dependent variable must be numeric")
                return()
            }
            
            # Set empty todo content when analysis is running
            self$results$todo$setContent("")
            
            # The actual plotting will be handled in .plot method
            # Here we just prepare the plot state
            plotData <- list(
                data = mydata,
                options = options
            )
            
            image <- self$results$plot
            image$setState(plotData)

            private$.applyClinicalPreset()
            private$.generateExplanations()
        },

        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            if (preset == "custom") {
                return()
            }

            if (preset == "biomarker_distribution") {
                self$options$add_boxplot <- TRUE
                self$options$themex <- "bw"
            } else if (preset == "publication_ready") {
                self$options$themex <- "pubr"
            }
        },

        .generateExplanations = function() {
            if (self$options$showExplanations) {
                self$results$explanations$setVisible(TRUE)
                self$results$explanations$setContent(
                    "<h3>Explanations</h3>
                    <p>
                        This violin plot shows the distribution of a continuous variable across different groups.
                        The width of the violin represents the density of the data at different values.
                        The white dot inside the violin represents the median.
                        The black bar in the center of the violin represents the interquartile range.
                        The thin black line extending from the bar represents the 95% confidence interval.
                    </p>"
                )
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Check required variables
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                return()
            }
            
            # Performance optimization: check if we can use cached plot
            if (private$.canUseCache()) {
                return(private$.cached_plot)
            }
            
            # Get plot data from state or prepare fresh
            plotData <- image$state
            if (is.null(plotData)) {
                mydata <- private$.prepareData()
                options <- private$.prepareOptions()
                
                if (is.null(mydata) || nrow(mydata) == 0) {
                    return()
                }
                
                plotData <- list(data = mydata, options = options)
            }
            
            mydata <- plotData$data
            options <- plotData$options
            
            # Define variables for aesthetics
            dep <- self$options$dep
            group <- self$options$group
            
            # Handle fill variable
            if (is.null(self$options$fill)) {
                fill_var <- group
            } else {
                fill_var <- self$options$fill
            }
            
            # Handle color variable
            if (is.null(self$options$col)) {
                col_var <- group
            } else {
                col_var <- self$options$col
            }
            
            # Create base plot
            p <- ggplot2::ggplot(mydata, ggplot2::aes(x = .data[[group]], 
                                                      y = .data[[dep]], 
                                                      fill = .data[[fill_var]], 
                                                      color = .data[[col_var]]))
            
            # Add violin layer with advanced options
            violin_args <- list()
            
            if (self$options$trim_violin) {
                violin_args$trim <- TRUE
            } else {
                violin_args$trim <- FALSE
            }
            
            violin_args$scale <- self$options$scale_violin
            violin_args$width <- self$options$violin_width
            violin_args$alpha <- self$options$violin_alpha
            
            # Add quantile lines if requested
            if (self$options$draw_quantiles && length(self$options$quantile_lines) > 0) {
                quantiles <- as.numeric(self$options$quantile_lines)
                quantiles <- quantiles[!is.na(quantiles) & quantiles > 0 & quantiles < 1]
                if (length(quantiles) > 0) {
                    violin_args$draw_quantiles <- quantiles
                }
            }
            
            p <- p + do.call(ggplot2::geom_violin, violin_args)
            
            # Add boxplot overlay if requested
            if (self$options$add_boxplot) {
                p <- p + ggplot2::geom_boxplot(
                    width = self$options$boxplot_width,
                    alpha = self$options$boxplot_alpha,
                    outlier.shape = NA,  # Hide outliers if points are shown
                    show.legend = FALSE
                )
            }
            
            # Add points if requested
            if (self$options$add_points) {
                jitter_width <- if (self$options$point_jitter) 0.2 else 0
                p <- p + ggplot2::geom_jitter(
                    width = jitter_width,
                    size = self$options$point_size,
                    alpha = self$options$point_alpha,
                    show.legend = FALSE
                )
            }
            
            # Add mean points if requested
            if (self$options$add_mean) {
                mean_data <- mydata %>%
                    dplyr::group_by(.data[[group]]) %>%
                    dplyr::summarise(mean_val = mean(.data[[dep]], na.rm = TRUE), .groups = 'drop')
                
                p <- p + ggplot2::geom_point(
                    data = mean_data,
                    ggplot2::aes(x = .data[[group]], y = .data$mean_val),
                    size = 4,
                    shape = 23,
                    fill = "red",
                    color = "darkred",
                    inherit.aes = FALSE
                )
            }
            
            # Apply color palette
            if (self$options$color_palette == "viridis") {
                p <- p + ggplot2::scale_fill_viridis_d() + ggplot2::scale_color_viridis_d()
            } else if (self$options$color_palette == "brewer") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual") + ggplot2::scale_color_brewer(type = "qual")
            } else if (self$options$color_palette == "manual" && length(self$options$manual_colors) > 0) {
                colors <- self$options$manual_colors
                p <- p + ggplot2::scale_fill_manual(values = colors) + ggplot2::scale_color_manual(values = colors)
            }
            
            # Apply theme
            themex <- self$options$themex
            if (themex == "ipsum") {
                if (requireNamespace('hrbrthemes', quietly = TRUE)) {
                    p <- p + hrbrthemes::theme_ipsum()
                } else {
                    p <- p + ggplot2::theme_minimal()
                }
            } else if (themex == "grey") {
                p <- p + ggplot2::theme_grey()
            } else if (themex == "gray") {
                p <- p + ggplot2::theme_gray()
            } else if (themex == "bw") {
                p <- p + ggplot2::theme_bw()
            } else if (themex == "linedraw") {
                p <- p + ggplot2::theme_linedraw()
            } else if (themex == "light") {
                p <- p + ggplot2::theme_light()
            } else if (themex == "dark") {
                p <- p + ggplot2::theme_dark()
            } else if (themex == "minimal") {
                p <- p + ggplot2::theme_minimal()
            } else if (themex == "classic") {
                p <- p + ggplot2::theme_classic()
            } else if (themex == "void") {
                p <- p + ggplot2::theme_void()
            } else if (themex == "test") {
                p <- p + ggplot2::theme_test()
            }
            
            # Add custom axis labels
            if (self$options$usexlabel) {
                p <- p + ggplot2::xlab(self$options$xlabel)
            }
            
            if (self$options$useylabel) {
                p <- p + ggplot2::ylab(self$options$ylabel)  # Fixed bug: was xlab
            }
            
            # Flip coordinates if requested
            if (self$options$flip) {
                p <- p + ggplot2::coord_flip()
            }
            
            # Cache the plot
            private$.cached_plot <- p
            
            print(p)
            TRUE
        }
    )
)