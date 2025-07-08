#' @title Waffle Charts
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import waffle
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @import scales
#'
#' @description Create Waffle Charts to visualize distributions.
#'
#' @param data A data frame.
#' @param groups A grouping variable to organize the squares.
#' @param counts Optionally, a numeric variable for specific counts (if not provided, will use number of cases).
#' @param facet Optionally, a variable to facet the plot.
#' @param rows Number of rows in the waffle chart.
#' @param flip Flip the waffle chart.
#' @param color_palette The color palette to use. Options are 'default', 'colorblind', 'professional',
#' 'presentation', 'journal', 'pastel', and 'dark'.
#' @param legendtitle Title for the legend.
#' @param show_legend Show the legend.
#' @param mytitle Title for the plot.
#'
#' @return The function produces a waffle chart.
#'
#'

jwaffleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jwaffleClass",
    inherit = jwaffleBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .cached_plot = NULL,
        .cached_palette = NULL,
        
        .init = function() {
            self$results$plot$setSize(600, 500)

            if (!is.null(self$options$facet)) {
                mydata <- self$data
                facet_var <- self$options$facet
                if (!is.null(mydata[[facet_var]])) {
                    num_levels <- length(unique(mydata[[facet_var]]))
                    if (num_levels > 1)
                        self$results$plot$setSize(num_levels * 600, 500)
                }
            }
        },

        # Performance optimization methods
        .calculateDataHash = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(NULL)
            }
            
            # Determine relevant variables
            relevant_vars <- c()
            if (!is.null(self$options$groups)) relevant_vars <- c(relevant_vars, self$options$groups)
            if (!is.null(self$options$counts)) relevant_vars <- c(relevant_vars, self$options$counts)
            if (!is.null(self$options$facet)) relevant_vars <- c(relevant_vars, self$options$facet)
            
            # Remove NULLs and ensure variables exist
            relevant_vars <- relevant_vars[!sapply(relevant_vars, is.null)]
            relevant_vars <- relevant_vars[relevant_vars %in% names(self$data)]
            
            if (length(relevant_vars) == 0) {
                return(NULL)
            }
            
            # Create hash string including data summary
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
                sep = "_"
            )
            
            return(data_summary)
        },
        
        .calculateOptionsHash = function() {
            # Create hash of all relevant options for waffle chart
            options_list <- list(
                groups = self$options$groups,
                counts = self$options$counts,
                facet = self$options$facet,
                rows = self$options$rows,
                flip = self$options$flip,
                color_palette = self$options$color_palette,
                legendtitle = self$options$legendtitle,
                show_legend = self$options$show_legend,
                mytitle = self$options$mytitle
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
                
                # Clean data
                mydata <- jmvcore::naOmit(mydata)
                
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
                
                # Clear cached results when options change
                private$.cached_plot <- NULL
                private$.cached_palette <- NULL
            }
            
            return(private$.prepared_options)
        },
        
        .generateColorPalette = function(n_groups) {
            current_hash <- paste(private$.calculateOptionsHash(), n_groups, sep = "_")
            
            if (is.null(private$.cached_palette) || attr(private$.cached_palette, "hash") != current_hash) {
                # Enhanced color palettes with professional styling
                palettes <- list(
                    default = colorRampPalette(c("#4DA6FF", "#FFB84D"))(n_groups),
                    colorblind = colorRampPalette(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))(n_groups),
                    professional = colorRampPalette(c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C", "#34495E"))(n_groups),
                    presentation = colorRampPalette(c("#003f5c", "#bc5090", "#ffa600", "#58508d", "#ff6361", "#003f5c"))(n_groups),
                    journal = colorRampPalette(c("#334455", "#778899", "#99AABB", "#BBCCDD", "#556677", "#667788"))(n_groups),
                    pastel = colorRampPalette(c("#69b3a2", "#404080", "#FFA07A", "#98D8E8", "#F7DC6F", "#BB8FCE"))(n_groups),
                    dark = colorRampPalette(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))(n_groups)
                )
                
                selected_palette <- palettes[[self$options$color_palette]]
                if (is.null(selected_palette)) {
                    selected_palette <- palettes$default
                }
                
                attr(selected_palette, "hash") <- current_hash
                private$.cached_palette <- selected_palette
            }
            
            return(private$.cached_palette)
        },
        
        .run = function() {
            if (is.null(self$options$groups)) {
                todo <- glue::glue(
                    "<br>Welcome to ClinicoPath
                    <br><br>
                    This tool will help you create Waffle Charts to visualize distributions.
                    <br><br>
                    Please provide:
                    <br>1. A grouping variable to organize the squares
                    <br>2. Optionally, a numeric variable for specific counts (if not provided, will use number of cases)
                    <br><br>
                    The waffle chart will show proportions using squares arranged in a grid.
                    <br><hr>"
                )
                self$results$todo$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Performance optimization: prepare data and options with caching
            mydata <- private$.prepareData()
            options <- private$.prepareOptions()
            
            if (is.null(mydata) || nrow(mydata) == 0) {
                stop('Data contains no (complete) rows')
            }
            
            todo <- glue::glue("<br>Creating waffle chart...<br><hr>")
            self$results$todo$setContent(todo)
        },

        .plot = function(image, ...) {
            if (is.null(self$options$groups))
                return()

            # Performance optimization: use prepared data and check cache
            if (private$.canUseCache()) {
                if (!is.null(private$.cached_plot)) {
                    print(private$.cached_plot)
                    return(TRUE)
                }
            }

            # Prepare data using cached method
            mydata <- private$.prepareData()
            
            if (is.null(mydata) || nrow(mydata) == 0)
                stop('Data contains no (complete) rows')

            groups_var <- self$options$groups
            facet_var <- self$options$facet
            counts_var <- self$options$counts

            # Build grouping expression based on whether faceting is used
            if (!is.null(facet_var)) {
                if (!is.null(counts_var)) {
                    plotdata <- mydata %>%
                        dplyr::group_by(
                            !!rlang::sym(groups_var),
                            !!rlang::sym(facet_var)
                        ) %>%
                        dplyr::summarise(
                            count = sum(!!rlang::sym(counts_var))
                        ) %>%
                        dplyr::ungroup()
                } else {
                    plotdata <- mydata %>%
                        dplyr::group_by(
                            !!rlang::sym(groups_var),
                            !!rlang::sym(facet_var)
                        ) %>%
                        dplyr::summarise(
                            count = dplyr::n()
                        ) %>%
                        dplyr::ungroup()
                }
            } else {
                if (!is.null(counts_var)) {
                    plotdata <- mydata %>%
                        dplyr::group_by(!!rlang::sym(groups_var)) %>%
                        dplyr::summarise(
                            count = sum(!!rlang::sym(counts_var))
                        ) %>%
                        dplyr::ungroup()
                } else {
                    plotdata <- mydata %>%
                        dplyr::group_by(!!rlang::sym(groups_var)) %>%
                        dplyr::summarise(
                            count = dplyr::n()
                        ) %>%
                        dplyr::ungroup()
                }
            }

            # Calculate values for caption
            total_cases <- sum(plotdata$count)
            n_squares <- 100  # Total number of squares in waffle chart
            cases_per_square <- total_cases / n_squares
            squares_per_case <- 100 / total_cases  # Each square represents this percentage
            caption_text <- sprintf("Each square represents %.1f cases (approximately %.1f%%) (total n=%d)",
                                    cases_per_square, squares_per_case, total_cases)

            # Get number of unique groups
            n_groups <- length(unique(plotdata[[groups_var]]))

            # Generate color palette using cached method
            sel_palette <- private$.generateColorPalette(n_groups)

            # Create base plot
            p <- ggplot2::ggplot(
                plotdata,
                ggplot2::aes(
                    fill = !!rlang::sym(groups_var),
                    values = count
                )
            ) +
                waffle::geom_waffle(
                    n_rows = self$options$rows,
                    size = 0.5,
                    color = "white",
                    flip = self$options$flip,
                    make_proportional = TRUE
                ) +
                ggplot2::scale_fill_manual(
                    values = sel_palette,
                    name = if (self$options$legendtitle != '')
                        self$options$legendtitle
                    else
                        groups_var
                ) +
                ggplot2::coord_equal() +
                ggplot2::theme_minimal()

            # Add labels in specific order
            if (!is.null(facet_var)) {
                facet_title <- facet_var  # Store the facet variable name
                p <- p +
                    ggplot2::labs(
                        tag = facet_title,  # Facet variable name
                        caption = paste0(  # Combine caption and title
                            self$options$mytitle,
                            "\n\n",
                            caption_text
                        )
                    ) +
                    ggplot2::facet_wrap(
                        as.formula(paste0("~", facet_var)),
                        nrow = 1,
                        strip.position = "bottom"
                    )
            } else {
                p <- p + ggplot2::labs(
                    caption = paste0(
                        self$options$mytitle,
                        "\n\n",
                        caption_text
                    )
                )
            }

            # Handle legend
            if (!self$options$show_legend) {
                p <- p + ggplot2::theme(legend.position = "none")
            }

            # Apply final theme adjustments with enhanced professional styling
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(
                    hjust = 0.5,
                    size = 16,
                    face = "bold",
                    margin = ggplot2::margin(b = 20)
                ),
                plot.caption = ggplot2::element_text(
                    size = 11,
                    hjust = 0.5,
                    face = "bold",
                    margin = ggplot2::margin(t = 25)
                ),
                plot.tag = ggplot2::element_text(
                    size = 14,
                    face = "bold",
                    hjust = 0.5
                ),
                plot.tag.position = "top",
                legend.position = if(self$options$show_legend) "right" else "none",
                legend.title = ggplot2::element_text(
                    size = 12,
                    face = "bold"
                ),
                legend.text = ggplot2::element_text(
                    size = 10
                ),
                legend.margin = ggplot2::margin(l = 20),
                strip.background = ggplot2::element_rect(
                    fill = "grey95",
                    color = "grey85",
                    size = 0.5
                ),
                strip.text = ggplot2::element_text(
                    size = 12,
                    face = "bold",
                    margin = ggplot2::margin(b = 10, t = 10)
                ),
                panel.grid = ggplot2::element_blank(),
                axis.text = ggplot2::element_blank(),
                axis.title = ggplot2::element_blank(),
                panel.background = ggplot2::element_rect(
                    fill = "white",
                    color = NA
                ),
                plot.background = ggplot2::element_rect(
                    fill = "white",
                    color = NA
                )
            )

            # Cache the plot and return
            private$.cached_plot <- p
            print(p)
            TRUE
        }
    )
)
