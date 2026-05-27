#' @title Radar Plot
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjradarplotClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjradarplotClass",
        inherit = jjradarplotBase,
        private = list(
            # init ----
            # TODO (data hygiene): .init sizing path at L22 forces a full-column
            # as.factor() coercion every time the user toggles splitBy, even before
            # they finish configuring the analysis. On large datasets this is
            # wasteful. Worse, cols*400 / rows*400 is unbounded: a splitBy column
            # with hundreds of unique levels produces enormous canvases. Cap
            # num_levels at a sane upper bound (e.g., 16) and emit a notice when
            # exceeded, or defer sizing to .run() / .plot() once data is validated.
            #
            # TODO (forward-looking): function lacks `.asSource()` and a reproducible
            # R code export (showRCode option + rCode Html result). Adding either
            # would require defining a `public = list(.asSource = function() {...})`
            # block. When implemented, use `deparse()` for string literals and
            # `jmvcore::composeTerm` for column names to keep codegen safe for
            # non-syntactic variable names (e.g., `Tumor Grade`, `weird"name\path`).
            .init = function() {
                # Set default plot size
                self$results$plot$setSize(600, 600)

                # Adjust plot size based on number of groups if splitting
                if (!is.null(self$options$splitBy)) {
                    mydata <- self$data
                    splitBy <- self$options$splitBy

                    num_levels <- nlevels(as.factor(mydata[[splitBy]]))

                    # Calculate grid dimensions
                    cols <- ceiling(sqrt(num_levels))
                    rows <- ceiling(num_levels / cols)

                    self$results$plot$setSize(cols * 400, rows * 400)
                }
            },
            
            # run ----
            .run = function() {
                # Initial Message ----
                vars <- self$options$vars
                categoryVar <- self$options$categoryVar
                
                if (is.null(vars) || length(vars) < 3 || is.null(categoryVar)) {
                    todo <- glue::glue(
                        "<br>Welcome to ClinicoPath Radar Plot
                        <br><br>
                        This tool will help you create radar plots (spider plots) for multivariate data visualization.
                        <br><br>
                        Requirements:
                        <ul>
                        <li>At least 3 variables for radar axes</li>
                        <li>One categorical variable to define different radar polygons</li>
                        </ul>
                        <br>
                        This function uses ggplot2 and scales data for optimal visualization.
                        <br>
                        Please cite jamovi and the packages as given below.
                        <br><hr>"
                    )
                    
                    self$results$todo$setContent(todo)
                    return()
                } else {
                    safe_vars <- htmltools::htmlEscape(paste(vars, collapse = ", "))
                    safe_cat <- htmltools::htmlEscape(categoryVar)
                    safe_split <- if (!is.null(self$options$splitBy)) paste0(", split by ", htmltools::htmlEscape(self$options$splitBy)) else ""
                    todo <- glue::glue(
                        "<br>Radar plot analysis for variables: {safe_vars} by {safe_cat}{safe_split}.<br><hr>"
                    )

                    self$results$todo$setContent(todo)
                    
                    # Data validation
                    if (nrow(self$data) == 0)
                        jmvcore::reject(.('Data contains no (complete) rows'))
                    
                    # Add checkpoint for user feedback
                    private$.checkpoint()
                }
            },
            
            # plot ----
            .plot = function(image, ggtheme, theme, ...) {
                # Validation ----
                vars <- self$options$vars
                categoryVar <- self$options$categoryVar
                
                if (is.null(vars) || length(vars) < 3 || is.null(categoryVar))
                    return()
                
                if (nrow(self$data) == 0)
                    jmvcore::reject(.('Data contains no (complete) rows'))

                # Add checkpoint for user feedback
                private$.checkpoint()

                # Prepare Data ----
                mydata <- self$data
                mydata <- jmvcore::naOmit(mydata)

                if (nrow(mydata) == 0)
                    jmvcore::reject(.('Data contains no (complete) rows after removing missing values'))
                
                # Select only needed variables
                radar_vars <- c(vars, categoryVar)
                if (!is.null(self$options$splitBy)) {
                    radar_vars <- c(radar_vars, self$options$splitBy)
                }
                
                mydata <- mydata[radar_vars]
                
                # Check if we have numeric variables for radar axes
                numeric_vars <- sapply(mydata[vars], is.numeric)
                if (!all(numeric_vars)) {
                    non_numeric <- names(numeric_vars)[!numeric_vars]
                    jmvcore::reject(.('Variables must be numeric for radar plot: {vars}'), vars = paste(non_numeric, collapse = ", "))
                }
                
                # Create radar plot
                if (is.null(self$options$splitBy)) {
                    # Single radar plot
                    plot <- private$.createRadarPlot(mydata, vars, categoryVar, ggtheme)
                } else {
                    # Faceted radar plot
                    plot <- private$.createFacetedRadarPlot(mydata, vars, categoryVar, self$options$splitBy, ggtheme)
                }
                
                print(plot)
                TRUE
            },
            
            # Helper function to create single radar plot ----
            .createRadarPlot = function(data, vars, categoryVar, ggtheme) {
                # Aggregate data by category (mean)
                aggregated_data <- data %>%
                    dplyr::group_by(!!rlang::sym(categoryVar)) %>%
                    dplyr::summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')
                
                # Scale variables to 0-1 range for better visualization
                if (self$options$scaleData) {
                    for (var in vars) {
                        min_val <- min(data[[var]], na.rm = TRUE)
                        max_val <- max(data[[var]], na.rm = TRUE)
                        if (max_val != min_val) {
                            aggregated_data[[var]] <- (aggregated_data[[var]] - min_val) / (max_val - min_val)
                        }
                    }
                }
                
                # Transform to long format for ggplot
                plot_data <- aggregated_data %>%
                    tidyr::pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value") %>%
                    dplyr::mutate(variable = factor(variable, levels = vars))
                
                # Create radar plot using coord_radar
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = variable, y = value, 
                                                               group = !!rlang::sym(categoryVar), 
                                                               color = !!rlang::sym(categoryVar),
                                                               fill = !!rlang::sym(categoryVar))) +
                    ggplot2::geom_polygon(alpha = self$options$alpha, size = self$options$lineSize) +
                    ggplot2::geom_point(size = self$options$pointSize) +
                    ggplot2::coord_polar() +
                    ggplot2::scale_y_continuous(limits = c(0, ifelse(self$options$scaleData, 1, NA))) +
                    ggtheme +
                    ggplot2::labs(
                        title = self$options$title,
                        x = "",
                        y = if (self$options$scaleData) "Scaled Value" else "Value",
                        color = categoryVar,
                        fill = categoryVar
                    ) +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(size = self$options$axisLabelSize),
                        legend.position = self$options$legendPosition
                    )
                
                return(plot)
            },
            
            # Helper function to create faceted radar plot ----
            .createFacetedRadarPlot = function(data, vars, categoryVar, splitBy, ggtheme) {
                # Aggregate data by category and split variable
                aggregated_data <- data %>%
                    dplyr::group_by(!!rlang::sym(categoryVar), !!rlang::sym(splitBy)) %>%
                    dplyr::summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')
                
                # Scale variables to 0-1 range for better visualization
                if (self$options$scaleData) {
                    for (var in vars) {
                        min_val <- min(data[[var]], na.rm = TRUE)
                        max_val <- max(data[[var]], na.rm = TRUE)
                        if (max_val != min_val) {
                            aggregated_data[[var]] <- (aggregated_data[[var]] - min_val) / (max_val - min_val)
                        }
                    }
                }
                
                # Transform to long format for ggplot
                plot_data <- aggregated_data %>%
                    tidyr::pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value") %>%
                    dplyr::mutate(variable = factor(variable, levels = vars))
                
                # Create faceted radar plot
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = variable, y = value, 
                                                               group = !!rlang::sym(categoryVar), 
                                                               color = !!rlang::sym(categoryVar),
                                                               fill = !!rlang::sym(categoryVar))) +
                    ggplot2::geom_polygon(alpha = self$options$alpha, size = self$options$lineSize) +
                    ggplot2::geom_point(size = self$options$pointSize) +
                    ggplot2::coord_polar() +
                    ggplot2::scale_y_continuous(limits = c(0, ifelse(self$options$scaleData, 1, NA))) +
                    ggplot2::facet_wrap(jmvcore::asFormula(paste0("~", jmvcore::composeTerm(splitBy)))) +
                    ggtheme +
                    ggplot2::labs(
                        title = self$options$title,
                        x = "",
                        y = if (self$options$scaleData) "Scaled Value" else "Value",
                        color = categoryVar,
                        fill = categoryVar
                    ) +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(size = self$options$axisLabelSize),
                        legend.position = self$options$legendPosition,
                        strip.text = ggplot2::element_text(size = self$options$facetLabelSize)
                    )
                
                return(plot)
            }
        )
    )