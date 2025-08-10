#' @title Automatic Plot Selection Based on Variable Types
#'
#' @description 
#' This function automatically selects and generates the most appropriate statistical
#' visualization based on the data types of the selected variables. It supports both
#' independent and repeated measurements designs, with various plot types including
#' violin plots, scatter plots, bar charts, dot plots, and alluvial diagrams.
#'
#' The function uses ggstatsplot package for statistical visualizations and ggalluvial
#' for flow diagrams. Plot selection follows these rules:
#' - Factor vs Continuous: Violin plots (ggbetweenstats/ggwithinstats)
#' - Continuous vs Continuous: Scatter plots (ggscatterstats) 
#' - Factor vs Factor: Bar charts (ggbarstats) or Alluvial diagrams for repeated measures
#' - Continuous vs Factor: Dot plots (ggdotplotstats)
#'
#' @param data The data as a data frame
#' @param dep The dependent variable (y-axis, 1st measurement). Can be continuous or categorical
#' @param group The grouping variable (x-axis, 2nd measurement). Can be continuous or categorical
#' @param grvar Optional grouping variable for creating grouped plots across multiple panels
#' @param direction Measurement design type. Either "independent" for between-subjects 
#'   comparisons or "repeated" for within-subjects/repeated measures comparisons
#' @param distribution Statistical approach for analysis. Options:
#'   - "p" = parametric (assumes normal distribution)
#'   - "np" = nonparametric (distribution-free methods) 
#'   - "r" = robust (resistant to outliers)
#'   - "bf" = Bayes factor (Bayesian approach)
#' @param alluvsty Style for alluvial diagrams when both variables are factors in repeated design.
#'   - "t1" = ggalluvial style with stratum labels
#'   - "t2" = easyalluvial style with automatic variable selection
#' @param excl Logical. If TRUE, excludes rows with missing values before analysis
#'
#' @return A results object containing plots and explanatory text
#'
#' @details
#' The function intelligently selects plot types based on variable combinations:
#' 
#' **Independent Measurements:**
#' - Factor + Continuous → Violin plot with statistical comparisons
#' - Continuous + Continuous → Scatter plot with correlation analysis
#' - Factor + Factor → Bar chart with contingency table analysis
#' - Continuous + Factor → Cleveland dot plot
#'
#' **Repeated Measurements:**
#' - Factor + Continuous → Paired violin plot with within-subjects comparisons
#' - Continuous + Continuous → Scatter plot (correlation analysis)
#' - Factor + Factor → Alluvial diagram showing changes between time points
#' - Continuous + Factor → Cleveland dot plot
#'
#' Statistical tests are automatically selected based on the distribution parameter
#' and variable types. All plots include appropriate statistical annotations.
#'
#' @examples
#' \donttest{
#' # Basic usage with factor and continuous variables
#' statsplot2(
#'   data = mtcars,
#'   dep = "mpg",
#'   group = "cyl",
#'   direction = "independent",
#'   distribution = "p"
#' )
#' 
#' # Repeated measures design with alluvial diagram
#' statsplot2(
#'   data = survey_data,
#'   dep = "condition_baseline", 
#'   group = "condition_followup",
#'   direction = "repeated",
#'   alluvsty = "t1"
#' )
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @import ggstatsplot
#' @import ggalluvial
#' @importFrom easyalluvial alluvial_wide
#' @importFrom glue glue
#' @importFrom rlang sym
#'



statsplot2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "statsplot2Class",
        inherit = statsplot2Base,
        private = list(
            # Consolidated validation method (assumes non-NULL variables)
            .validateInputs = function() {
                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')
                
                # Validate variables exist
                if (!self$options$dep %in% names(self$data))
                    stop(paste('Variable', self$options$dep, 'not found in data. Available variables:', 
                              paste(names(self$data), collapse = ', ')))
                
                if (!self$options$group %in% names(self$data))
                    stop(paste('Variable', self$options$group, 'not found in data. Available variables:', 
                              paste(names(self$data), collapse = ', ')))
                
                mydep <- self$data[[self$options$dep]]
                mygroup <- self$data[[self$options$group]]
                
                # Check for minimum data requirements
                if (length(mydep) < 2 || length(mygroup) < 2) {
                    stop(paste('Insufficient data: At least 2 observations required for statistical analysis.',
                              'Current data size:', nrow(self$data), 'rows.',
                              'Suggestion: Ensure your dataset contains sufficient observations.'))
                }
                
                # Check for complete missing data
                if (all(is.na(mydep))) {
                    stop(paste('Dependent variable contains only missing values.',
                              'Suggestion: Check your data quality or select a different variable.'))
                } else if (all(is.na(mygroup))) {
                    stop(paste('Group variable contains only missing values.',
                              'Suggestion: Check your data quality or select a different variable.'))
                }
                
                # Check for valid factor levels
                if (is.factor(mydep) && length(levels(mydep)) < 1)
                    stop('Dependent variable has no valid factor levels')
                    
                if (is.factor(mygroup) && length(levels(mygroup)) < 1)
                    stop('Group variable has no valid factor levels')
                
                # Validate optional grouping variable
                if (!is.null(self$options$grvar)) {
                    if (!self$options$grvar %in% names(self$data)) {
                        stop(paste('Grouping variable', self$options$grvar, 'not found in data. Available variables:', 
                                  paste(names(self$data), collapse = ', ')))
                    }
                    
                    grvar_data <- self$data[[self$options$grvar]]
                    if (all(is.na(grvar_data))) {
                        stop(paste('Grouping variable contains only missing values.',
                                  'Suggestion: Check your data or use "Exclude Missing Values" option to automatically handle this.'))
                    }
                    
                    if (is.factor(grvar_data) && length(levels(grvar_data)) < 2) {
                        stop(paste('Grouping variable must have at least 2 levels for grouped analysis.',
                                  'Current levels:', paste(levels(grvar_data), collapse = ', '),
                                  'Suggestion: Use a different grouping variable with multiple categories.'))
                    }
                }
                
                # Validate distribution parameter
                distribution <- self$options$distribution
                if (is.null(distribution) || !distribution %in% c('p', 'np', 'r', 'bf')) {
                    stop('Invalid distribution parameter. Must be one of: p (parametric), np (nonparametric), r (robust), bf (bayes factor)')
                }
                
                # Validate direction parameter
                direction <- self$options$direction
                if (is.null(direction) || !direction %in% c('independent', 'repeated')) {
                    stop('Invalid direction parameter. Must be either "independent" or "repeated"')
                }
                
                # Validate alluvial style parameter if factor-factor comparison
                # Only validate if we have valid variable selections
                if (!is.null(self$options$dep) && !is.null(self$options$group)) {
                    mydep <- self$data[[self$options$dep]]
                    mygroup <- self$data[[self$options$group]]
                    if (is.factor(mydep) && is.factor(mygroup) && direction == "repeated") {
                        alluvsty <- self$options$alluvsty
                        if (is.null(alluvsty) || !alluvsty %in% c('t1', 't2')) {
                            stop(paste('Invalid alluvial style parameter for factor-factor repeated measures.',
                                      'Must be either "t1" (ggalluvial style) or "t2" (easyalluvial style).',
                                      'Current value:', alluvsty,
                                      'Suggestion: Select an appropriate alluvial diagram style from the options.'))
                        }
                    }
                }
                
                return(TRUE)
            },
            
            .run = function() {
                # Simple NULL check and early return
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    todo <- "
                    <br>Welcome to ClinicoPath
                    <br><br>
                    This tool will help you generate plots based on variable types.
                    <br><br>
                    This function uses ggstatsplot and ggalluvial packages. Please cite jamovi and the packages as given below.
                    "
                    self$results$todo$setContent(todo)
                    return()
                }
                
                # Clear todo message
                self$results$todo$setContent("")
                
                # Basic variable processing
                tryCatch({
                    mydep <- self$data[[self$options$dep]]
                    mygroup <- self$data[[self$options$group]]
                    
                    if (is.null(mydep) || is.null(mygroup)) {
                        return()
                    }
                    
                    # Simple type detection
                    dep_type <- if (inherits(mydep, "factor")) "factor" else "continuous"
                    group_type <- if (inherits(mygroup, "factor")) "factor" else "continuous"
                    direction <- self$options$direction
                    
                    # Generate explanation
                    if (direction == "independent") {
                        if (group_type == "factor" && dep_type == "continuous") {
                            stat_exp <- "Violin plot for comparing continuous variable between independent groups"
                        } else if (group_type == "continuous" && dep_type == "continuous") {
                            stat_exp <- "Scatter plot for correlation analysis between two continuous variables"
                        } else if (group_type == "factor" && dep_type == "factor") {
                            stat_exp <- "Bar chart for comparing categorical variables between groups"
                        } else if (group_type == "continuous" && dep_type == "factor") {
                            stat_exp <- "Dot plot for comparing categorical variable across continuous predictor"
                        } else {
                            stat_exp <- "Unsupported variable combination"
                        }
                    } else {
                        stat_exp <- "Repeated measures analysis selected"
                    }
                    
                    self$results$text4$setContent(stat_exp)
                    
                }, error = function(e) {
                    # If any error occurs, just show a message
                    self$results$text4$setContent("Error processing variables")
                })
            },
            
            # Generate plot based on variable types and design
            .generatePlot = function(data, dep_var, group_var, direction, distribution) {
                # Convert variable names to symbols for NSE
                dep_sym <- rlang::sym(dep_var)
                group_sym <- rlang::sym(group_var)
                
                # Get cached variable types and data for performance
                var_info <- self$.getVariableTypes(data, dep_var, group_var)
                plot_key <- paste(direction, var_info$group_type, var_info$dep_type, sep = "_")
                
                # Generate plots based on variable type combinations
                plot <- NULL
                
                if (direction == "independent") {
                    plot <- switch(
                        plot_key,
                        "independent_factor_continuous" = ggstatsplot::ggbetweenstats(
                            data = data, x = !!group_sym, y = !!dep_sym, type = distribution),
                        "independent_continuous_continuous" = ggstatsplot::ggscatterstats(
                            data = data, x = !!group_sym, y = !!dep_sym, type = distribution),
                        "independent_factor_factor" = ggstatsplot::ggbarstats(
                            data = data, main = !!dep_sym, condition = !!group_sym),
                        "independent_continuous_factor" = ggstatsplot::ggdotplotstats(
                            data = data, x = !!group_sym, y = !!dep_sym, type = distribution),
                        stop("Unsupported variable combination for independent analysis")
                    )
                } else if (direction == "repeated") {
                    if (plot_key == "repeated_factor_continuous") {
                        plot <- ggstatsplot::ggwithinstats(
                            data = data, x = !!group_sym, y = !!dep_sym, 
                            type = distribution, pairwise.comparisons = TRUE)
                    } else if (plot_key == "repeated_continuous_continuous") {
                        plot <- ggstatsplot::ggscatterstats(
                            data = data, x = !!group_sym, y = !!dep_sym, 
                            type = distribution, marginal = FALSE)
                    } else if (plot_key == "repeated_factor_factor") {
                        # Handle alluvial diagrams for factor-factor repeated measures
                        alluvsty <- self$options$alluvsty
                        
                        if (alluvsty == "t1") {
                            plotData <- data.frame(gr = var_info$mygroup, dp = var_info$mydep)
                            mydata_changes <- plotData %>%
                                dplyr::group_by(gr, dp) %>%
                                dplyr::tally(x = .)
                            
                            stratum <- ggalluvial::StatStratum
                            plot <- ggplot2::ggplot(data = mydata_changes,
                                                   ggplot2::aes(
                                                       axis1 = gr,
                                                       axis2 = dp,
                                                       y = n
                                                   )) +
                                ggplot2::scale_x_discrete(
                                    limits = c(group_var, dep_var),
                                    expand = c(.1, .05)
                                ) +
                                ggplot2::xlab(group_var) +
                                ggalluvial::geom_alluvium(ggplot2::aes(fill = gr, colour = gr)) +
                                ggalluvial::geom_stratum() +
                                ggalluvial::stat_stratum(geom = "stratum") +
                                ggplot2::geom_label(stat = stratum, infer.label = TRUE) +
                                ggplot2::theme_minimal()
                        } else if (alluvsty == "t2") {
                            plot <- easyalluvial::alluvial_wide(
                                data = data,
                                max_variables = 5,
                                fill_by = 'first_variable'
                            )
                        }
                    } else if (plot_key == "repeated_continuous_factor") {
                        plot <- ggstatsplot::ggdotplotstats(
                            data = data, x = !!group_sym, y = !!dep_sym, type = distribution)
                    } else {
                        stop("Unsupported variable combination for repeated measures analysis")
                    }
                } else {
                    stop("Invalid direction parameter")
                }
                
                return(plot)
            },
            
            # Cache variable types and data for performance optimization
            .getVariableTypes = function(data, dep_var, group_var) {
                mydep <- data[[dep_var]]
                mygroup <- data[[group_var]]
                list(
                    dep_type = if (inherits(mydep, "factor")) "factor" else "continuous",
                    group_type = if (inherits(mygroup, "factor")) "factor" else "continuous",
                    mydep = mydep,
                    mygroup = mygroup
                )
            },

            .plot = function(image, ggtheme, theme, ...) {
                # the plot function ----


                # Error messages ----

                if (is.null(self$options$dep) ||
                    is.null(self$options$group))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')
                
                # Validate package dependencies
                required_packages <- c('ggstatsplot', 'ggalluvial', 'easyalluvial')
                missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
                
                if (length(missing_packages) > 0) {
                    stop(paste('Required packages not available:', paste(missing_packages, collapse = ', ')))
                }
                
                # Validate variables exist
                if (!self$options$dep %in% names(self$data))
                    stop(paste('Variable', self$options$dep, 'not found in data'))
                
                if (!self$options$group %in% names(self$data))
                    stop(paste('Variable', self$options$group, 'not found in data'))



                mydep <- self$data[[self$options$dep]]
                mygroup <- self$data[[self$options$group]]
                
                # Validate minimum data requirements for plotting
                if (length(mydep) < 2 || length(mygroup) < 2)
                    stop('Insufficient data: At least 2 observations required for plotting')
                
                # Validate that variables have some non-missing data
                valid_dep <- sum(!is.na(mydep))
                valid_group <- sum(!is.na(mygroup))
                
                if (valid_dep < 2)
                    stop('Dependent variable has insufficient valid data (need at least 2 non-missing values)')
                    
                if (valid_group < 2)
                    stop('Group variable has insufficient valid data (need at least 2 non-missing values)')

                contin <- c("integer", "numeric", "double")






                # direction ----

                direction <- self$options$direction
                
                # Validate direction parameter
                if (is.null(direction) || !direction %in% c('independent', 'repeated')) {
                    stop('Invalid direction parameter. Must be either "independent" or "repeated"')
                }

                # Get parameter values directly
                direction <- self$options$direction
                distribution <- self$options$distribution



                # Prepare data
                mydata <- self$data
                excl <- self$options$excl
                if (excl) {mydata <- jmvcore::naOmit(mydata)}
                
                # Get variable names
                dep <- self$options$dep
                group <- self$options$group
                grvar <- self$options$grvar





                if ( ! is.null(self$options$grvar) ) {
                    grvar <- self$options$grvar
                    
                    # Validate grouping variable exists
                    if (!grvar %in% names(mydata)) {
                        stop(paste('Grouping variable', grvar, 'not found in data'))
                    }
                    
                    # Validate grouping variable has valid data
                    grvar_data <- mydata[[grvar]]
                    if (all(is.na(grvar_data))) {
                        stop('Grouping variable contains only missing values')
                    }
                    
                    if (is.factor(grvar_data) && length(levels(grvar_data)) < 2) {
                        stop(paste('Grouping variable must have at least 2 levels for grouped analysis.',
                                  'Current levels:', paste(levels(grvar_data), collapse = ', '),
                                  'Suggestion: Use a different grouping variable with multiple categories.'))
                    }
                }



                # Determine variable types for plot selection
                contin <- c("integer", "numeric", "double") 
                dep_type <- if (inherits(mydep, "factor")) "factor" else "continuous"
                group_type <- if (inherits(mygroup, "factor")) "factor" else "continuous"
                plot_key <- paste(direction, group_type, dep_type, sep = "_")
                
                # Generate plots based on variable type combinations
                if (direction == "independent") {
                    plot <- switch(
                        plot_key,
                        "independent_factor_continuous" = ggstatsplot::ggbetweenstats(
                            data = mydata, x = !!group, y = !!dep, type = distribution),
                        "independent_continuous_continuous" = ggstatsplot::ggscatterstats(
                            data = mydata, x = !!group, y = !!dep, type = distribution),
                        "independent_factor_factor" = ggstatsplot::ggbarstats(
                            data = mydata, main = !!dep, condition = !!group),
                        "independent_continuous_factor" = ggstatsplot::ggdotplotstats(
                            data = mydata, x = !!group, y = !!dep, type = distribution),
                        stop("Unsupported variable combination for independent analysis")
                    )


                } else if (direction == "repeated") {
                    if (plot_key == "repeated_factor_continuous") {
                        plot <- ggstatsplot::ggwithinstats(
                            data = mydata, x = !!group, y = !!dep, 
                            type = distribution, pairwise.comparisons = TRUE)



                    } else if (plot_key == "repeated_continuous_continuous") {
                        plot <- ggstatsplot::ggscatterstats(
                            data = mydata, x = !!group, y = !!dep, 
                            type = distribution, marginal = FALSE)









                    } else if (plot_key == "repeated_factor_factor") {
                        # Handle alluvial diagrams for factor-factor repeated measures




                        alluvsty <- self$options$alluvsty

                        if (alluvsty == "t1") {

    

                        plotData <- data.frame(gr = mygroup,
                                               dp = mydep)


                        mydata_changes <- plotData %>%
                            dplyr::group_by(gr, dp) %>%
                            dplyr::tally(x = .)










                        stratum <- ggalluvial::StatStratum

                        plot <- ggplot2::ggplot(data = mydata_changes,
                                                ggplot2::aes(
                                                    axis1 = gr,
                                                    axis2 = dp,
                                                    y = n
                                                )) +
                            ggplot2::scale_x_discrete(
                                limits = c(self$options$group, self$options$dep),
                                expand = c(.1, .05)
                            ) +
                            ggplot2::xlab(self$options$group) +
                            ggalluvial::geom_alluvium(ggplot2::aes(fill = gr,
                                                                   colour = gr)) +
                            ggalluvial::geom_stratum() +
                            ggalluvial::stat_stratum(geom = "stratum") +
                            ggplot2::geom_label(stat = stratum, infer.label = TRUE) +

                            ggplot2::theme_minimal()




                        } else if (alluvsty == "t2") {


                            plot <-
                                easyalluvial::alluvial_wide( data = mydata,
                                                             max_variables = 5,
                                                             fill_by = 'first_variable'
                                                             )


                        }




                    } else if (inherits(mygroup, contin) &&
                               inherits(mydep, "factor")) {
                        
                        plot <- ggstatsplot::ggdotplotstats(
                            data = mydata, x = !!group, y = !!dep, type = distribution)
                    } else {
                        stop("Unsupported variable combination for repeated measures analysis")
                    }

                } else {
                    stop("Invalid direction parameter")
                }

                # Handle grouped analysis if grouping variable specified
                if ( ! is.null(self$options$grvar) ) {


                    plot <- ggstatsplot::grouped_ggbetweenstats(
                        data = mydata,
                        x = !!group,
                        y = !!dep,
                        grouping.var = !!grvar,
                        pairwise.comparisons = TRUE,
                        p.adjust.method = "bonferroni"
                    )






















                }
















                # Validate plot object and print
                if (is.null(plot)) {
                    stop('Plot generation failed: No valid plot created')
                }
                
                if (is.character(plot) && length(plot) == 1) {
                    stop(paste('Plot generation failed:', plot))
                }
                
                # Try to print plot with error handling
                tryCatch({
                    print(plot)
                    TRUE
                }, error = function(e) {
                    stop(paste('Error displaying plot:', e$message))
                })

            }


        )
    )
