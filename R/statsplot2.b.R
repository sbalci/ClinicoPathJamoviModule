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
#' @param originaltheme Logical. If TRUE, uses original ggplot2 themes instead of ggstatsplot themes
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
#'



statsplot2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "statsplot2Class",
        inherit = statsplot2Base,
        private = list(
            .run = function() {

                StatStratum <- ggalluvial::StatStratum


                if (is.null(self$options$dep) ||
                    is.null(self$options$group)) {

                    todo <- glue::glue(
                "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate plots based on variable types.
                <br><br>
                This function uses ggstatsplot and ggalluvial packages. Please cite jamovi and the packages as given below.
                "
                    )

                    self$results$todo$setContent(todo)

                    return()

                } else {
                    todo <- ""
                    self$results$todo$setContent(todo)


                    if (nrow(self$data) == 0)
                        stop('Data contains no (complete) rows')

                    # Validate variables exist
                    if (!self$options$dep %in% names(self$data))
                        stop(paste('Variable', self$options$dep, 'not found in data'))
                    
                    if (!self$options$group %in% names(self$data))
                        stop(paste('Variable', self$options$group, 'not found in data'))

                    mydep <- self$data[[self$options$dep]]
                    mygroup <- self$data[[self$options$group]]
                    
                    # Check for minimum data requirements
                    if (length(mydep) < 2 || length(mygroup) < 2)
                        stop('Insufficient data: At least 2 observations required')
                    
                    # Check for complete missing data
                    if (all(is.na(mydep)) || all(is.na(mygroup)))
                        stop('Variables contain only missing values')
                    
                    # Check for valid factor levels
                    if (is.factor(mydep) && length(levels(mydep)) < 1)
                        stop('Dependent variable has no valid factor levels')
                        
                    if (is.factor(mygroup) && length(levels(mygroup)) < 1)
                        stop('Group variable has no valid factor levels')

                    contin <- c("integer", "numeric", "double")

                    # Validate and set distribution parameter
                    distribution <- self$options$distribution
                    if (is.null(distribution) || !distribution %in% c('p', 'np', 'r', 'bf')) {
                        stop('Invalid distribution parameter. Must be one of: p (parametric), np (nonparametric), r (robust), bf (bayes factor)')
                    }
                    distribution <- jmvcore::composeTerm(distribution)

                    # Validate and set direction parameter  
                    direction <- self$options$direction
                    if (is.null(direction) || !direction %in% c('independent', 'repeated')) {
                        stop('Invalid direction parameter. Must be either "independent" or "repeated"')
                    }
                    direction <- jmvcore::composeTerm(direction)
                    
                    # Validate alluvial style parameter if factor-factor comparison
                    if (is.factor(mydep) && is.factor(mygroup) && direction == "repeated") {
                        alluvsty <- self$options$alluvsty
                        if (is.null(alluvsty) || !alluvsty %in% c('t1', 't2')) {
                            stop('Invalid alluvial style parameter. Must be either "t1" or "t2"')
                        }
                    }




                # independent ----

                    # Determine variable types for simplified logic
                    dep_type <- if (inherits(mydep, "factor")) "factor" else "continuous"
                    group_type <- if (inherits(mygroup, "factor")) "factor" else "continuous"
                    
                    # Generate explanation text based on combination
                    if (direction == "independent") {
                        stat_exp <- switch(
                            paste(group_type, dep_type, sep = "_"),
                            "factor_continuous" = "Violin plot for comparing continuous variable between independent groups",
                            "continuous_continuous" = "Scatter plot for correlation analysis between two continuous variables", 
                            "factor_factor" = "Bar chart for comparing categorical variables between groups",
                            "continuous_factor" = "Dot plot for comparing categorical variable across continuous predictor",
                            "Unsupported variable combination"
                        )
                    } else if (direction == "repeated") {
                        stat_exp <- switch(
                            paste(group_type, dep_type, sep = "_"),
                            "factor_continuous" = "Violin plot for comparing continuous variable in repeated measures design",
                            "continuous_continuous" = "Scatter plot for repeated measures correlation analysis", 
                            "factor_factor" = "Alluvial diagram showing changes between categorical measurements",
                            "continuous_factor" = "Dot plot for repeated measures with categorical outcome",
                            "Unsupported variable combination"
                        )
                    } else {
                        stat_exp <- "Please select a valid direction (independent or repeated)"
                    }

                    self$results$text4$setContent(stat_exp)



                }
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

                # Validate and construct distribution parameter
                distribution_param <- self$options$distribution
                if (is.null(distribution_param) || !distribution_param %in% c('p', 'np', 'r', 'bf')) {
                    stop('Invalid distribution parameter. Must be one of: p, np, r, bf')
                }
                distribution <- jmvcore::constructFormula(terms = distribution_param)



                mydata <- self$data



                excl <- self$options$excl

                if (excl) {mydata <- jmvcore::naOmit(mydata)}




                dep <- self$options$dep

                group <- self$options$group

                dep <- jmvcore::composeTerms(listOfComponents = dep)

                group <- jmvcore::composeTerm(components = group)





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
                        stop('Grouping variable must have at least 2 levels for grouped analysis')
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
                            data = mydata, x = !!dep, y = !!group),
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
                            data = mydata, x = !!dep, y = !!group, type = distribution)
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
