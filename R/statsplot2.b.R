#' @title Plots and Graphs Based on Variable Types
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr



statsplot2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "statsplot2Class",
        inherit = statsplot2Base,
        private = list(
            # Private function to detect variable types and analysis parameters
            .detectAnalysisType = function() {
                # Return early if no variables selected
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    return(NULL)
                }
                
                # Get variable data
                mydep <- self$data[[self$options$dep]]
                mygroup <- self$data[[self$options$group]]
                
                # Define continuous types
                contin <- c("integer", "numeric", "double")
                
                # Determine variable types using inherits with contin array
                dep_type <- if (inherits(mydep, "factor")) {
                    "factor"
                } else if (inherits(mydep, contin)) {
                    "continuous"
                } else {
                    "unknown"
                }
                
                group_type <- if (inherits(mygroup, "factor")) {
                    "factor"
                } else if (inherits(mygroup, contin)) {
                    "continuous"
                } else {
                    "unknown"
                }
                
                # Get other options
                direction <- self$options$direction
                distribution <- self$options$distribution
                alluvsty <- self$options$alluvsty
                
                # Create analysis type identifier
                plot_type <- paste(direction, group_type, dep_type, sep = "_")
                
                # Return analysis information
                list(
                    dep_type = dep_type,
                    group_type = group_type,
                    direction = direction,
                    distribution = distribution,
                    alluvsty = alluvsty,
                    plot_type = plot_type,
                    dep_var = self$options$dep,
                    group_var = self$options$group,
                    grvar = self$options$grvar
                )
            },

            .generateExplanationMessage = function(analysis_info) {
                # Format variable descriptions with their types
                dep_desc <- glue::glue("{analysis_info$dep_var} ({analysis_info$dep_type})")
                group_desc <- glue::glue("{analysis_info$group_var} ({analysis_info$group_type})")
                
                # Generate explanation message based on plot type
                base_message <- switch(analysis_info$plot_type,
                    "independent_factor_continuous" = glue::glue(
                        "You have selected to use a violin plot to compare {dep_desc} between independent groups defined by {group_desc}."
                    ),
                    "independent_continuous_continuous" = glue::glue(
                        "You have selected to use a scatter plot to examine the relationship between {group_desc} and {dep_desc}."
                    ),
                    "independent_factor_factor" = glue::glue(
                        "You have selected to use a bar chart to compare {dep_desc} across categories of {group_desc}."
                    ),
                    "independent_continuous_factor" = glue::glue(
                        "You have selected to compare {dep_desc} with {group_desc}. Note: Consider switching variables for a more appropriate visualization."
                    ),
                    "repeated_factor_continuous" = glue::glue(
                        "You have selected to use a violin plot to compare {dep_desc} between repeated measurements defined by {group_desc}."
                    ),
                    "repeated_continuous_continuous" = glue::glue(
                        "Currently this tool does not support scatterplots for repeated measurements of {group_desc} and {dep_desc}. You may refer to R-project rmcorr package."
                    ),
                    "repeated_factor_factor" = glue::glue(
                        "You have selected to compare repeated measurements of {dep_desc} and {group_desc} using an alluvial diagram."
                    ),
                    "repeated_continuous_factor" = glue::glue(
                        "Please switch the variables: {dep_desc} and {group_desc} to generate an appropriate plot."
                    ),
                    # Default case for unknown combinations or types
                    glue::glue("Variable type combination not supported: {dep_desc} vs {group_desc} with {analysis_info$direction} design. Please ensure variables are either factors or numeric types.")
                )
                
                # Add notes about option applicability
                notes <- character(0)
                
                # Note about statistical approach
                if (analysis_info$dep_type == "factor" && analysis_info$group_type == "factor") {
                    notes <- c(notes, "Note: Statistical approach option does not apply to categorical comparisons.")
                }
                
                # Note about alluvial style
                if (analysis_info$plot_type == "repeated_factor_factor") {
                    notes <- c(notes, "Alluvial style option is now available for this repeated categorical comparison.")
                } else if (analysis_info$direction == "repeated") {
                    notes <- c(notes, "Alluvial style option only applies to repeated factor vs factor comparisons.")
                }
                
                # Combine messages
                if (length(notes) > 0) {
                    stat_exp <- glue::glue("{base_message}\n\n{paste(notes, collapse = '\n')}")
                } else {
                    stat_exp <- base_message
                }
                
                return(stat_exp)
            },

            .run = function() {

                StatStratum <- ggalluvial::StatStratum

                analysis_info <- NULL

                # Get analysis type information
                analysis_info <- private$.detectAnalysisType()
                
                # If no variables selected, show initial message
                if (is.null(analysis_info)) {

                    todo <- glue::glue(
                "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate plots based on variable types.
                <br><br>
                This function uses ggstatsplot and ggalluvial packages. Please cite jamovi and the packages as given below.
                "
                    )

                    self$results$todo$setVisible(TRUE)
                    self$results$todo$setContent(todo)

                    return()

                }
                
                # Clear todo message
                self$results$todo$setVisible(FALSE)
                
                # Check for data
                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                
                # Generate explanation message using the new function
                stat_exp <- private$.generateExplanationMessage(analysis_info)
                
                # Set the explanation message in results
                self$results$text4$setContent(stat_exp)

            },
            
            .init = function() {
                # Initialize and set option visibility based on selected variables
                
                # Get analysis type information
                analysis_info <- private$.detectAnalysisType()
                
                if (is.null(analysis_info)) {
                    # No variables selected - hide conditional options
                    # Keep all options visible initially
                    return()
                }
                
                # Determine which options should be enabled
                
                # 1. Study Design (direction) - Always relevant when variables are selected
                # Could be disabled for specific unsupported combinations
                
                # 2. Statistical Approach (distribution) - Only relevant for quantitative analyses
                # Enable for: continuous outcomes, disable for pure categorical comparisons
                enable_distribution <- (
                    analysis_info$dep_type == "continuous" || 
                    analysis_info$group_type == "continuous"
                )
                
                # 3. Alluvial Style - Only relevant for repeated factor vs factor
                enable_alluvial <- (
                    analysis_info$direction == "repeated" && 
                    analysis_info$dep_type == "factor" && 
                    analysis_info$group_type == "factor"
                )
                
                # Apply visibility rules (if UI supports it)
                # Note: Jamovi may not support dynamic enable/disable in all versions
                # This is primarily for documentation of when options are relevant
                
                if (!enable_distribution) {
                    # Statistical approach not relevant for pure categorical comparisons
                    # User should be aware this option doesn't affect factor vs factor plots
                }
                
                if (!enable_alluvial) {
                    # Alluvial style only matters for repeated factor vs factor
                    # Hide or disable this option for other combinations
                }
                
                # You could also add informative messages
                if (analysis_info$plot_type == "independent_factor_factor" && 
                    !is.null(self$options$distribution) && 
                    self$options$distribution != "p") {
                    # Note: Statistical approach doesn't affect bar charts for factor comparisons
                }
                
                if (analysis_info$plot_type != "repeated_factor_factor" && 
                    !is.null(self$options$alluvsty)) {
                    # Note: Alluvial style only applies to repeated factor comparisons
                }
            },
            
            # Prepare data for plotting (handle NA exclusion, term composition)
            .prepareDataForPlot = function(analysis_info) {
                # Get base data
                mydata <- self$data
                
                # Handle NA exclusion if requested
                if (self$options$excl) {
                    mydata <- jmvcore::naOmit(mydata)
                }
                
                # Prepare composed terms for use with ggstatsplot
                # Note: We use simple strings instead of composed terms to avoid NSE issues
                dep_var <- analysis_info$dep_var
                group_var <- analysis_info$group_var
                grvar <- analysis_info$grvar
                
                # Return prepared data and variable names
                list(
                    data = mydata,
                    dep = dep_var,
                    group = group_var,
                    grvar = grvar,
                    distribution = analysis_info$distribution,
                    alluvsty = analysis_info$alluvsty
                )
            },
            
            # Main dispatcher for plot generation
            .generatePlot = function(analysis_info, prepared_data) {
                # Check if grouped plot is needed
                if (!is.null(prepared_data$grvar)) {
                    return(private$.plotGrouped(analysis_info, prepared_data))
                }
                
                # Dispatch to appropriate plot function based on plot type
                plot <- switch(analysis_info$plot_type,
                    "independent_factor_continuous" = private$.plotBetweenStats(prepared_data),
                    "independent_continuous_continuous" = private$.plotScatterStats(prepared_data),
                    "independent_factor_factor" = private$.plotBarStats(prepared_data),
                    "independent_continuous_factor" = private$.plotDotplotStats(prepared_data),
                    "repeated_factor_continuous" = private$.plotWithinStats(prepared_data),
                    "repeated_factor_factor" = private$.plotAlluvial(prepared_data),
                    "repeated_continuous_continuous" = NULL,  # Not supported
                    "repeated_continuous_factor" = NULL,  # Not supported
                    NULL  # Default case
                )
                
                return(plot)
            },
            
            # Plot function for between-subjects comparisons (factor vs continuous)
            .plotBetweenStats = function(prepared_data) {
                plot <- ggstatsplot::ggbetweenstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$group),
                    y = !!rlang::sym(prepared_data$dep),
                    type = prepared_data$distribution
                )
                return(plot)
            },
            
            # Plot function for scatter plots (continuous vs continuous)
            .plotScatterStats = function(prepared_data) {
                plot <- ggstatsplot::ggscatterstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$group),
                    y = !!rlang::sym(prepared_data$dep),
                    type = prepared_data$distribution
                )
                return(plot)
            },
            
            # Plot function for bar charts (factor vs factor)
            .plotBarStats = function(prepared_data) {
                plot <- ggstatsplot::ggbarstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$dep),
                    y = !!rlang::sym(prepared_data$group)
                )
                return(plot)
            },
            
            # Plot function for dot plots (continuous vs factor)
            .plotDotplotStats = function(prepared_data) {
                # For ggdotplotstats: x = continuous, y = factor
                # The combination is "independent_continuous_factor" meaning:
                # group is continuous, dep is factor
                plot <- ggstatsplot::ggdotplotstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$group),  # continuous variable
                    y = !!rlang::sym(prepared_data$dep)     # factor variable
                )
                return(plot)
            },
            
            # Plot function for within-subjects comparisons (repeated measures)
            .plotWithinStats = function(prepared_data) {
                plot <- ggstatsplot::ggwithinstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$group),
                    y = !!rlang::sym(prepared_data$dep),
                    type = prepared_data$distribution,
                    pairwise.comparisons = TRUE
                )
                return(plot)
            },
            
            # Plot function for alluvial diagrams (factor vs factor, repeated)
            .plotAlluvial = function(prepared_data) {
                if (prepared_data$alluvsty == "t1") {
                    # Use ggalluvial
                    plot <- private$.plotAlluvialGG(prepared_data)
                } else {
                    # Use easyalluvial
                    plot <- private$.plotAlluvialEasy(prepared_data)
                }
                return(plot)
            },
            
            # ggalluvial implementation
            .plotAlluvialGG = function(prepared_data) {
                # Create plot data
                plotData <- data.frame(
                    gr = prepared_data$data[[prepared_data$group]],
                    dp = prepared_data$data[[prepared_data$dep]]
                )
                
                # Tally the combinations
                mydata_changes <- plotData %>%
                    dplyr::group_by(gr, dp) %>%
                    dplyr::tally()
                
                # Create alluvial plot
                stratum <- ggalluvial::StatStratum
                
                plot <- ggplot2::ggplot(
                    data = mydata_changes,
                    ggplot2::aes(axis1 = gr, axis2 = dp, y = n)
                ) +
                    ggplot2::scale_x_discrete(
                        limits = c(prepared_data$group, prepared_data$dep),
                        expand = c(.1, .05)
                    ) +
                    ggplot2::xlab(prepared_data$group) +
                    ggalluvial::geom_alluvium(ggplot2::aes(fill = gr, colour = gr)) +
                    ggalluvial::geom_stratum() +
                    ggalluvial::stat_stratum(geom = "stratum") +
                    ggplot2::geom_label(stat = stratum, infer.label = TRUE) +
                    ggplot2::theme_minimal()
                
                return(plot)
            },
            
            # easyalluvial implementation
            .plotAlluvialEasy = function(prepared_data) {
                plot <- easyalluvial::alluvial_wide(
                    data = prepared_data$data,
                    max_variables = 5,
                    fill_by = 'first_variable'
                )
                return(plot)
            },
            
            # Grouped plots for when grvar is specified
            .plotGrouped = function(analysis_info, prepared_data) {
                # Currently only supporting grouped_ggbetweenstats
                # Can be extended for other grouped plot types
                if (analysis_info$plot_type == "independent_factor_continuous") {
                    plot <- ggstatsplot::grouped_ggbetweenstats(
                        data = prepared_data$data,
                        x = !!rlang::sym(prepared_data$group),
                        y = !!rlang::sym(prepared_data$dep),
                        grouping.var = !!rlang::sym(prepared_data$grvar),
                        pairwise.comparisons = TRUE,
                        p.adjust.method = "bonferroni"
                    )
                } else {
                    # For other types, return single plot for now
                    plot <- private$.generatePlot(analysis_info, prepared_data)
                }
                return(plot)
            },

            .plot = function(image, ggtheme, theme, ...) {
                # the plot function ----
                
                # Get analysis type information
                analysis_info <- private$.detectAnalysisType()
                
                # Return early if no variables selected
                if (is.null(analysis_info)) {
                    return()
                }
                
                # Check for data
                if (nrow(self$data) == 0) {
                    stop('Data contains no (complete) rows')
                }
                
                # Check if plot type is supported
                if (analysis_info$plot_type %in% c("repeated_continuous_continuous", "repeated_continuous_factor")) {
                    # These combinations are not supported
                    return()
                }
                
                # Prepare data for plotting
                prepared_data <- private$.prepareDataForPlot(analysis_info)
                
                # Generate the plot
                plot <- private$.generatePlot(analysis_info, prepared_data)
                
                # Return the plot
                if (!is.null(plot)) {
                    print(plot)
                    return(TRUE)
                } else {
                    return()
                }
            }

        )
    )
