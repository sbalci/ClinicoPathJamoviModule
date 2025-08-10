#' @title Bar Charts
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom digest digest
#' @importFrom purrr map imap
#' @importFrom rlang sym %||%
#' @importFrom glue glue
#'

jjbarstatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjbarstatsClass",
        inherit = jjbarstatsBase,
        private = list(
            # Cache variables for performance
            .cached_data = NULL,
            .data_hash = NULL,
            .validation_passed = FALSE,
            
            # init ----

            .init = function() {

                deplen <- length(self$options$dep)
                self$results$plot$setSize(650, deplen * 450)

                if (!is.null(self$options$grvar)) {

                    mydata <- self$data

                    grvar <-  self$options$grvar

                    num_levels <- nlevels(
                        as.factor(mydata[[grvar]])
                    )

                    self$results$plot2$setSize(num_levels * 650, deplen * 450)

                }

            },

            # Helper Methods ----

            .validateVariables = function() {
                dep_vars <- self$options$dep
                group_var <- self$options$group
                
                # Check if variables exist
                all_vars <- c(dep_vars, group_var)
                if (!is.null(self$options$grvar)) {
                    all_vars <- c(all_vars, self$options$grvar)
                }
                
                missing_vars <- all_vars[!all_vars %in% names(self$data)]
                if (length(missing_vars) > 0) {
                    stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
                }
                
                # Check that variables are appropriate for bar charts (categorical)
                for (var in all_vars) {
                    if (!is.null(var)) {
                        var_class <- class(self$data[[var]])
                        if (!any(c("factor", "character", "logical") %in% var_class)) {
                            # Try to convert numeric to factor if it has few unique values
                            if (is.numeric(self$data[[var]])) {
                                unique_vals <- length(unique(self$data[[var]], na.rm = TRUE))
                                if (unique_vals > 10) {
                                    stop(paste("Variable '", var, "' appears to be continuous (", unique_vals, 
                                             " unique values). Bar charts are for categorical data. Consider converting to groups first."))
                                }
                            }
                        }
                    }
                }
                
                # Enhanced validation for statistical tests
                private$.validateStatisticalRequirements(dep_vars, group_var)
                
                return(TRUE)
            },

            .validateStatisticalRequirements = function(dep_vars, group_var) {
                # Check minimum group sizes for statistical tests
                if (!is.null(group_var) && group_var %in% names(self$data)) {
                    group_sizes <- table(self$data[[group_var]], useNA = "no")
                    
                    if (any(group_sizes < 5)) {
                        small_groups <- names(group_sizes[group_sizes < 5])
                        warning(paste("Small group sizes detected (", paste(paste(small_groups, ":", group_sizes[small_groups]), collapse = ", "),
                                    "). Chi-square tests require minimum 5 observations per group for reliable results."))
                    }
                    
                    if (length(group_sizes) < 2) {
                        stop("Grouping variable must have at least 2 categories for comparison.")
                    }
                }
                
                # Check dependent variables have sufficient variation
                for (dep_var in dep_vars) {
                    if (!is.null(dep_var) && dep_var %in% names(self$data)) {
                        dep_levels <- table(self$data[[dep_var]], useNA = "no")
                        if (length(dep_levels) < 2) {
                            stop(paste("Variable '", dep_var, "' has insufficient variation (only", length(dep_levels), "level). Need at least 2 categories."))
                        }
                    }
                }
            },

            .getCachedData = function() {
                # Create hash of current data and options state
                current_hash <- digest::digest(list(
                    data_dim = dim(self$data),
                    data_names = names(self$data),
                    options = list(
                        dep = self$options$dep,
                        group = self$options$group,
                        grvar = self$options$grvar,
                        excl = self$options$excl
                    )
                ), algo = "md5")
                
                # Return cached data if hash matches and validation passed
                if (!is.null(private$.cached_data) && 
                    !is.null(private$.data_hash) &&
                    private$.data_hash == current_hash && 
                    private$.validation_passed) {
                    return(private$.cached_data)
                }
                
                # Validate and prepare fresh data
                private$.validateVariables()
                private$.cached_data <- private$.prepareData()
                private$.data_hash <- current_hash
                private$.validation_passed <- TRUE
                
                return(private$.cached_data)
            },

            .selectTheme = function(ggtheme) {
                if (self$options$originaltheme) {
                    return(ggstatsplot::theme_ggstatsplot())
                } else {
                    return(ggtheme)
                }
            },

            .prepareData = function() {
                mydata <- self$data
                
                # Handle missing data based on user preference
                if (self$options$excl) {
                    # Remove rows with any missing values in relevant variables
                    relevant_vars <- c(self$options$dep, self$options$group)
                    if (!is.null(self$options$grvar)) {
                        relevant_vars <- c(relevant_vars, self$options$grvar)
                    }
                    mydata <- mydata[complete.cases(mydata[relevant_vars]), ]
                } else {
                    # Let ggstatsplot handle NAs (it will exclude them with warnings)
                    mydata <- mydata
                }
                
                if (nrow(mydata) == 0) {
                    stop('No complete data rows available after handling missing values. Please check your data or change the "Exclude Missing (NA)" setting.')
                }
                
                return(mydata)
            },

            .createBarPlot = function(data, dep_var, ggtheme, grouped = FALSE, progress_label = NULL) {
                # Progress indicator
                if (!is.null(progress_label)) {
                    private$.checkpoint()
                }
                
                # Performance optimization: Disable expensive features for large datasets
                n_groups <- length(unique(data[[self$options$group]]))
                n_total <- nrow(data)
                
                # Auto-disable pairwise for large group counts (performance)
                use_pairwise <- self$options$pairwisecomparisons
                if (use_pairwise && n_groups > 10) {
                    warning("Pairwise comparisons disabled for performance (>10 groups). Set manually to override.")
                    use_pairwise <- FALSE
                }
                
                # Base arguments for ggstatsplot functions with performance optimizations
                base_args <- list(
                    data = data,
                    x = rlang::sym(dep_var),
                    y = rlang::sym(self$options$group),
                    type = self$options$typestatistics,
                    pairwise.comparisons = use_pairwise,
                    pairwise.display = self$options$pairwisedisplay,
                    p.adjust.method = self$options$padjustmethod,
                    results.subtitle = if (!is.null(self$options$resultssubtitle)) self$options$resultssubtitle else TRUE,
                    messages = if (!is.null(self$options$messages)) self$options$messages else FALSE
                )
                
                # Enhanced error handling with context preservation
                tryCatch({
                    if (grouped) {
                        # Add grouping variable for grouped analysis
                        base_args$grouping.var <- rlang::sym(self$options$grvar)
                        base_args$ggtheme <- private$.selectTheme(ggtheme)
                        base_args$messages <- FALSE  # Reduce console clutter
                        
                        return(do.call(ggstatsplot::grouped_ggbarstats, base_args))
                    } else {
                        # Standard bar chart
                        plot <- do.call(ggstatsplot::ggbarstats, base_args)
                        return(plot + private$.selectTheme(ggtheme))
                    }
                }, error = function(e) {
                    # Preserve original error context with enhanced information
                    original_error <- conditionMessage(e)
                    context_info <- paste0(
                        "Variable: ", dep_var, 
                        ", Groups: ", length(unique(data[[self$options$group]])),
                        ", N: ", nrow(data),
                        if (grouped) paste0(", Split by: ", self$options$grvar) else ""
                    )
                    
                    stop(paste0("Bar chart creation failed for ", progress_label %||% dep_var, 
                               ". Context: ", context_info, 
                               ". Original error: ", original_error),
                         call. = FALSE)
                })
            },

            .createMultiplePlots = function(data, dep_vars, ggtheme, grouped = FALSE) {
                # Progress indicator for multiple plots
                private$.checkpoint()
                
                # More memory-efficient symbol creation
                dep_symbols <- purrr::map(dep_vars, ~ rlang::sym(.x))
                
                # Create plots with progress tracking
                plotlist <- purrr::imap(dep_symbols, ~ {
                    progress_label <- paste0("plot ", .y, "/", length(dep_symbols), " (", dep_vars[.y], ")")
                    private$.createBarPlot(
                        data = data, 
                        dep_var = dep_vars[.y], 
                        ggtheme = ggtheme, 
                        grouped = grouped,
                        progress_label = progress_label
                    )
                })
                
                # Combine plots
                return(ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(ncol = 1)
                ))
            }

            # run ----
            ,
            .run = function() {
                # Initial Message ----
                if (is.null(self$options$dep) ||
                    is.null(self$options$group)) {
                    
                    todo <- glue::glue(
                        "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Bar Charts with statistical testing.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html' target='_blank'>ggbarstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html' target='_blank'>grouped_ggbarstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                    )

                    self$results$todo$setContent(todo)
                    return()

                } else {
                    # Enhanced data validation with better error messages and caching
                    tryCatch({
                        # Basic data check
                        if (nrow(self$data) == 0) {
                            stop('Dataset is empty. Please ensure your data contains observations.')
                        }
                        
                        # Use cached data validation and preparation
                        start_time <- Sys.time()
                        prepared_data <- private$.getCachedData()
                        prep_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
                        
                        # Enhanced success message with timing and caching info
                        cache_status <- if (private$.validation_passed && !is.null(private$.cached_data)) {
                            " (cached)"
                        } else {
                            " (fresh validation)"
                        }
                        
                        # Performance warnings
                        perf_warning <- ""
                        n_groups <- length(unique(prepared_data[[self$options$group]]))
                        if (self$options$pairwisecomparisons && n_groups > 5) {
                            perf_warning <- paste0("<br>⚠️ <b>Performance Note:</b> Pairwise comparisons with ", n_groups, 
                                                 " groups may be slow. Consider disabling for faster results.<br>")
                        }
                        if (self$options$typestatistics == "bayes") {
                            perf_warning <- paste0(perf_warning, 
                                                 "<br>⚠️ <b>Performance Note:</b> Bayesian analysis is computationally intensive.<br>")
                        }
                        
                        todo <- glue::glue(
                            "<br>Bar chart analysis comparing {paste(self$options$dep, collapse=', ')} by {self$options$group}{if(!is.null(self$options$grvar)) paste0(', grouped by ', self$options$grvar) else ''}.<br>
                            <br>Data prepared: {nrow(prepared_data)} observations{if(!self$options$excl) ' (missing values will be handled by statistical functions)' else ' (complete cases only)'}{cache_status}.<br>
                            {perf_warning}
                            {if(prep_time > 0.1) paste0('<br>Preparation time: ', prep_time, ' seconds.<br>') else ''}
                            <hr>"
                        )
                        
                        self$results$todo$setContent(todo)
                        
                    }, error = function(e) {
                        # Reset cache on error
                        private$.cached_data <- NULL
                        private$.validation_passed <- FALSE
                        
                        # Enhanced error reporting with more context
                        error_context <- ""
                        if (grepl("continuous", e$message, ignore.case = TRUE)) {
                            error_context <- "<br>💡 <b>Tip:</b> Use Data > Transform to create categorical groups from continuous variables.<br>"
                        } else if (grepl("minimum|group size", e$message, ignore.case = TRUE)) {
                            error_context <- "<br>💡 <b>Tip:</b> Consider combining small categories or collecting more data.<br>"
                        } else if (grepl("variation|level", e$message, ignore.case = TRUE)) {
                            error_context <- "<br>💡 <b>Tip:</b> Ensure your variables have multiple categories for comparison.<br>"
                        }
                        
                        error_msg <- glue::glue(
                            "<br>❌ <b>Error in Bar Chart Analysis:</b><br>
                            <br>{e$message}<br>
                            {error_context}
                            <br><b>General Troubleshooting:</b><br>
                            • Ensure dependent and grouping variables are categorical<br>
                            • Check that selected variables exist in your dataset<br>
                            • Verify sufficient sample sizes in each group (≥5 recommended)<br>
                            • Confirm variables have adequate variation (≥2 categories)<br><hr>"
                        )
                        self$results$todo$setContent(error_msg)
                        return()
                    })
                    
                    # Add checkpoint for user feedback
                    private$.checkpoint()
                }
            }

            ,
            .plot = function(image, ggtheme, theme, ...) {
                # Validation ----
                if (is.null(self$options$dep) || is.null(self$options$group))
                    return()

                # Use cached data for performance
                tryCatch({
                    mydata <- private$.getCachedData()
                }, error = function(e) {
                    stop(paste("Plot preparation failed:", e$message))
                })

                dep <- self$options$dep

                # Single vs Multiple dependent variables using shared logic
                if (length(dep) == 1) {
                    plot <- private$.createBarPlot(
                        data = mydata, 
                        dep_var = dep, 
                        ggtheme = ggtheme, 
                        grouped = FALSE,
                        progress_label = "main plot"
                    )
                } else {
                    plot <- private$.createMultiplePlots(
                        data = mydata, 
                        dep_vars = dep, 
                        ggtheme = ggtheme, 
                        grouped = FALSE
                    )
                }

                # Print Plot ----
                print(plot)
                TRUE
            }


            ,

            .plot2 = function(image, ggtheme, theme, ...) {
                # Validation ----
                if (is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                    return()

                # Use cached data for performance  
                tryCatch({
                    mydata <- private$.getCachedData()
                }, error = function(e) {
                    stop(paste("Grouped plot preparation failed:", e$message))
                })

                dep <- self$options$dep

                # Single vs Multiple dependent variables using shared logic (grouped)
                if (length(dep) == 1) {
                    plot2 <- private$.createBarPlot(
                        data = mydata, 
                        dep_var = dep, 
                        ggtheme = ggtheme, 
                        grouped = TRUE,
                        progress_label = "grouped plot"
                    )
                } else {
                    plot2 <- private$.createMultiplePlots(
                        data = mydata, 
                        dep_vars = dep, 
                        ggtheme = ggtheme, 
                        grouped = TRUE
                    )
                }

                # Print Plot ----
                print(plot2)
                TRUE
            }

        )
    )
