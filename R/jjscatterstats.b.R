#' @title Scatter Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#'

jjscatterstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjscatterstatsClass",
    inherit = jjscatterstatsBase,
    private = list(

        # === Variable Name Safety ===
        .escapeVar = function(varname) {
            if (is.null(varname) || varname == "") return(varname)
            # Wrap in backticks if contains spaces or special chars
            if (grepl("[^A-Za-z0-9_.]", varname)) {
                return(paste0("`", varname, "`"))
            }
            return(varname)
        },

        # init ----

        .init = function() {

            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$plot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$grvar)) {
                mydata <- self$data
                grvar <-  self$options$grvar
                num_levels <- nlevels(as.factor(mydata[[grvar]]))
                self$results$plot2$setSize(num_levels * plotwidth, plotheight)
            }

            # Set size for plot3 (enhanced scatter)
            self$results$plot3$setSize(plotwidth, plotheight)

            # Control visibility of plot3 based on enhanced plot variables
            # This is also handled in .r.yaml visible expression
            # Keeping this here as backup and for R function usage
            hasEnhancedVars <- !is.null(self$options$colorvar) ||
                               !is.null(self$options$sizevar) ||
                               !is.null(self$options$shapevar) ||
                               !is.null(self$options$alphavar) ||
                               !is.null(self$options$labelvar)
            self$results$plot3$setVisible(hasEnhancedVars)
        },

        # run ----

        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Scatter Plot with correlation analysis.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.html' target='_blank'>ggscatterstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggscatterstats.html' target='_blank'>grouped_ggscatterstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)
                return()

            } else {

                # todo ----
                todo <- glue::glue(
                    "<br>You have selected to use a scatter plot with correlation analysis.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')
            }

            private$.applyClinicalPreset()
            private$.generateExplanations()
        },

        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            if (preset == "custom") {
                return()
            }

            # CRITICAL FIX: Make preset mutations transparent with warnings
            preset_message <- NULL

            if (preset == "biomarker_correlation") {
                preset_message <- paste0(
                    "<div style='background:#e3f2fd; border-left:4px solid #2196F3; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#1976D2; margin-top:0;'>ℹ️ Clinical Preset Applied: Biomarker Correlation</h4>",
                    "<p><strong>The following settings have been automatically configured:</strong></p>",
                    "<ul>",
                    "<li>Statistical test: <strong>Nonparametric (Spearman correlation)</strong></li>",
                    "<li>Additional plot: <strong>ggpubr scatter plot enabled</strong></li>",
                    "<li>Color palette: <strong>JCO (Journal of Clinical Oncology)</strong></li>",
                    "</ul>",
                    "<p style='margin-bottom:0;'><em>You can modify these settings manually or select 'Custom' preset.</em></p>",
                    "</div>"
                )
                self$options$typestatistics <- "nonparametric"
                self$options$addGGPubrPlot <- TRUE
                self$options$ggpubrPalette <- "jco"

            } else if (preset == "treatment_response_analysis") {
                preset_message <- paste0(
                    "<div style='background:#e3f2fd; border-left:4px solid #2196F3; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#1976D2; margin-top:0;'>ℹ️ Clinical Preset Applied: Treatment Response Analysis</h4>",
                    "<p><strong>The following settings have been automatically configured:</strong></p>",
                    "<ul>",
                    "<li>Statistical test: <strong>Robust (trimmed mean correlation)</strong></li>",
                    "<li>Marginal distributions: <strong>Enabled</strong></li>",
                    "</ul>",
                    "<p style='margin-bottom:0;'><em>You can modify these settings manually or select 'Custom' preset.</em></p>",
                    "</div>"
                )
                self$options$typestatistics <- "robust"
                self$options$marginal <- TRUE

            } else if (preset == "publication_ready") {
                preset_message <- paste0(
                    "<div style='background:#e3f2fd; border-left:4px solid #2196F3; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#1976D2; margin-top:0;'>ℹ️ Clinical Preset Applied: Publication Ready</h4>",
                    "<p><strong>The following settings have been automatically configured:</strong></p>",
                    "<ul>",
                    "<li>Theme: <strong>Original ggstatsplot theme</strong></li>",
                    "<li>Results subtitle: <strong>Enabled (shows statistics on plot)</strong></li>",
                    "</ul>",
                    "<p style='margin-bottom:0;'><em>You can modify these settings manually or select 'Custom' preset.</em></p>",
                    "</div>"
                )
                self$options$originaltheme <- TRUE
                self$options$resultssubtitle <- TRUE
            }

            # Display preset notification
            if (!is.null(preset_message)) {
                self$results$presetInfo$setContent(preset_message)
                self$results$presetInfo$setVisible(TRUE)
            }
        },

        .generateExplanations = function() {
            if (self$options$showExplanations) {
                self$results$explanations$setVisible(TRUE)
                self$results$explanations$setContent(
                    "<h3>Explanations</h3>
                    <p>
                        This scatter plot shows the relationship between two continuous variables.
                        The correlation coefficient (r) measures the strength and direction of the linear relationship between the two variables.
                        The p-value indicates the statistical significance of the correlation.
                    </p>"
                )
            }
        },


        # plot ----

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            plotData <- self$data

            plotData[[self$options$dep]] <- jmvcore::toNumeric(plotData[[self$options$dep]])
            plotData[[self$options$group]] <- jmvcore::toNumeric(plotData[[self$options$group]])

            # Prepare arguments for ggscatterstats
            if (!is.null(self$options$mytitle) && self$options$mytitle != "") {
                title <- jmvcore::format(self$options$mytitle)
            } else {
                title <- paste(self$options$dep, "vs", self$options$group)
            }

            if (!is.null(self$options$xtitle) && self$options$xtitle != "") {
                xtitle <- jmvcore::format(self$options$xtitle)
            } else {
                xtitle <- self$options$dep
            }

            if (!is.null(self$options$ytitle) && self$options$ytitle != "") {
                ytitle <- jmvcore::format(self$options$ytitle)
            } else {
                ytitle <- self$options$group
            }

            # Function arguments
            .args <- list(
                data = plotData,
                x = self$options$dep,
                y = self$options$group,
                type = self$options$typestatistics,
                title = title,
                xlab = xtitle,
                ylab = ytitle,
                results.subtitle = self$options$resultssubtitle,
                conf.level = self$options$conflevel,
                bf.message = self$options$bfmessage,
                k = self$options$k,
                marginal = self$options$marginal,
                marginal.type = self$options$marginalType,
                point.size = self$options$pointsize,
                point.alpha = self$options$pointalpha,
                method = self$options$smoothMethod,  # Wire smoothMethod
                smooth.line.args = list(
                    size = self$options$smoothlinesize,
                    color = self$options$smoothlinecolor
                )
            )

            if (self$options$marginal) {
                .args$xfill <- self$options$xsidefill
                .args$yfill <- self$options$ysidefill
            }

            plot <- do.call(ggstatsplot::ggscatterstats, .args)

            if (self$options$showRugPlot) {
                plot <- plot + ggplot2::geom_rug(alpha = 0.5)
            }

            if (!self$options$originaltheme) {
                plot <- plot + ggplot2::theme_bw()
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }

            print(plot)
            TRUE
        },

        # plot2 ----

        .plot2 = function(image, ggtheme, theme, ...) {

            if (is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            plotData <- self$data

            plotData[[self$options$dep]] <- jmvcore::toNumeric(plotData[[self$options$dep]])
            plotData[[self$options$group]] <- jmvcore::toNumeric(plotData[[self$options$group]])

            # Prepare arguments for grouped_ggscatterstats
            if (!is.null(self$options$mytitle) && self$options$mytitle != "") {
                title <- jmvcore::format(self$options$mytitle)
            } else {
                title <- paste(self$options$dep, "vs", self$options$group, "by", self$options$grvar)
            }

            if (!is.null(self$options$xtitle) && self$options$xtitle != "") {
                xtitle <- jmvcore::format(self$options$xtitle)
            } else {
                xtitle <- self$options$dep
            }

            if (!is.null(self$options$ytitle) && self$options$ytitle != "") {
                ytitle <- jmvcore::format(self$options$ytitle)
            } else {
                ytitle <- self$options$group
            }

            # Call grouped_ggscatterstats with proper NSE handling
            # Use rlang::expr to create the call with symbols
            plot_call <- rlang::expr(
                ggstatsplot::grouped_ggscatterstats(
                    data = plotData,
                    x = !!rlang::sym(self$options$dep),
                    y = !!rlang::sym(self$options$group),
                    grouping.var = !!rlang::sym(self$options$grvar),
                    type = !!self$options$typestatistics,
                    title.prefix = !!title,
                    xlab = !!xtitle,
                    ylab = !!ytitle,
                    results.subtitle = !!self$options$resultssubtitle,
                    conf.level = !!self$options$conflevel,
                    bf.message = !!self$options$bfmessage,
                    k = !!self$options$k,
                    marginal = !!self$options$marginal,
                    marginal.type = !!self$options$marginalType,  # CRITICAL FIX: Use actual option value
                    point.size = !!self$options$pointsize,
                    point.alpha = !!self$options$pointalpha,
                    method = !!self$options$smoothMethod, # Wire smoothMethod
                    smooth.line.args = !!list(
                        size = self$options$smoothlinesize,
                        color = self$options$smoothlinecolor
                    )
                )
            )

            # Add marginal options if needed
            if (self$options$marginal) {
                plot_call <- rlang::expr(
                    ggstatsplot::grouped_ggscatterstats(
                        data = plotData,
                        x = !!rlang::sym(self$options$dep),
                        y = !!rlang::sym(self$options$group),
                        grouping.var = !!rlang::sym(self$options$grvar),
                        type = !!self$options$typestatistics,
                        title.prefix = !!title,
                        xlab = !!xtitle,
                        ylab = !!ytitle,
                        results.subtitle = !!self$options$resultssubtitle,
                        conf.level = !!self$options$conflevel,
                        bf.message = !!self$options$bfmessage,
                        k = !!self$options$k,
                        marginal = !!self$options$marginal,
                        marginal.type = !!self$options$marginalType, # Correctly use option
                        xfill = !!self$options$xsidefill,
                        yfill = !!self$options$ysidefill,
                        point.size = !!self$options$pointsize,
                        point.alpha = !!self$options$pointalpha,
                        method = !!self$options$smoothMethod,
                        smooth.line.args = !!list(
                            size = self$options$smoothlinesize,
                            color = self$options$smoothlinecolor
                        )
                    )
                )
            }

            # Evaluate the call
            plot <- eval(plot_call)

            if (self$options$showRugPlot) {
                plot <- plot + ggplot2::geom_rug(alpha = 0.5)
            }

            if (!self$options$originaltheme) {
                plot <- plot + ggplot2::theme_bw()
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }

            print(plot)
            TRUE
        },

        # plot3 - Enhanced scatter with multiple aesthetics ----

        .plot3 = function(image, ggtheme, theme, ...) {

            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            # Only show if any enhanced variables are selected
            hasEnhanced <- !is.null(self$options$colorvar) ||
                          !is.null(self$options$sizevar) ||
                          !is.null(self$options$shapevar) ||
                          !is.null(self$options$alphavar) ||
                          !is.null(self$options$labelvar)

            if (!hasEnhanced)
                return()

            plotData <- self$data

            # Convert variables to numeric
            plotData[[self$options$dep]] <- jmvcore::toNumeric(plotData[[self$options$dep]])
            plotData[[self$options$group]] <- jmvcore::toNumeric(plotData[[self$options$group]])

            # Prepare title and labels
            if (!is.null(self$options$mytitle) && self$options$mytitle != "") {
                title <- jmvcore::format(self$options$mytitle)
            } else {
                title <- paste(self$options$dep, "vs", self$options$group)
            }

            if (!is.null(self$options$xtitle) && self$options$xtitle != "") {
                xtitle <- jmvcore::format(self$options$xtitle)
            } else {
                xtitle <- self$options$dep
            }

            if (!is.null(self$options$ytitle) && self$options$ytitle != "") {
                ytitle <- jmvcore::format(self$options$ytitle)
            } else {
                ytitle <- self$options$group
            }

            # Build base aesthetic mapping
            aes_mapping <- ggplot2::aes(
                x = .data[[self$options$dep]],
                y = .data[[self$options$group]]
            )

            # Start building plot
            p <- ggplot2::ggplot(plotData, aes_mapping)

            # Build point aesthetics mapping
            point_aes <- list()

            if (!is.null(self$options$colorvar) && self$options$colorvar != "") {
                point_aes$colour <- rlang::sym(private$.escapeVar(self$options$colorvar))
            }

            if (!is.null(self$options$sizevar) && self$options$sizevar != "") {
                point_aes$size <- rlang::sym(private$.escapeVar(self$options$sizevar))
            }

            if (!is.null(self$options$shapevar) && self$options$shapevar != "") {
                point_aes$shape <- rlang::sym(private$.escapeVar(self$options$shapevar))
            }

            if (!is.null(self$options$alphavar) && self$options$alphavar != "") {
                point_aes$alpha <- rlang::sym(private$.escapeVar(self$options$alphavar))
            }

            # Add points with aesthetics
            if (length(point_aes) > 0) {
                p <- p + ggplot2::geom_point(
                    mapping = do.call(ggplot2::aes, point_aes)
                )
            } else {
                p <- p + ggplot2::geom_point(
                    size = self$options$pointsize,
                    alpha = self$options$pointalpha
                )
            }

            # Add smooth line with selected method
            smooth_method <- switch(
                self$options$smoothMethod,
                "lm" = "lm",
                "loess" = "loess",
                "gam" = "gam",
                "lm"
            )

            p <- p + ggplot2::geom_smooth(
                method = smooth_method,
                se = TRUE,
                linewidth = self$options$smoothlinesize,
                color = self$options$smoothlinecolor
            )

            # Add rug plot if requested
            if (self$options$showRugPlot) {
                p <- p + ggplot2::geom_rug(alpha = 0.3, length = ggplot2::unit(0.05, "npc"))
            }

            # Add point labels using ggrepel if requested
            if (!is.null(self$options$labelvar) && self$options$labelvar != "") {
                if (requireNamespace("ggrepel", quietly = TRUE)) {
                    label_aes <- ggplot2::aes(label = .data[[self$options$labelvar]])
                    p <- p + ggrepel::geom_text_repel(
                        mapping = label_aes,
                        size = 3,
                        max.overlaps = 10
                    )
                }
            }

            # CRITICAL FIX: Add correlation annotation with proper method handling
            tryCatch({
                test_type <- self$options$typestatistics
                cor_method <- "pearson"  # Default
                method_label <- "Pearson"
                warning_msg <- NULL

                if (test_type == "parametric") {
                    cor_method <- "pearson"
                    method_label <- "Pearson"
                } else if (test_type == "nonparametric") {
                    cor_method <- "spearman"
                    method_label <- "Spearman"
                } else if (test_type == "robust") {
                    # Robust correlation requires special packages
                    if (requireNamespace("WRS2", quietly = TRUE)) {
                        # Could use WRS2::pbcor for robust correlation
                        # For now, fall back to Spearman with warning
                        cor_method <- "spearman"
                        method_label <- "Spearman (robust unavailable)"
                        warning_msg <- paste0(
                            "⚠️ Robust correlation not fully implemented for enhanced plot. ",
                            "Falling back to Spearman correlation. ",
                            "For robust analysis, use the main ggstatsplot plot (plot 1)."
                        )
                    } else {
                        cor_method <- "pearson"
                        method_label <- "Pearson (robust unavailable)"
                        warning_msg <- paste0(
                            "⚠️ Robust correlation requires WRS2 package which is not available. ",
                            "Falling back to Pearson correlation."
                        )
                    }
                } else if (test_type == "bayes" || test_type == "bayesian") {
                    # Bayesian correlation requires BayesFactor package
                    cor_method <- "pearson"
                    method_label <- "Pearson (Bayesian unavailable)"
                    warning_msg <- paste0(
                        "⚠️ Bayesian correlation not implemented for enhanced plot. ",
                        "Falling back to Pearson correlation. ",
                        "For Bayesian analysis, use the main ggstatsplot plot (plot 1)."
                    )
                } else {
                    # Unknown method, default to Pearson with warning
                    cor_method <- "pearson"
                    method_label <- "Pearson (default)"
                    warning_msg <- paste0(
                        "⚠️ Unknown correlation method '", test_type, "'. ",
                        "Falling back to Pearson correlation."
                    )
                }

                # Show warning if method was changed
                if (!is.null(warning_msg)) {
                    current_warnings <- self$results$warnings$state
                    if (is.null(current_warnings)) {
                        current_warnings <- ""
                    }
                    new_warning <- paste0(
                        current_warnings,
                        "<p style='color:#856404;'>", warning_msg, "</p>"
                    )
                    self$results$warnings$setContent(new_warning)
                    self$results$warnings$setVisible(TRUE)
                }

                cor_result <- stats::cor.test(
                    plotData[[self$options$dep]],
                    plotData[[self$options$group]],
                    method = cor_method
                )

                cor_text <- sprintf(
                    "%s: r = %.3f, p %s %.3f",
                    method_label,
                    cor_result$estimate,
                    ifelse(cor_result$p.value < 0.001, "<", "="),
                    ifelse(cor_result$p.value < 0.001, 0.001, cor_result$p.value)
                )

                p <- p + ggplot2::labs(subtitle = cor_text)
            }, error = function(e) {
                # If correlation fails, continue without it
                warning_msg <- paste0("⚠️ Correlation calculation failed: ", e$message)
                self$results$warnings$setContent(warning_msg)
                self$results$warnings$setVisible(TRUE)
            })

            # Add labels
            p <- p + ggplot2::labs(
                title = title,
                x = xtitle,
                y = ytitle
            )

            # Apply theme
            if (!self$options$originaltheme) {
                p <- p + ggplot2::theme_bw()
            } else {
                p <- p + ggstatsplot::theme_ggstatsplot()
            }

            # Add marginal plots if requested
            if (self$options$marginalType != "none") {
                if (requireNamespace("ggExtra", quietly = TRUE)) {
                    p <- ggExtra::ggMarginal(
                        p,
                        type = self$options$marginalType,
                        size = 5
                    )
                }
            }

            print(p)
            TRUE
        }

        ,
        .plotGGPubr = function(image, ...) {
            # Validate inputs
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            # Skip if ggpubr plot not requested
            if (!self$options$addGGPubrPlot)
                return()

            # Prepare data
            mydata <- self$data
            dep <- self$options$dep
            group <- self$options$group

            # Build scatter plot arguments
            args <- list(
                data = mydata,
                x = dep,
                y = group,
                palette = self$options$ggpubrPalette
            )

            # CRITICAL FIX: Implement ggpubrAddSmooth option
            # Build the 'add' parameter based on user selections
            # ggpubr::ggscatter 'add' argument only accepts a single string in some versions
            # So we handle multiple elements by adding them manually
            add_element <- NULL

            if (self$options$ggpubrAddCorr) {
                add_element <- "reg.line"
                args$conf.int <- TRUE
                args$cor.coef <- TRUE
                args$cor.method <- self$options$ggpubrCorrMethod
            } else if (self$options$ggpubrAddSmooth) {
                # Only set loess here if reg.line is NOT set
                add_element <- "loess"
                args$conf.int <- TRUE # Add CI for loess too if it's the only one
            }

            # Set the add argument if we have one
            if (!is.null(add_element)) {
                args$add <- add_element
            }

            # Create scatter plot
            plot <- do.call(ggpubr::ggscatter, args)

            # If BOTH are selected, we need to add loess manually since we used reg.line for 'add'
            if (self$options$ggpubrAddCorr && self$options$ggpubrAddSmooth) {
                plot <- plot + ggplot2::geom_smooth(method = "loess", se = TRUE)
            }

            # Apply theme
            plot <- plot + ggpubr::theme_pubr()

            print(plot)
            TRUE
        }

        ,
        .plotGGPubr2 = function(image, ...) {
            # Validate inputs
            if (is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            # Skip if ggpubr plot not requested
            if (!self$options$addGGPubrPlot)
                return()

            # Prepare data
            mydata <- self$data
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar

            # Build scatter plot arguments with faceting
            args <- list(
                data = mydata,
                x = dep,
                y = group,
                palette = self$options$ggpubrPalette,
                facet.by = grvar
            )

            # CRITICAL FIX: Implement ggpubrAddSmooth option
            # Build the 'add' parameter based on user selections
            # ggpubr::ggscatter 'add' argument only accepts a single string in some versions
            # So we handle multiple elements by adding them manually
            add_element <- NULL

            if (self$options$ggpubrAddCorr) {
                add_element <- "reg.line"
                args$conf.int <- TRUE
                args$cor.coef <- TRUE
                args$cor.method <- self$options$ggpubrCorrMethod
            } else if (self$options$ggpubrAddSmooth) {
                # Only set loess here if reg.line is NOT set
                add_element <- "loess"
                args$conf.int <- TRUE # Add CI for loess too if it's the only one
            }

            # Set the add argument if we have one
            if (!is.null(add_element)) {
                args$add <- add_element
            }

            # Create scatter plot
            plot <- do.call(ggpubr::ggscatter, args)

            # If BOTH are selected, we need to add loess manually since we used reg.line for 'add'
            if (self$options$ggpubrAddCorr && self$options$ggpubrAddSmooth) {
                plot <- plot + ggplot2::geom_smooth(method = "loess", se = TRUE)
            }

            # Apply theme
            plot <- plot + ggpubr::theme_pubr()

            print(plot)
            TRUE
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Scatter Plot Statistics analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep <- self$options$dep
            group <- self$options$group

            if (is.null(dep) || is.null(group))
                return('')

            # Escape variable names
            dep_escaped <- if (!is.null(dep) && !identical(make.names(dep), dep)) {
                paste0('`', dep, '`')
            } else {
                dep
            }

            group_escaped <- if (!is.null(group) && !identical(make.names(group), group)) {
                paste0('`', group, '`')
            } else {
                group
            }

            # Build arguments
            dep_arg <- paste0('dep = "', dep_escaped, '"')
            group_arg <- paste0('group = "', group_escaped, '"')

            # Get other arguments
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::jjscatterstats(\n    data = data,\n    ',
                   dep_arg, ',\n    ', group_arg, args, ')')
        }
    ) # End of public list
)