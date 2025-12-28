#' @title Syndromic Plot for PCA
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @import dplyr
#' @import ggrepel
#'

# The syndromic plot visualization is based on the syndRomics R package
# (https://github.com/ucsf-ferguson-lab/syndRomics) developed by the Ferguson Lab
# at UC San Francisco. The triangular design and arrow-based PCA loading visualization
# methodology was originally described in Ferguson et al. (2013) PLOS ONE 8(3):e59712.
# This implementation adapts the syndRomics approach for jamovi's interactive
# analysis environment while maintaining the core geometric principles.
#

jjsyndromicplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjsyndromicplotClass",
    inherit = jjsyndromicplotBase,
    private = list(
        .pcaResults = NULL,
        .pcaData = NULL,
        .presetSettings = NULL,
        .componentToUse = NULL,
        .presetMessage = "",

        # Variable name escaping utility for special characters
        .escapeVar = function(x) {
            # Convert variable names with special characters to safe R names
            # This mirrors the modelbuilder behavior for handling spaces and punctuation
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        # Notice creation helper with single-line enforcement
        .setNotice = function(content, type = 3) {
            # Convert multi-line content to single line
            content <- gsub("\\n", " ", content)
            content <- gsub("\\s+", " ", trimws(content))

            # Create fresh Notice object
            notice <- jmvcore::Notice$new(
                content = content,
                type = type
            )

            # Dynamic insertion based on type
            # All notices now go to the end (position 999)
            # Previously: ERROR (1) and STRONG_WARNING (2) at top, WARNING (3) and INFO (4) at bottom
            # Now: All notices at bottom for consistent ordering
            self$results$notices$insert(999, notice)
        },

        # init ----

        .init = function() {

            # Set plot size
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 600

            self$results$plot$setSize(plotwidth, plotheight)

        },

        # run ----

        .run = function() {

            # Reset messages each run
            self$results$warnings$setContent("")
            self$results$warnings$setVisible(FALSE)
            self$results$explanations$setVisible(self$options$showExplanations)
            self$results$explanations$setContent("")
            private$.pcaResults <- NULL
            private$.pcaData <- NULL
            private$.presetSettings <- NULL
            private$.componentToUse <- NULL
            private$.presetMessage <- ""

            # Initial Message ----
            if (length(self$options$vars) < 3) {

                # todo ----
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Syndromic Plots for Principal Component Analysis.
                <br><br>
                Syndromic plots visualize PCA loadings using a triangular design with directional arrows.
                Arrow width and color represent loading magnitude and direction.
                <br><br>
                This function is based on syndRomics package. See documentation
                <a href='https://github.com/ucsf-ferguson-lab/syndRomics' target='_blank'>syndRomics on GitHub</a>.
                <br><br>
                <b>Required:</b> Select at least 3 continuous variables for PCA.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)
                return()

            } else {

                # todo ----
                todo <- glue::glue(
                    "<br>Syndromic plot generated for Principal Component Analysis.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0) {
                    private$.setNotice("Data contains no complete rows after removing missing values.", type = 1)
                    stop('Data contains no (complete) rows')
                }
            }

            # Apply clinical preset
            private$.applyClinicalPreset()

            # Run PCA ----
            private$.runPCA()

            # Generate explanations if requested
            if (self$options$showExplanations) {
                private$.generateExplanations()
            }

        },

        # Apply clinical preset ----
        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            settings <- list(
                cutoff = self$options$cutoff,
                arrowsize = self$options$arrowsize,
                textsize = self$options$textsize,
                varorder = self$options$varorder,
                colorlow = self$options$colorlow,
                colormid = self$options$colormid,
                colorhigh = self$options$colorhigh
            )

            preset_message <- NULL

            if (preset == "biomarker_discovery") {
                preset_message <- paste0(
                    "<div style='background:#e3f2fd; border-left:4px solid #2196F3; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#1976D2; margin-top:0;'>ℹ️ Clinical Preset Applied: Biomarker Discovery</h4>",
                    "<p><strong>The following settings have been automatically configured:</strong></p>",
                    "<ul>",
                    "<li>Cutoff threshold: <strong>0.4</strong></li>",
                    "<li>Arrow size: <strong>12</strong></li>",
                    "<li>Text size: <strong>10</strong></li>",
                    "<li>Variable order: <strong>By absolute loading (decreasing)</strong></li>",
                    "<li>Color scheme: <strong>Blue-White-Red gradient</strong></li>",
                    "</ul>",
                    "<p style='margin-bottom:0;'><em>Note: These settings override the UI controls. To use custom values, select 'None' or 'Custom'.</em></p>",
                    "</div>"
                )
                settings$cutoff <- 0.4
                settings$arrowsize <- 12
                settings$textsize <- 10
                settings$varorder <- "absdecreasing"
                settings$colorlow <- "blue"
                settings$colormid <- "white"
                settings$colorhigh <- "red"

            } else if (preset == "disease_subtyping") {
                preset_message <- paste0(
                    "<div style='background:#e3f2fd; border-left:4px solid #2196F3; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#1976D2; margin-top:0;'>ℹ️ Clinical Preset Applied: Disease Subtyping</h4>",
                    "<p><strong>The following settings have been automatically configured:</strong></p>",
                    "<ul>",
                    "<li>Cutoff threshold: <strong>0.3</strong> (more variables shown)</li>",
                    "<li>Arrow size: <strong>15</strong> (larger for clarity)</li>",
                    "<li>Text size: <strong>8</strong> (smaller to fit more labels)</li>",
                    "<li>Variable order: <strong>By loading value (decreasing)</strong></li>",
                    "<li>Color scheme: <strong>Green-White-Purple gradient</strong></li>",
                    "</ul>",
                    "<p style='margin-bottom:0;'><em>Note: These settings override the UI controls. To use custom values, select 'None' or 'Custom'.</em></p>",
                    "</div>"
                )
                settings$cutoff <- 0.3
                settings$arrowsize <- 15
                settings$textsize <- 8
                settings$varorder <- "decreasing"
                settings$colorlow <- "green"
                settings$colormid <- "white"
                settings$colorhigh <- "purple"
            }

            private$.presetSettings <- settings

            # Display preset notification
            if (!is.null(preset_message)) {
                private$.presetMessage <- preset_message
            }
        },

        # Generate explanations ----
        .generateExplanations = function() {
            self$results$explanations$setVisible(TRUE)
            self$results$explanations$setContent(
                "<h3>Explanations</h3>
                <p>
                    Principal Component Analysis (PCA) is a statistical technique used to reduce the dimensionality of a dataset while preserving as much of the original variance as possible.
                    It does this by creating new, uncorrelated variables called principal components (PCs), which are linear combinations of the original variables.
                </p>
                <p>
                    The syndromic plot is a visualization of the PCA loadings.
                    The loadings represent the contribution of each original variable to the principal component.
                    Variables with high absolute loadings are the most important for that component.
                </p>
                <p>
                    In the syndromic plot, the <strong>arrows</strong> represent the variables.
                    The <strong>width</strong> of the arrow represents the magnitude (absolute strength) of the loading.
                    The <strong>color</strong> represents the direction:
                    <ul>
                        <li><strong>High Color (e.g. Red)</strong>: Positive loading (variable increases with PC).</li>
                        <li><strong>Low Color (e.g. Blue)</strong>: Negative loading (variable decreases with PC).</li>
                    </ul>
                </p>"
            )
        },

        # Run PCA ----
        .runPCA = function() {

            # Get selected variables
            vars <- self$options$vars

            # Extract data
            mydata <- self$data
            pca_data <- mydata[, vars, drop = FALSE]

            # CRITICAL FIX: Handle missing data and report N
            initial_n <- nrow(pca_data)
            n_vars_initial <- ncol(pca_data)
            warning_msgs <- character()

            # Prepend Preset Message if exists
            if (nchar(private$.presetMessage) > 0) {
                warning_msgs <- c(warning_msgs, private$.presetMessage)
            }

            # Handle Missing Data
            missing_method <- if (!is.null(self$options$missing)) self$options$missing else "listwise"

            if (missing_method == "mean_imputation") {
                # Mean Imputation
                for (col in names(pca_data)) {
                    if (any(is.na(pca_data[[col]]))) {
                        pca_data[[col]][is.na(pca_data[[col]])] <- mean(pca_data[[col]], na.rm = TRUE)
                    }
                }
                # No rows dropped
            } else {
                # Listwise Deletion (default)
                pca_data <- na.omit(pca_data)
            }

            final_n <- nrow(pca_data)
            n_vars_final <- ncol(pca_data) # Will update if zero-var removed

            # Data Summary Message
            dropped_n <- initial_n - final_n
            if (dropped_n > 0) {
                private$.setNotice(
                    paste0("Analyzed N=", final_n, " (", dropped_n, " cases removed using ",
                           ifelse(missing_method=="mean_imputation", "mean imputation", "listwise deletion"), ")"),
                    type = 4
                )
            }
            summary_msg <- paste0(
                "<div style='background:#f5f5f5; padding:10px; margin:10px 0; border:1px solid #ddd;'>",
                "<strong>Data Summary:</strong>",
                "<ul>",
                "<li>Initial N: ", initial_n, "</li>",
                "<li>Analyzed N: ", final_n, " (", dropped_n, " cases removed)</li>",
                "<li>Missing Data Method: ", ifelse(missing_method=="mean_imputation", "Mean Imputation", "Listwise Deletion"), "</li>",
                "</ul>",
                "</div>"
            )
            warning_msgs <- c(warning_msgs, summary_msg)


            # CRITICAL FIX: Validate input types BEFORE coercion
            # Check for categorical/factor variables
            non_numeric_vars <- c()
            for (var_name in names(pca_data)) {
                col_data <- pca_data[[var_name]]
                # Check if factor, character, or ordered
                if (is.factor(col_data) || is.character(col_data) || is.ordered(col_data)) {
                    non_numeric_vars <- c(non_numeric_vars, var_name)
                }
                # Also check if it's numeric but actually categorical (few unique values)
                else if (is.numeric(col_data)) {
                    unique_vals <- length(unique(col_data[!is.na(col_data)]))
                    if (unique_vals <= 5 && unique_vals < nrow(pca_data) * 0.1) {
                        private$.setNotice(
                            paste0("Variable '", var_name, "' has only ", unique_vals,
                                   " unique values and may be categorical rather than continuous."),
                            type = 3
                        )
                        warning_msgs <- c(
                            warning_msgs,
                            paste0(
                                "<p>⚠️ Variable '<code>", var_name, "</code>' has only ",
                                unique_vals, " unique values. It may be categorical rather than ",
                                "continuous. PCA assumes continuous variables.</p>"
                            )
                        )
                    }
                }
            }

            # If categorical variables found, stop with clear error
            if (length(non_numeric_vars) > 0) {
                private$.setNotice(
                    paste0("PCA requires continuous numeric variables. Categorical variables detected: ",
                           paste(non_numeric_vars, collapse = ", ")),
                    type = 1
                )
                error_msg <- paste0(
                    "<div style='background:#fff3cd; border-left:4px solid #ff9800; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#ff6f00; margin-top:0;'>❌ Categorical Variables Detected</h4>",
                    "<p><strong>PCA requires continuous numeric variables only.</strong></p>",
                    "<p>The following variables are categorical/non-numeric:</p>",
                    "<ul>",
                    paste0("<li><code>", non_numeric_vars, "</code></li>", collapse = ""),
                    "</ul>",
                    "<p><strong>Action required:</strong> Remove these variables or convert them to numeric if appropriate.</p>",
                    "</div>"
                )
                self$results$warnings$setContent(error_msg)
                self$results$warnings$setVisible(TRUE)
                return()
            }

            # Convert to numeric matrix (now safe after validation)
            pca_matrix <- as.matrix(sapply(pca_data, as.numeric))

            # Flag and drop zero-variance variables
            sd_vals <- apply(pca_matrix, 2, sd)
            zero_var_names <- names(sd_vals[sd_vals == 0 | is.na(sd_vals)])
            if (length(zero_var_names) > 0) {
                private$.setNotice(
                    paste0("Zero-variance variables removed: ", paste(zero_var_names, collapse = ", ")),
                    type = 3
                )
                warning_msgs <- c(
                    warning_msgs,
                    paste0(
                        "<p>⚠️ The following variables have zero variance and were removed before PCA: ",
                        paste0("<code>", zero_var_names, "</code>", collapse = ", "),
                        ".</p>"
                    )
                )
                keep_vars <- sd_vals > 0 & !is.na(sd_vals)
                pca_matrix <- pca_matrix[, keep_vars, drop = FALSE]
                vars <- vars[keep_vars]
                if (length(vars) < 3) {
                    private$.setNotice("Insufficient variables remaining after removing zero-variance variables (need at least 3).", type = 1)
                    self$results$warnings$setContent(paste0(warning_msgs, collapse = ""))
                    self$results$warnings$setVisible(TRUE)
                    return()
                }
            }

            # Check for sufficient data
            if (nrow(pca_matrix) < 3) {
                private$.setNotice("Insufficient data for PCA. Need at least 3 complete observations.", type = 1)
                stop('Insufficient data for PCA. Need at least 3 complete observations.')
            }

            # Run PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # CRITICAL FIX: Check component bounds
            n_components <- ncol(pca$rotation)
            requested_component <- self$options$component
            component_to_use <- requested_component

            if (requested_component > n_components) {
                private$.setNotice(
                    paste0("Requested component PC", requested_component,
                           " exceeds available components (PC1-PC", n_components,
                           "). Reset to PC1."),
                    type = 3
                )
                warning_msgs <- c(
                    warning_msgs,
                    paste0(
                        "<div style='background:#fff3cd; border-left:4px solid #ff9800; padding:15px; margin:10px 0;'>",
                        "<h4 style='color:#ff6f00; margin-top:0;'>⚠️ Component Number Out of Bounds</h4>",
                        "<p><strong>Requested component ", requested_component,
                        " exceeds available components.</strong></p>",
                        "<p>PCA with ", length(vars), " variables produced ", n_components,
                        " components (PC1 through PC", n_components, ").</p>",
                        "<p><strong>Action:</strong> Component has been reset to PC1. ",
                        "Please select a component between 1 and ", n_components, ".</p>",
                        "</div>"
                    )
                )

                component_to_use <- 1
            }

            private$.componentToUse <- component_to_use

            if (length(warning_msgs) > 0) {
                self$results$warnings$setContent(paste0(warning_msgs, collapse = ""))
                self$results$warnings$setVisible(TRUE)
            }

            # Store PCA results for plotting
            private$.pcaResults <- pca
            private$.pcaData <- pca_matrix

            # Analysis completion notice
            variance_explained <- round(pca$sdev[component_to_use]^2 / sum(pca$sdev^2) * 100, 1)
            private$.setNotice(
                paste0("PCA completed: PC", component_to_use, " explains ", variance_explained,
                       "% of variance (N=", final_n, ", p=", length(vars), ")"),
                type = 4
            )

            # Populate results table
            private$.populateTable()

        },

        # Populate loadings table ----
        .populateTable = function() {

            if (is.null(private$.pcaResults))
                return()

            pca <- private$.pcaResults
            pca_data <- private$.pcaData

            # Extract standardized loadings
            loadings <- private$.stand_loadings(pca, pca_data)

            # Get component to display
            component <- if (!is.null(private$.componentToUse)) private$.componentToUse else self$options$component

            # Create results table
            table <- self$results$loadings

            vars <- rownames(loadings)

            for (i in seq_along(vars)) {
                row <- list()
                row[['variable']] <- vars[i]
                row[['loading']] <- loadings[i, component]
                row[['absloading']] <- abs(loadings[i, component])

                table$addRow(rowKey = i, values = row)
            }

        },

        # Extract standardized loadings ----
        .stand_loadings = function(pca, pca_data) {

            # CRITICAL FIX: Correct dimension alignment for unscaled PCA
            # Original bug: Division recycled DOWN columns instead of across rows
            # Matrix is (n_vars × n_components), SD vector has length n_vars
            # Must divide each ROW (variable) by its SD, not recycle down columns

            loadings_unscaled <- pca$rotation %*% diag(pca$sdev)

            if (is.numeric(pca$scale)) {
                # Data was scaled by prcomp, loadings already standardized
                loadings <- as.data.frame(loadings_unscaled)
            } else {
                # Data was NOT scaled - must standardize loadings manually
                # Use sweep to divide each row by its corresponding SD
                sd_vals <- apply(pca_data, 2, sd)
                loadings <- as.data.frame(
                    sweep(loadings_unscaled, 1, sd_vals, FUN = "/")
                )
            }

            colnames(loadings) <- paste0('PC', 1:ncol(pca$x))
            rownames(loadings) <- colnames(pca_data)

            return(loadings)
        },

        # plot ----

        .plot = function(image, ggtheme, theme, ...) {

            if (length(self$options$vars) < 3)
                return()

            if (is.null(private$.pcaResults))
                return()

            pca <- private$.pcaResults
            pca_data <- private$.pcaData

            # Get parameters
            component <- if (!is.null(private$.componentToUse)) private$.componentToUse else self$options$component
            settings <- private$.presetSettings
            if (is.null(settings)) {
                settings <- list(
                    cutoff = self$options$cutoff,
                    arrowsize = self$options$arrowsize,
                    textsize = self$options$textsize,
                    varorder = self$options$varorder,
                    colorlow = self$options$colorlow,
                    colormid = self$options$colormid,
                    colorhigh = self$options$colorhigh
                )
            }

            cutoff <- settings$cutoff
            arrow_size_multi <- settings$arrowsize
            text_size <- settings$textsize
            repel <- self$options$repel
            plot_legend <- self$options$plotlegend
            plot_cutoff <- self$options$plotcutoff
            var_order <- switch(
                settings$varorder,
                absdecreasing = "absdecreasing",
                absincreasing = "absincreasing",
                decreasing = "decreasing",
                increasing = "increasing",
                settings$varorder
            )

            # Get colors
            color_low <- settings$colorlow
            color_mid <- settings$colormid
            color_high <- settings$colorhigh
            colors <- c(color_low, color_mid, color_high)

            # Calculate VAF
            VAF <- paste0(round(pca$sdev[component]^2 / sum(pca$sdev^2) * 100, 1), '%')

            # Extract loadings
            loadings <- private$.stand_loadings(pca, pca_data)
            load_df <- data.frame(
                Variables = rownames(loadings),
                loading = loadings[, component],
                component = paste0('PC', component)
            )

            # Generate syndromic plot
            plot <- private$.extract_syndromic_plot(
                load_df = load_df,
                pc = paste0('PC', component),
                cutoff = cutoff,
                VAF = VAF,
                arrow_size_multi = arrow_size_multi,
                repel = repel,
                plot_legend = plot_legend,
                plot_cutoff = plot_cutoff,
                text_size = text_size,
                var_order = var_order,
                colors = colors
            )

            print(plot)
            TRUE

        },

        # Extract syndromic plot ----
        .extract_syndromic_plot = function(load_df, pc, cutoff = 0.5, VAF,
                                          arrow_size_multi = 10,
                                          repel = TRUE, plot_legend = TRUE,
                                          plot_cutoff = TRUE, text_size = 6,
                                          var_order = 'absdecreasing',
                                          colors = c("steelblue1", "white", "firebrick1")) {

            old_scipen <- getOption('scipen')
            on.exit(options(scipen = old_scipen))
            options(scipen = 999)

            p <- load_df

            if (length(p$loading) < 1) {
                private$.setNotice(paste0(pc, " has no loadings above cutoff. Try lowering the cutoff threshold."), type = 1)
                stop(paste(pc, 'has no loadings above cutoff'))
            }

            # Order variables
            if (var_order %in% c('absdecreasing', 'abs decreasing')) {
                p <- p %>% dplyr::arrange(desc(abs(.data$loading)))
            } else if (var_order %in% c('absincreasing', 'abs increasing')) {
                p <- p %>% dplyr::arrange(abs(.data$loading))
            } else if (var_order == 'decreasing') {
                p <- p %>% dplyr::arrange(desc(.data$loading))
            } else if (var_order == 'increasing') {
                p <- p %>% dplyr::arrange(.data$loading)
            }

            # Filter by cutoff
            p <- p %>% dplyr::filter(abs(.data$loading) >= cutoff)

            if (nrow(p) < 1) {
                private$.setNotice(paste0("No loadings above cutoff (", cutoff, ") for ", pc, ". Try lowering the cutoff threshold."), type = 1)
                stop(paste('There is no loading above cutoff for', pc))
            }

            # Calculate positions
            p <- p %>%
                dplyr::mutate(
                    div = 0:(n() - 1),
                    angle = .data$div * 2 * pi / n() + pi / 2,
                    angletext = .data$div * 2 * pi / n() + pi / 2 - pi / 22,
                    xend = 3.5 * cos(.data$angle),
                    yend = 3.5 * sin(.data$angle),
                    x = 7 * cos(.data$angle),
                    y = 7 * sin(.data$angle),
                    xtext = 9 * cos(.data$angle),
                    ytext = 9 * sin(.data$angle),
                    xload = 6 * cos(.data$angletext),
                    yload = 6 * sin(.data$angletext),
                    angletext = (.data$angle * 180 / pi) + ifelse(.data$x < 0, 180, 0),
                    loading_txt = as.character(round(.data$loading, 3)),
                    arrow_weight = (round(.data$loading, 3)),
                    hadjust = ifelse(.data$x < 0, 'right', 'left'),
                    hadjust = ifelse(.data$y < (-3) | .data$y > 3, 'center', .data$hadjust)
                )

            # Create triangle polygon
            angle1 <- seq(0, 1.2, length.out = 50)
            angle2 <- seq(1.95, 3.18, length.out = 50)
            angle3 <- seq(4.02, 5.4, length.out = 50)

            pol <- data.frame(
                x = c(2.8 * cos(angle1) - 1, 2.8 * cos(angle2) + 1, 2.8 * cos(angle3)),
                y = c(2.8 * sin(angle1) - 1, 2.8 * sin(angle2) - 1, 2.8 * sin(angle3) + 1)
            )

            # Create plot
            s_plot <- ggplot2::ggplot(p, aes(color = .data$loading, label = .data$Variables,
                                             x = .data$x, y = .data$y,
                                             xend = .data$xend, yend = .data$yend)) +
                geom_polygon(data = pol, aes(.data$x, .data$y), inherit.aes = FALSE, fill = 'grey') +
                ggplot2::scale_color_gradient2(
                    name = "Loading",
                    high = colors[3], mid = colors[2], low = colors[1],
                    midpoint = 0, na.value = 'transparent'
                ) +
                ggplot2::geom_segment(
                    arrow = arrow(type = 'closed', length = unit(0.3, 'cm'), angle = 25),
                    linewidth = abs(p$arrow_weight) * arrow_size_multi, show.legend = FALSE,
                    linejoin = 'mitre'
                ) +
                ggplot2::ylab(NULL) +
                ggplot2::xlab(NULL) +
                ggplot2::theme(
                    axis.text = element_blank(), panel.background = element_blank(),
                    axis.ticks = element_blank(), axis.line = element_blank(),
                    legend.text = element_blank(), legend.direction = 'horizontal',
                    legend.position = 'bottom', text = element_text(size = text_size * 2)
                ) +
                ggplot2::xlim(-13, 13) +
                ggplot2::ylim(-13, 13) +
                coord_equal()

            # Add legend
            if (plot_legend) {
                legend_res <- 0.005
                legend_df <- data.frame(x = seq(-3, 3, legend_res),
                                       z = seq(-1, 1, legend_res / 3)) %>%
                    dplyr::mutate(
                        xend = ifelse(.data$x <= 0, .data$x - legend_res, .data$x + legend_res),
                        y = rep(-11, length(.data$x)),
                        yend = rep(-11, length(.data$x))
                    )

                if (plot_cutoff) {
                    legend_df_cutoff <- legend_df %>%
                        filter(abs(x) < 3 * cutoff)

                    cutoff_line.xmin <- min(legend_df_cutoff$x)
                    cutoff_line.xmax <- max(legend_df_cutoff$x)

                    legend_df <- legend_df %>%
                        filter(abs(x) > 3 * cutoff)

                    s_plot <- s_plot +
                        ggplot2::geom_segment(data = legend_df, aes(x = .data$x, y = .data$y,
                                                                    xend = .data$xend,
                                                                    yend = .data$yend, color = .data$z,
                                                                    linewidth = abs(.data$z) * 20),
                                             inherit.aes = FALSE, show.legend = FALSE) +
                        geom_segment(data = legend_df_cutoff, aes(x = .data$x, y = .data$y,
                                                                  xend = .data$xend,
                                                                  yend = .data$yend,
                                                                  linewidth = abs(.data$z) * 20,
                                                                  alpha = abs(.data$z)),
                                    inherit.aes = FALSE, show.legend = FALSE, color = "grey") +
                        ggplot2::annotate(geom = "text", x = 0, y = -12.5,
                                         label = paste0("Loadings > |", cutoff, "|"),
                                         size = text_size - 2) +
                        ggplot2::annotate(geom = "segment", x = cutoff_line.xmin,
                                         xend = cutoff_line.xmin,
                                         y = -10.6, yend = -11.4, linewidth = 1, alpha = 0.5) +
                        ggplot2::annotate(geom = "segment", x = cutoff_line.xmax,
                                         xend = cutoff_line.xmax,
                                         y = -10.6, yend = -11.4, linewidth = 1, alpha = 0.5)
                } else {
                    s_plot <- s_plot +
                        ggplot2::geom_segment(data = legend_df, aes(x = .data$x, y = .data$y,
                                                                    xend = .data$xend,
                                                                    yend = .data$yend, color = .data$z,
                                                                    linewidth = abs(.data$z) * 20),
                                             inherit.aes = FALSE, show.legend = FALSE)
                }

                s_plot <- s_plot +
                    ggplot2::geom_segment(aes(x = max(legend_df$x), y = -11,
                                             xend = max(legend_df$x) + 0.1,
                                             yend = -11), color = colors[3],
                                         arrow = arrow(type = 'closed',
                                                      length = unit(0.1, 'cm'), angle = 25),
                                         linewidth = 6, show.legend = FALSE, linejoin = 'mitre') +
                    ggplot2::geom_segment(aes(x = min(legend_df$x), y = -11,
                                             xend = min(legend_df$x) - 0.1,
                                             yend = -11), color = colors[1],
                                         arrow = arrow(type = 'closed',
                                                      length = unit(0.1, 'cm'), angle = 25),
                                         linewidth = 6, show.legend = FALSE, linejoin = 'mitre') +
                    ggplot2::annotate(geom = 'text', x = -4.1, y = -11, label = "-1",
                                     size = text_size) +
                    ggplot2::annotate(geom = 'text', x = 4.1, y = -11, label = "1",
                                     size = text_size)
            }

            # Add component label and VAF
            s_plot <- s_plot +
                ggplot2::annotate(geom = 'text', x = 0, y = 0.25, vjust = 'center',
                                 hjust = 'center', color = 'black', label = pc,
                                 size = text_size) +
                ggplot2::annotate(geom = 'text', x = 0, y = -0.75, vjust = 'center',
                                 hjust = 'center', color = 'black', label = VAF,
                                 size = text_size) +
                ggplot2::geom_text(aes(label = .data$loading_txt, x = .data$xload,
                                      y = .data$yload, angle = .data$angletext),
                                  color = 'black', size = text_size * 0.7)

            # Add variable labels
            if (repel) {
                s_plot <- s_plot +
                    ggrepel::geom_text_repel(aes(x = .data$xtext, y = .data$ytext,
                                                label = .data$Variables, hjust = .data$hadjust),
                                            color = 'black', min.segment.length = 0.5,
                                            size = text_size)
            } else {
                s_plot <- s_plot +
                    ggplot2::geom_text(aes(x = .data$xtext, y = .data$ytext,
                                          label = .data$Variables, hjust = .data$hadjust),
                                      color = 'black', size = text_size)
            }

            return(s_plot)
        }
    )
)
