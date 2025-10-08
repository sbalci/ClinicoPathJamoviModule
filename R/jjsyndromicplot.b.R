#' @title Syndromic Plot for PCA
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @import dplyr
#' @import ggrepel
#'

jjsyndromicplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjsyndromicplotClass",
    inherit = jjsyndromicplotBase,
    private = list(

        # init ----

        .init = function() {

            # Set plot size
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 600

            self$results$plot$setSize(plotwidth, plotheight)

        },

        # run ----

        .run = function() {

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

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')
            }

            # Run PCA ----
            private$.runPCA()

        },

        # Run PCA ----
        .runPCA = function() {

            # Get selected variables
            vars <- self$options$vars

            # Extract data
            mydata <- self$data
            pca_data <- mydata[, vars, drop = FALSE]

            # Remove missing values
            pca_data <- na.omit(pca_data)

            # Convert to numeric matrix
            pca_matrix <- as.matrix(sapply(pca_data, as.numeric))

            # Check for sufficient data
            if (nrow(pca_matrix) < 3) {
                stop('Insufficient data for PCA. Need at least 3 complete observations.')
            }

            # Run PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # Store PCA results for plotting
            private$.pcaResults <- pca
            private$.pcaData <- pca_matrix

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
            component <- self$options$component

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

            if (is.numeric(pca$scale)) {
                loadings <- as.data.frame((pca$rotation %*% diag(pca$sdev)))
            } else {
                loadings <- as.data.frame((pca$rotation %*% diag(pca$sdev)) / apply(pca_data, 2, sd))
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
            component <- self$options$component
            cutoff <- self$options$cutoff
            arrow_size_multi <- self$options$arrowsize
            text_size <- self$options$textsize
            repel <- self$options$repel
            plot_legend <- self$options$plotlegend
            plot_cutoff <- self$options$plotcutoff
            var_order <- self$options$varorder

            # Get colors
            color_low <- self$options$colorlow
            color_mid <- self$options$colormid
            color_high <- self$options$colorhigh
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
                                          var_order = 'abs decreasing',
                                          colors = c("steelblue1", "white", "firebrick1")) {

            old_scipen <- getOption('scipen')
            on.exit(options(scipen = old_scipen))
            options(scipen = 999)

            p <- load_df

            if (length(p$loading) < 1) {
                stop(paste(pc, 'has no loadings above cutoff'))
            }

            # Order variables
            if (var_order == 'abs decreasing') {
                p <- p %>% dplyr::arrange(desc(abs(.data$loading)))
            } else if (var_order == 'abs increasing') {
                p <- p %>% dplyr::arrange(abs(.data$loading))
            } else if (var_order == 'decreasing') {
                p <- p %>% dplyr::arrange(desc(.data$loading))
            } else if (var_order == 'increasing') {
                p <- p %>% dplyr::arrange(.data$loading)
            }

            # Filter by cutoff
            p <- p %>% dplyr::filter(abs(.data$loading) >= cutoff)

            if (nrow(p) < 1) {
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
                    size = abs(p$arrow_weight) * arrow_size_multi, show.legend = FALSE,
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
                                                                    size = abs(.data$z) * 20),
                                             inherit.aes = FALSE, show.legend = FALSE) +
                        geom_segment(data = legend_df_cutoff, aes(x = .data$x, y = .data$y,
                                                                  xend = .data$xend,
                                                                  yend = .data$yend,
                                                                  size = abs(.data$z) * 20,
                                                                  alpha = abs(.data$z)),
                                    inherit.aes = FALSE, show.legend = FALSE, color = "grey") +
                        ggplot2::annotate(geom = "text", x = 0, y = -12.5,
                                         label = paste0("Loadings > |", cutoff, "|"),
                                         size = text_size - 2) +
                        ggplot2::annotate(geom = "segment", x = cutoff_line.xmin,
                                         xend = cutoff_line.xmin,
                                         y = -10.6, yend = -11.4, size = 1, alpha = 0.5) +
                        ggplot2::annotate(geom = "segment", x = cutoff_line.xmax,
                                         xend = cutoff_line.xmax,
                                         y = -10.6, yend = -11.4, size = 1, alpha = 0.5)
                } else {
                    s_plot <- s_plot +
                        ggplot2::geom_segment(data = legend_df, aes(x = .data$x, y = .data$y,
                                                                    xend = .data$xend,
                                                                    yend = .data$yend, color = .data$z,
                                                                    size = abs(.data$z) * 20),
                                             inherit.aes = FALSE, show.legend = FALSE)
                }

                s_plot <- s_plot +
                    ggplot2::geom_segment(aes(x = max(legend_df$x), y = -11,
                                             xend = max(legend_df$x) + 0.1,
                                             yend = -11), color = colors[3],
                                         arrow = arrow(type = 'closed',
                                                      length = unit(0.1, 'cm'), angle = 25),
                                         size = 6, show.legend = FALSE, linejoin = 'mitre') +
                    ggplot2::geom_segment(aes(x = min(legend_df$x), y = -11,
                                             xend = min(legend_df$x) - 0.1,
                                             yend = -11), color = colors[1],
                                         arrow = arrow(type = 'closed',
                                                      length = unit(0.1, 'cm'), angle = 25),
                                         size = 6, show.legend = FALSE, linejoin = 'mitre') +
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
