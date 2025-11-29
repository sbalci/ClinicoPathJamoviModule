#' @title PCA Loading Heatmap
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import stats
#'

pcaloadingheatmapClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "pcaloadingheatmapClass",
    inherit = pcaloadingheatmapBase,
    private = list(

        # Private fields ----
        .pcaResults = NULL,
        .pcaData = NULL,

        # init ----

        .init = function() {

            # Set plot size
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$heatmap$setSize(plotwidth, plotheight)
            self$results$barmap$setSize(plotwidth, plotheight)

        },

        # run ----

        .run = function() {

            # Initial Message ----
            vars <- self$options$vars

            if (is.null(vars) || length(vars) < 3) {

                # todo ----
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool creates heatmap and barmap visualizations of PCA loadings.
                <br><br>
                <b>Features:</b>
                <ul>
                <li>Heatmap: Color-coded matrix of all loadings</li>
                <li>Barmap: Bar charts showing loading patterns across components</li>
                <li>Optional highlight stars for loadings above cutoff</li>
                <li>Publication-ready formatting</li>
                </ul>
                <br>
                Based on syndRomics package visualization methods.
                <br><br>
                <b>Required:</b> Select at least 3 continuous variables for PCA.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)
                return()

            } else {

                # Validate variable availability ----
                missing_vars <- setdiff(vars, names(self$data))
                if (length(missing_vars) > 0) {
                    stop(paste0('The following variables are not in the data: ',
                                paste(missing_vars, collapse = ', ')))
                }

                # Ensure numeric inputs ----
                var_data <- self$data[, vars, drop = FALSE]
                non_numeric <- vars[!vapply(var_data, is.numeric, logical(1))]
                if (length(non_numeric) > 0) {
                    stop(paste0('All selected variables must be numeric. Non-numeric variables detected: ',
                                paste(non_numeric, collapse = ', ')))
                }

                # todo ----
                todo <- glue::glue(
                    "<br>PCA Loading Heatmap generated.<br><hr>")

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

        },

        # Heatmap plot ----

        .heatmap = function(image, ggtheme, theme, ...) {

            if (length(self$options$vars) < 3)
                return()

            if (is.null(private$.pcaResults))
                return()

            pca <- private$.pcaResults
            pca_data <- private$.pcaData

            # Extract loadings
            load_df <- private$.stand_loadings(pca, pca_data)

            # Get parameters
            ndim <- 1:min(self$options$ncomp, ncol(load_df))
            cutoff <- self$options$cutoff
            text_values <- self$options$textvalues
            star_values <- self$options$starvalues
            text_size <- self$options$textsize

            # Colors
            colors <- c(self$options$colorlow, self$options$colormid, self$options$colorhigh)

            # Check ndim
            if (max(ndim) > ncol(load_df)) {
                ndim <- 1:ncol(load_df)
            }

            # Prepare data
            load_df$Variables <- rownames(load_df)

            load_df_long <- load_df %>%
                pivot_longer(-Variables, names_to = "component", values_to = "loading") %>%
                mutate(
                    component = factor(.data$component, levels = names(load_df)[names(load_df) != "Variables"]),
                    star = ifelse(abs(.data$loading) >= cutoff, '*', ''),
                    loading_txt = as.character(round(.data$loading, 2)),
                    weight = round(.data$loading, 2)
                )

            # Create heatmap
            h_plot <- load_df_long %>%
                filter(.data$component %in% paste('PC', ndim, sep = '')) %>%
                ggplot(aes(.data$component, .data$Variables, fill = .data$loading)) +
                geom_raster() +
                scale_fill_gradient2(
                    low = colors[1], high = colors[3], mid = colors[2],
                    limits = c(-1, 1), na.value = "transparent"
                ) +
                labs(fill = "s. loading") +
                ylab(NULL) + xlab(NULL) +
                theme_minimal()

            if (text_values) {
                h_plot <- h_plot + geom_text(aes(label = .data$loading_txt), size = text_size)
            } else if (star_values) {
                h_plot <- h_plot + geom_text(aes(label = .data$star), color = 'black')
            }

            print(h_plot)
            TRUE

        },

        # Barmap plot ----

        .barmap = function(image, ggtheme, theme, ...) {

            if (length(self$options$vars) < 3)
                return()

            if (is.null(private$.pcaResults))
                return()

            pca <- private$.pcaResults
            pca_data <- private$.pcaData

            # Extract loadings
            load_df <- private$.stand_loadings(pca, pca_data)

            # Get parameters
            ndim <- 1:min(self$options$ncomp, ncol(load_df))
            cutoff <- self$options$cutoff
            text_values <- self$options$textvalues
            star_values <- self$options$starvalues
            text_size <- self$options$textsize
            plot_cutoff <- self$options$plotcutoff
            gradient_color <- self$options$gradientcolor

            # Colors
            colors <- c(self$options$colorlow, self$options$colormid, self$options$colorhigh)

            # Check ndim
            if (max(ndim) > ncol(load_df)) {
                ndim <- 1:ncol(load_df)
            }

            # Prepare data
            load_df$Variables <- rownames(load_df)

            cutoff_df <- load_df %>%
                pivot_longer(-Variables, names_to = "component", values_to = "loading") %>%
                filter(.data$component %in% paste('PC', ndim, sep = '')) %>%
                group_by(.data$component) %>%
                summarise(count = n()) %>%
                mutate(
                    cutoff = cutoff,
                    component = factor(.data$component, levels = names(load_df)[names(load_df) != "Variables"])
                )

            load_df_long <- load_df %>%
                pivot_longer(-Variables, names_to = "component", values_to = "loading") %>%
                filter(.data$component %in% paste('PC', ndim, sep = '')) %>%
                left_join(cutoff_df, by = 'component') %>%
                group_by(.data$component) %>%
                mutate(
                    cutoff = cutoff,
                    star = ifelse(abs(.data$loading) >= cutoff, '*', ''),
                    loading_txt = as.character(round(.data$loading, 2)),
                    weight = round(.data$loading, 2)
                ) %>%
                ungroup() %>%
                mutate(component = factor(.data$component, levels = names(load_df)[names(load_df) != "Variables"]))

            # Create barmap
            b_plot <- load_df_long %>%
                ggplot(aes(.data$Variables, .data$loading)) +
                geom_col(show.legend = self$options$plotlegend) +
                theme_minimal() +
                coord_flip() +
                facet_grid(~.data$component)

            if (text_values) {
                b_plot <- b_plot + geom_text(aes(label = .data$loading_txt, y = .data$loading * 1.1), size = text_size)
            } else if (star_values) {
                b_plot <- b_plot + geom_text(aes(label = .data$star, y = .data$loading * 1.1), color = 'black')
            }

            if (gradient_color) {
                b_plot <- b_plot +
                    aes(.data$Variables, .data$loading, fill = .data$loading) +
                    scale_fill_gradient2(
                        low = colors[1], high = colors[3], mid = colors[2],
                        limits = c(-1, 1), na.value = "transparent"
                    )
            } else {
                b_plot <- b_plot +
                    aes(.data$Variables, .data$loading, fill = (.data$loading) > 0) +
                    scale_fill_manual(values = c(colors[1], colors[3]), na.value = "transparent") +
                    theme(legend.position = "none")
            }

            if (plot_cutoff) {
                b_plot <- b_plot +
                    geom_hline(data = cutoff_df, aes(yintercept = cutoff), color = colors[3], alpha = 0.6) +
                    geom_hline(data = cutoff_df, aes(yintercept = -cutoff), color = colors[1], alpha = 0.6)
            }

            b_plot <- b_plot +
                ylim(-1.1, 1.1) +
                labs(fill = 's.loading') +
                geom_hline(yintercept = 0, color = 'black') +
                ylab(NULL) + xlab(NULL)

            print(b_plot)
            TRUE

        },

        # helper to extract standardized/correlation loadings ----
        .stand_loadings = function(pca, pca_data) {
            loadings <- pcaloadingheatmap_normalized_loadings(pca, pca_data, self$options$scale)
            colnames(loadings) <- paste0('PC', seq_len(ncol(pca$rotation)))
            rownames(loadings) <- colnames(pca_data)
            return(as.data.frame(loadings))
        }
    )
)

#' Normalize PCA loadings for visualization
#'
#' Converts PCA rotation matrices to correlation-style loadings that are
#' comparable across different `scale` settings. Values are clipped within the
#' (-1, 1) range to avoid small numerical drifts outside the unit interval.
#'
#' @param pca A `prcomp` object.
#' @param pca_data Numeric matrix used to fit the PCA.
#' @param scaled Logical; whether the PCA was run with `scale.=TRUE`.
#'
#' @keywords internal
pcaloadingheatmap_normalized_loadings <- function(pca, pca_data, scaled) {
    loadings <- sweep(pca$rotation, 2, pca$sdev, `*`)

    if (!scaled) {
        var_sds <- apply(pca_data, 2, stats::sd)
        if (any(is.na(var_sds) | var_sds == 0)) {
            stop('Unable to compute loadings because one or more variables have zero variance.')
        }
        loadings <- sweep(loadings, 1, var_sds, `/`)
    }

    loadings <- pmax(pmin(loadings, 1), -1)
    loadings
}
