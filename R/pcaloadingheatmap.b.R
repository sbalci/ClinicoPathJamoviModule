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
        .rowInfo = NULL,
        .varianceInfo = NULL,
        .originalVarNames = NULL,
        .notices = list(),

        # Variable name handling utilities ----
        .escapeVar = function(x) {
            if (is.null(x) || length(x) == 0) return(NULL)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        .cleanVarNames = function(df, vars) {
            # Store original names for later restoration
            private$.originalVarNames <- setNames(vars, sapply(vars, private$.escapeVar))

            # Rename columns to safe names
            for (v in vars) {
                safe_name <- private$.escapeVar(v)
                if (v != safe_name && v %in% names(df)) {
                    names(df)[names(df) == v] <- safe_name
                }
            }
            return(df)
        },

        .getOriginalName = function(safe_name) {
            if (is.null(private$.originalVarNames)) return(safe_name)
            idx <- match(safe_name, names(private$.originalVarNames))
            if (!is.na(idx)) {
                return(private$.originalVarNames[idx])
            }
            return(safe_name)
        },

        # Notices management ----
        .addNotice = function(name, message, severity = "WARNING") {
            if (!name %in% names(private$.notices)) {
                notice <- jmvcore::Notice$new(
                    name = name,
                    message = message,
                    severity = severity
                )
                private$.notices[[name]] <- notice
            }
        },

        .insertNotices = function() {
            if (length(private$.notices) == 0) return()

            # Priority order: ERROR > STRONG_WARNING > WARNING > INFO
            severities <- c("ERROR", "STRONG_WARNING", "WARNING", "INFO")
            position <- 1

            for (sev in severities) {
                for (notice_name in names(private$.notices)) {
                    notice <- private$.notices[[notice_name]]
                    if (notice$severity == sev) {
                        self$results$insert(position, notice)
                        position <- position + 1
                    }
                }
            }
            private$.notices <- list()  # Clear after insertion
        },

        # init ----

        .init = function() {

            # Set plot size
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$heatmap$setSize(plotwidth, plotheight)
            self$results$barmap$setSize(plotwidth, plotheight)
            self$results$scree$setSize(plotwidth, plotheight)

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
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                # Add ERROR notice if some variables selected but < 3
                if (!is.null(vars) && length(vars) > 0 && length(vars) < 3) {
                    private$.addNotice(
                        name = "insufficient_vars",
                        message = sprintf("PCA requires at least 3 variables. Currently selected: %d. Please add %d more variable(s).",
                                        length(vars), 3 - length(vars)),
                        severity = "ERROR"
                    )
                    private$.insertNotices()
                }
                return()

            } else {

                # Validate variable availability ----
                missing_vars <- setdiff(vars, names(self$data))
                if (length(missing_vars) > 0) {
                    private$.addNotice(
                        name = "missing_vars",
                        message = sprintf("The following variables are not in the dataset: %s. Please check variable selection and ensure all selected variables exist.",
                                        paste(missing_vars, collapse = ', ')),
                        severity = "ERROR"
                    )
                    private$.insertNotices()
                    return()
                }

                # Ensure numeric inputs ----
                var_data <- self$data[, vars, drop = FALSE]
                non_numeric <- vars[!vapply(var_data, is.numeric, logical(1))]
                if (length(non_numeric) > 0) {
                    private$.addNotice(
                        name = "non_numeric",
                        message = sprintf("PCA requires numeric variables. Non-numeric variables detected: %s. Please select only continuous numeric variables.",
                                        paste(non_numeric, collapse = ', ')),
                        severity = "ERROR"
                    )
                    private$.insertNotices()
                    return()
                }

                if (nrow(self$data) == 0) {
                    private$.addNotice(
                        name = "empty_data",
                        message = "Dataset contains no observations. Please provide data with at least 3 complete rows.",
                        severity = "ERROR"
                    )
                    private$.insertNotices()
                    return()
                }
            }

            # Run PCA ----
            private$.runPCA()

            # Informational summary for users ----
            pca <- private$.pcaResults
            row_info <- private$.rowInfo
            ncomp_available <- ncol(pca$rotation)
            ncomp_used <- min(self$options$ncomp, ncomp_available)
            var_explained <- (pca$sdev ^ 2) / sum(pca$sdev ^ 2)
            var_fmt <- paste0(
                paste0('PC', seq_len(ncomp_used), ': ', sprintf('%.1f', 100 * var_explained[seq_len(ncomp_used)]), '%'),
                collapse = '; ')
            cum_var <- sum(var_explained[seq_len(ncomp_used)])
            cum_var_fmt <- sprintf('Cumulative (PC1–PC%d): %.1f%%', ncomp_used, 100 * cum_var)

            # Add sample size warnings ----
            n_obs <- row_info$rows_used
            n_vars <- length(vars)

            if (n_obs < 3 * n_vars) {
                private$.addNotice(
                    name = "small_sample",
                    message = sprintf("Sample size (%d observations) is less than 3× the number of variables (%d). PCA results may be unstable. Ideally n ≥ %d.",
                                    n_obs, n_vars, 5 * n_vars),
                    severity = "WARNING"
                )
            }

            if (n_obs < 20) {
                private$.addNotice(
                    name = "very_small_sample",
                    message = sprintf("Very small sample size (%d observations). PCA with <20 observations is exploratory only and results may not generalize.",
                                    n_obs),
                    severity = "STRONG_WARNING"
                )
            }

            # Convert HTML warnings to notices ----
            if (isTRUE(self$options$textvalues) && isTRUE(self$options$starvalues)) {
                private$.addNotice(
                    name = "stars_hidden",
                    message = "Stars are hidden while numeric loadings are shown. Uncheck 'Show Loading Values' to display stars.",
                    severity = "INFO"
                )
            }

            if (!isTRUE(self$options$center)) {
                private$.addNotice(
                    name = "no_centering",
                    message = "PCA centering is disabled. This is uncommon and may bias loadings. Consider enabling centering unless you have specific methodological reasons.",
                    severity = "STRONG_WARNING"
                )
            }

            if (self$options$ncomp > ncomp_available) {
                private$.addNotice(
                    name = "truncated_components",
                    message = sprintf("Requested %d components but only %d are available (limited by min of variables, observations). Displaying PC1-PC%d.",
                                    self$options$ncomp, ncomp_available, ncomp_available),
                    severity = "WARNING"
                )
            }

            if (row_info$rows_removed > 0) {
                private$.addNotice(
                    name = "missing_data_removed",
                    message = sprintf("Removed %d row(s) with missing values. Analysis uses %d complete observations.",
                                    row_info$rows_removed, row_info$rows_used),
                    severity = "INFO"
                )
            }

            # Simplified HTML summary (warnings now in Notices) ----
            todo <- glue::glue(
                "<br><b>PCA Analysis Summary</b>",
                "<br>Variables: {length(vars)} | Observations: {row_info$rows_used}",
                "<br>Center: {ifelse(self$options$center, 'Yes', 'No')} | Scale: {ifelse(self$options$scale, 'Yes', 'No')}",
                "<br>Components displayed: PC1–PC{ncomp_used} (requested {self$options$ncomp}, available {ncomp_available})",
                "<br>Variance explained: {var_fmt}",
                "<br>{cum_var_fmt} | Cutoff: ±{self$options$cutoff}",
                "<br><hr>"
            )

            # Insert notices before HTML summary ----
            private$.insertNotices()
            self$results$todo$setContent(todo)
            private$.variance()

        },

        # Run PCA ----
        .runPCA = function() {

            # Get selected variables
            vars <- self$options$vars

            # Extract data
            mydata <- self$data
            pca_data <- mydata[, vars, drop = FALSE]

            # Clean variable names for safe R access
            pca_data <- private$.cleanVarNames(pca_data, vars)

            # Remove missing values
            n_rows_before <- nrow(pca_data)
            pca_data <- na.omit(pca_data)
            rows_used <- nrow(pca_data)
            private$.rowInfo <- list(
                rows_total = n_rows_before,
                rows_used = rows_used,
                rows_removed = n_rows_before - rows_used
            )

            # Convert to numeric matrix
            pca_matrix <- as.matrix(sapply(pca_data, as.numeric))

            # Check for sufficient data
            if (nrow(pca_matrix) < 3) {
                private$.addNotice(
                    name = "insufficient_obs",
                    message = sprintf("Insufficient data for PCA. Found %d complete observation(s) after removing missing values. Need at least 3 observations. Check for extensive missing data in selected variables.",
                                    nrow(pca_matrix)),
                    severity = "ERROR"
                )
                private$.insertNotices()
                return()
            }

            # Run PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # Store PCA results for plotting
            private$.pcaResults <- pca
            private$.pcaData <- pca_matrix
            private$.varianceInfo <- pcaloadingheatmap_variance_info(pca, ncomp = self$options$ncomp)

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

        # variance table ----
        .variance = function(...) {

            if (is.null(private$.varianceInfo))
                return()

            var_df <- private$.varianceInfo
            table <- self$results$variance

            # Clear existing rows - jamovi tables use deleteRows(), not clear()
            table$deleteRows()
            for (i in seq_len(nrow(var_df))) {
                table$addRow(rowKey = i, values = list(
                    component = paste0('PC', var_df$component[i]),
                    variance = var_df$variance[i],
                    cumulative = var_df$cumulative[i]
                ))
            }
        },

        # scree plot ----
        .scree = function(image, ggtheme, theme, ...) {

            if (is.null(private$.varianceInfo))
                return()

            var_df <- private$.varianceInfo

            s_plot <- ggplot(var_df, aes(x = factor(component, levels = component),
                                         y = variance)) +
                geom_col(fill = self$options$colorhigh, alpha = 0.8) +
                geom_line(aes(y = cumulative), group = 1, color = self$options$colorlow) +
                geom_point(aes(y = cumulative), color = self$options$colorlow) +
                scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                labs(x = 'Component', y = 'Variance explained (proportion)',
                     title = 'Variance Explained') +
                theme_minimal()

            print(s_plot)
            TRUE
        },

        # helper to extract standardized/correlation loadings ----
        .stand_loadings = function(pca, pca_data) {
            loadings <- pcaloadingheatmap_normalized_loadings(pca, pca_data, self$options$scale)
            colnames(loadings) <- paste0('PC', seq_len(ncol(pca$rotation)))
            # Restore original variable names for display in plots
            rownames(loadings) <- sapply(colnames(pca_data), private$.getOriginalName)
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

#' Variance explained by PCA components
#'
#' @keywords internal
pcaloadingheatmap_variance_info <- function(pca, ncomp) {
    var <- pca$sdev ^ 2
    prop <- var / sum(var)
    cum <- cumsum(prop)
    n_use <- min(length(prop), ncomp)
    data.frame(
        component = seq_len(n_use),
        variance = prop[seq_len(n_use)],
        cumulative = cum[seq_len(n_use)],
        stringsAsFactors = FALSE
    )
}
