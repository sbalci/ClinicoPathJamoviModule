#' @title PCA Loading Significance Test
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import pracma
#'

pcaloadingtestClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "pcaloadingtestClass",
    inherit = pcaloadingtestBase,
    private = list(

        # Private fields ----
        .pcaResults = NULL,
        .pcaData = NULL,
        .permResults = NULL,
        .originalLoadings = NULL,
        .resultsDF = NULL,

        # init ----

        .init = function() {

            # Set plot size
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 700
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$loadingplot$setSize(plotwidth, plotheight)

        },

        # run ----

        .run = function() {

            # Initial Message ----
            if (length(self$options$vars) < 3) {

                # todo ----
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool performs permutation-based significance testing for PCA loadings.
                <br><br>
                <b>How it works:</b>
                <ul>
                <li>Uses permV method: permutes ONE variable at a time</li>
                <li>Provides variable-specific and component-specific significance thresholds</li>
                <li>Higher statistical power than traditional permutation methods</li>
                <li>Proper Type I error control</li>
                </ul>
                <br>
                Based on Linting et al. (2011) and syndRomics package methods.
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
                    "<br>PCA Loading Significance Test running with {self$options$nperm} permutations per variable.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')
            }

            # Run permutation test ----
            private$.runPermutationTest()

        },

        # Run Permutation Test ----
        .runPermutationTest = function() {

            # Checkpoint
            private$.checkpoint()

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

            # Run original PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # Extract standardized loadings
            original_loadings <- private$.stand_loadings(pca, pca_matrix)

            # Number of components to test
            ndim <- min(self$options$ncomp, ncol(original_loadings))

            # Checkpoint
            private$.checkpoint()

            # Run permV permutations (one variable at a time)
            P <- self$options$nperm
            nvars <- ncol(pca_matrix)

            per_list <- list()

            for (i in 1:P) {
                # Checkpoint periodically
                if (i %% 20 == 0) {
                    private$.checkpoint()
                }

                # Permute one variable at a time
                perm_loadings_list <- list()

                for (v in 1:nvars) {
                    perm_data <- as.data.frame(pca_matrix)
                    perm_data[, v] <- sample(pca_matrix[, v])

                    tryCatch({
                        pca_per <- prcomp(perm_data, scale. = self$options$scale,
                                         center = self$options$center)
                        temp_loading <- private$.stand_loadings(pca_per, perm_data)[, 1:ndim, drop = FALSE]

                        # Apply Procrustes rotation
                        if (requireNamespace('pracma', quietly = TRUE)) {
                            r <- pracma::procrustes(as.matrix(original_loadings[, 1:ndim]),
                                                   as.matrix(temp_loading))
                            perm_loadings_list[[v]] <- r$P[v, ]
                        } else {
                            perm_loadings_list[[v]] <- temp_loading[v, ]
                        }
                    }, error = function(e) {
                        perm_loadings_list[[v]] <- rep(NA, ndim)
                    })
                }

                per_list[[i]] <- do.call(rbind, perm_loadings_list)
            }

            # Checkpoint
            private$.checkpoint()

            # Combine permutation results
            all_perm <- do.call(rbind, per_list)
            colnames(all_perm) <- paste0('PC', 1:ndim)

            # Calculate statistics for each variable-component pair
            results_list <- list()
            conf <- self$options$conflevel

            for (comp in 1:ndim) {
                for (v in 1:nvars) {
                    # Get permuted values for this variable-component
                    perm_vals <- all_perm[seq(v, nrow(all_perm), by = nvars), comp]

                    # Calculate statistics
                    original <- original_loadings[v, comp]
                    mean_perm <- mean(perm_vals, na.rm = TRUE)
                    ci_low <- quantile(perm_vals, (1 - conf) / 2, na.rm = TRUE)
                    ci_high <- quantile(perm_vals, 1 - (1 - conf) / 2, na.rm = TRUE)

                    # Calculate p-value
                    pval <- (sum(abs(perm_vals) > abs(original), na.rm = TRUE) + 1) / (P + 1)

                    results_list[[length(results_list) + 1]] <- list(
                        variable = rownames(original_loadings)[v],
                        component = paste0('PC', comp),
                        original = original,
                        mean = mean_perm,
                        ci_low = ci_low,
                        ci_high = ci_high,
                        pvalue = pval
                    )
                }
            }

            results_df <- do.call(rbind, lapply(results_list, as.data.frame))

            # Adjust p-values by component
            if (self$options$adjustmethod != 'none') {
                results_df <- results_df %>%
                    group_by(.data$component) %>%
                    mutate(adj_pvalue = p.adjust(.data$pvalue, method = self$options$adjustmethod)) %>%
                    ungroup()
            } else {
                results_df$adj_pvalue <- results_df$pvalue
            }

            # Store results
            private$.pcaResults <- pca
            private$.pcaData <- pca_matrix
            private$.permResults <- all_perm
            private$.originalLoadings <- original_loadings
            private$.resultsDF <- results_df

            # Populate results table
            private$.populateTable()

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

        # Populate results table ----
        .populateTable = function() {

            if (is.null(private$.resultsDF))
                return()

            table <- self$results$results
            results_df <- private$.resultsDF

            # Filter by selected component if specified
            if (self$options$componentfilter > 0) {
                results_df <- results_df %>%
                    filter(.data$component == paste0('PC', self$options$componentfilter))
            }

            for (i in 1:nrow(results_df)) {
                row <- results_df[i, ]
                table$addRow(rowKey = i, values = list(
                    variable = row$variable,
                    component = row$component,
                    original = row$original,
                    mean = row$mean,
                    cilow = row$ci_low,
                    cihigh = row$ci_high,
                    pvalue = row$pvalue,
                    adjpvalue = row$adj_pvalue,
                    significant = ifelse(row$adj_pvalue < 0.05, '★', '')
                ))
            }

        },

        # Loading plot ----

        .loadingplot = function(image, ggtheme, theme, ...) {

            if (length(self$options$vars) < 3)
                return()

            if (is.null(private$.resultsDF))
                return()

            results_df <- private$.resultsDF

            # Filter by component if needed
            comp_to_plot <- if (self$options$componentfilter > 0) {
                paste0('PC', self$options$componentfilter)
            } else {
                unique(results_df$component)[1]
            }

            plot_data <- results_df %>% filter(.data$component == comp_to_plot)

            # Create barplot with error bars
            p <- ggplot2::ggplot(plot_data, aes(x = .data$variable, y = .data$original)) +
                geom_col(aes(fill = .data$original), show.legend = TRUE) +
                geom_errorbar(aes(ymin = .data$ci_low, ymax = .data$ci_high),
                             width = 0.3, color = "black") +
                scale_fill_gradient2(
                    low = self$options$colorlow,
                    mid = self$options$colormid,
                    high = self$options$colorhigh,
                    midpoint = 0,
                    name = "Loading"
                ) +
                coord_flip() +
                theme_minimal() +
                labs(
                    title = paste("Loading Significance for", comp_to_plot),
                    subtitle = paste("Error bars show", self$options$conflevel * 100, "% CI from permutation"),
                    x = NULL,
                    y = "Standardized Loading"
                ) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

            # Add significance stars
            sig_data <- plot_data %>% filter(.data$adj_pvalue < 0.05)
            if (nrow(sig_data) > 0) {
                p <- p + geom_text(data = sig_data,
                                  aes(x = .data$variable,
                                      y = .data$original * 1.1,
                                      label = "★"),
                                  size = 6, color = "gold3")
            }

            print(p)
            TRUE

        }
    )
)
