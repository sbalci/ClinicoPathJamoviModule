#' @title PCA Loading Significance Test
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import pracma
#' @import scales
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
        .rowInfo = NULL,
        .varianceInfo = NULL,

        # init ----

        .init = function() {

            # Set plot size
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 700
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$loadingplot$setSize(plotwidth, plotheight)
            self$results$scree$setSize(plotwidth, plotheight)

        },

        # run ----

        .run = function() {

            # Initial Message ----
            if (length(self$options$vars) < 3) {

                # todo ----
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool performs permutation-based significance testing for PCA loadings using the
                <b>permV method</b> (Linting et al., 2011).
                <br><br>
                <b>How it works:</b>
                <ul>
                <li><b>permV method:</b> Permutes ONE variable at a time (not all variables simultaneously)</li>
                <li>Provides variable-specific and component-specific significance thresholds</li>
                <li>Higher statistical power than full-matrix permutation</li>
                <li>Proper Type I error control with Procrustes rotation</li>
                </ul>
                <br>
                <b>⚠️ CRITICAL REQUIREMENTS:</b>
                <ul>
                <li><b>Numeric variables only:</b> Factors and characters will be REJECTED (no silent coercion)</li>
                <li><b>Enable centering/scaling:</b> Required for correlation-based loading interpretation</li>
                <li>Without centering/scaling: Test compares RAW VARIANCE loadings, not correlation structure</li>
                </ul>
                <br>
                <b>Required:</b> Select at least 3 continuous numeric variables for PCA.
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

            # Informational summary for users ----
            row_info <- private$.rowInfo
            pca <- private$.pcaResults
            ncomp_available <- ncol(pca$rotation)
            ncomp_used <- min(self$options$ncomp, ncomp_available)
            trunc_note <- if (self$options$ncomp > ncomp_available) {
                glue::glue("<br><b>Requested {self$options$ncomp} components but only {ncomp_available} are available; testing PC1–PC{ncomp_available}.</b>")
            } else {
                ""
            }
            center_note <- if (!isTRUE(self$options$center) || !isTRUE(self$options$scale)) {
                "<br><b>Note:</b> Center/scale disabled; loadings reflect raw variance, not correlations."
            } else {
                ""
            }
            var_note <- if (!is.null(private$.varianceInfo)) {
                shown <- head(private$.varianceInfo, min(3, nrow(private$.varianceInfo)))
                paste0("<br>Variance explained: ",
                       paste0("PC", shown$component, "=", sprintf('%.1f%%', shown$variance * 100), collapse = "; "),
                       ". Cumulative PC1–PC", max(shown$component), ": ",
                       sprintf('%.1f%%', shown$cumulative[length(shown$cumulative)] * 100), ".")
            } else {
                ""
            }
            todo <- glue::glue(
                "<br>PCA loading test run with {length(self$options$vars)} variables; {row_info$rows_used} observations used (removed {row_info$rows_removed} rows with missing values).",
                "<br>Permutations: {self$options$nperm} per variable; Components tested: PC1–PC{ncomp_used}.",
                "{trunc_note}",
                "{center_note}",
                "{var_note}",
                "<br><hr>"
            )
            self$results$todo$setContent(todo)

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
            n_rows_before <- nrow(pca_data)
            pca_data <- na.omit(pca_data)
            private$.rowInfo <- list(
                rows_total = n_rows_before,
                rows_used = nrow(pca_data),
                rows_removed = n_rows_before - nrow(pca_data)
            )

            # CRITICAL FIX: Validate numeric inputs (prevent silent factor coercion)
            non_numeric <- sapply(pca_data, function(x) !is.numeric(x))
            if (any(non_numeric)) {
                non_numeric_vars <- names(pca_data)[non_numeric]
                stop(paste0(
                    'ERROR: Non-numeric variables detected: ',
                    paste(non_numeric_vars, collapse = ', '),
                    '\\n\\nPCA Loading Test requires numeric variables only. ',
                    'Factors and character variables cannot be used. ',
                    'Please select only continuous numeric variables.'
                ))
            }

            # Convert to numeric matrix
            pca_matrix <- as.matrix(sapply(pca_data, as.numeric))

            # Check for sufficient data
            if (nrow(pca_matrix) < 3) {
                stop('Insufficient data for PCA. Need at least 3 complete observations.')
            }

            # Optional seed for reproducibility
            if (!is.null(self$options$seed)) {
                set.seed(self$options$seed)
            }

            # CRITICAL FIX: Warn if centering/scaling disabled
            if (!self$options$center || !self$options$scale) {
                # Check if variables have different scales
                var_ranges <- apply(pca_matrix, 2, function(x) diff(range(x, na.rm = TRUE)))
                var_means <- apply(pca_matrix, 2, mean, na.rm = TRUE)

                if (max(var_ranges) / min(var_ranges) > 10 || max(abs(var_means)) > 1e-6) {
                    jmvcore::reject(paste0(
                        'WARNING: Centering and/or scaling are disabled, but variables have different scales.\\n\\n',
                        '⚠️ CRITICAL: Without centering/scaling, the permutation test for loadings ',
                        'compares RAW VARIANCE contributions, not standardized correlation-based loadings. ',
                        'Variables with larger variance will dominate the loadings.\\n\\n',
                        'RECOMMENDATION: Enable \"Center Variables\" and \"Scale Variables\" options for valid results.\\n\\n',
                        'If you proceed without centering/scaling, interpret loadings as reflecting raw variance, ',
                        'NOT standardized correlation structure (which is the standard interpretation of PCA loadings).'
                    ))
                }
            }

            # Run original PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # Extract standardized loadings
            original_loadings <- private$.stand_loadings(pca, pca_matrix)
            private$.varianceInfo <- pcaloadingtest_variance_info(pca, self$options$ncomp)

            # Number of components to test
            ndim <- min(self$options$ncomp, ncol(original_loadings))
            if (ndim < 1) {
                stop('No principal components available to test.')
            }

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

                        # CRITICAL FIX: Apply Procrustes rotation to align permuted loadings
                        if (requireNamespace('pracma', quietly = TRUE)) {
                            # pracma::procrustes returns:
                            # - $Q: rotated matrix (aligned temp_loading)
                            # - $P: rotation matrix (NOT what we want)
                            r <- pracma::procrustes(as.matrix(original_loadings[, 1:ndim]),
                                                   as.matrix(temp_loading))
                            # CRITICAL FIX: Use r$Q (rotated loadings), NOT r$P (rotation matrix)
                            perm_loadings_list[[v]] <- r$Q[v, ]
                        } else {
                            # CRITICAL WARNING: pracma not available, sign correction disabled
                            # This degrades to sign-unstable comparison with inflated Type I errors
                            jmvcore::reject(paste0(
                                'ERROR: The pracma package is required for Procrustes rotation ',
                                'in the permV method but is not installed.\n\n',
                                '⚠️ CRITICAL: Without Procrustes rotation, PCA loading significance tests ',
                                'suffer from sign indeterminacy. Different permutations can flip signs, ',
                                'causing grossly inflated Type I error rates.\n\n',
                                'SOLUTION: Install pracma with: install.packages("pracma")\n\n',
                                'The permV method (Linting et al., 2011) requires Procrustes alignment ',
                                'to handle rotation and reflection indeterminacy in PCA solutions.'
                            ))
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
            private$.varianceInfo <- pcaloadingtest_variance_info(pca, self$options$ncomp)

            # Populate results table
            private$.populateTable()
            private$.variance()

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
                comp_requested <- paste0('PC', self$options$componentfilter)
                if (!comp_requested %in% results_df$component) {
                    comp_requested <- unique(results_df$component)[1]
                }
                results_df <- results_df %>%
                    filter(.data$component == comp_requested)
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
                comp_req <- paste0('PC', self$options$componentfilter)
                if (comp_req %in% results_df$component) comp_req else unique(results_df$component)[1]
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

        },

        # variance table ----
        .variance = function(...) {

            if (is.null(private$.varianceInfo))
                return()

            var_df <- private$.varianceInfo
            table <- self$results$variance
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
        }
    )
)

#' Variance explained helper for pcaloadingtest
#'
#' @keywords internal
pcaloadingtest_variance_info <- function(pca, ncomp) {
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
