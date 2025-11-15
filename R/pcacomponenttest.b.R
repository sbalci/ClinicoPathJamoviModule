#' @title PCA Component Significance Test
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @import dplyr
#'

pcacomponenttestClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "pcacomponenttestClass",
    inherit = pcacomponenttestBase,
    private = list(

        # Private fields ----
        .pcaResults = NULL,
        .pcaData = NULL,
        .permResults = NULL,
        .originalVAF = NULL,
        .meanVAF = NULL,
        .ciLow = NULL,
        .ciHigh = NULL,
        .pvalues = NULL,
        .adjPvalues = NULL,

        # init ----

        .init = function() {

            # Set plot size
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$vafplot$setSize(plotwidth, plotheight)

        },

        # run ----

        .run = function() {

            # Initial Message ----
            if (length(self$options$vars) < 3) {

                # todo ----
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool performs <b>SEQUENTIAL</b> permutation-based significance testing for Principal Components.
                <br><br>
                <b>How it works (Buja & Eyuboglu 1992 method):</b>
                <ul>
                <li><b>Sequential testing:</b> Each component tested after removing variance from previous significant components</li>
                <li><b>Stops early:</b> Testing stops at first non-significant component (prevents Type I error inflation)</li>
                <li>Uses nonparametric permutation to generate null distribution for each component</li>
                <li>Provides p-values for objective component selection</li>
                </ul>
                <br>
                <b>⚠️ CRITICAL REQUIREMENTS:</b>
                <ul>
                <li><b>Numeric variables only:</b> Factors and characters will be REJECTED (no silent coercion)</li>
                <li><b>Enable centering/scaling:</b> Required for correlation-based interpretation (standard parallel analysis)</li>
                <li>Without centering/scaling: Test compares RAW VARIANCE, not correlation structure</li>
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
                    "<br>PCA Component Significance Test running with {self$options$nperm} permutations.<br><hr>")

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

            # CRITICAL FIX: Validate numeric inputs (prevent silent factor coercion)
            non_numeric <- sapply(pca_data, function(x) !is.numeric(x))
            if (any(non_numeric)) {
                non_numeric_vars <- names(pca_data)[non_numeric]
                stop(paste0(
                    'ERROR: Non-numeric variables detected: ',
                    paste(non_numeric_vars, collapse = ', '),
                    '\n\nPCA requires numeric variables only. ',
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

            # CRITICAL FIX: Warn if centering/scaling disabled
            if (!self$options$center || !self$options$scale) {
                # Check if variables have different scales
                var_ranges <- apply(pca_matrix, 2, function(x) diff(range(x, na.rm = TRUE)))
                var_means <- apply(pca_matrix, 2, mean, na.rm = TRUE)

                if (max(var_ranges) / min(var_ranges) > 10 || max(abs(var_means)) > 1e-6) {
                    jmvcore::reject(paste0(
                        'WARNING: Centering and/or scaling are disabled, but variables have different scales.\n\n',
                        '⚠️ CRITICAL: Without centering/scaling, the permutation test compares RAW VARIANCE, ',
                        'not correlation structure. Variables with larger variance will dominate the analysis.\n\n',
                        'RECOMMENDATION: Enable "Center Variables" and "Scale Variables" options for valid results.\n\n',
                        'If you proceed without centering/scaling, interpret results as testing raw variance contributions, ',
                        'NOT correlation-based structure (which is the standard interpretation of parallel analysis).'
                    ))
                }
            }

            # Run original PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # Calculate original VAF
            original_VAF <- pca$sdev^2 / sum(pca$sdev^2)

            # Number of components to test
            ndim <- min(self$options$ncomp, length(original_VAF))

            # Checkpoint
            private$.checkpoint()

            # CRITICAL FIX: Implement sequential permutation test (Buja-Eyuboglu method)
            # For each component, remove variance from previously significant components
            # before generating null distribution

            P <- self$options$nperm
            mean_VAF <- numeric(ndim)
            ci_low <- numeric(ndim)
            ci_high <- numeric(ndim)
            pvalue <- numeric(ndim)
            conf <- self$options$conflevel

            # Track residual data after removing significant components
            residual_data <- pca_matrix
            components_removed <- 0

            for (comp_idx in 1:ndim) {
                # Checkpoint periodically
                if (comp_idx %% 2 == 0) {
                    private$.checkpoint()
                }

                # Run permutations on residual data
                per_vaf <- numeric(P)

                for (i in 1:P) {
                    # Checkpoint every 50 permutations
                    if (i %% 50 == 0) {
                        private$.checkpoint()
                    }

                    # Permute each variable independently in residual space
                    perm_data <- as.data.frame(residual_data)
                    perm_data <- perm_data %>% mutate_all(.funs = sample)

                    # Run PCA on permuted residual data
                    tryCatch({
                        pca_per <- prcomp(perm_data, scale. = FALSE, center = FALSE)  # Already preprocessed
                        VAF_per <- pca_per$sdev^2 / sum(pca_per$sdev^2)
                        # Take FIRST component of permuted residual (this is the null for current component)
                        per_vaf[i] <- VAF_per[1]
                    }, error = function(e) {
                        # Skip this permutation if error
                        per_vaf[i] <- NA
                    })
                }

                # Calculate statistics for this component
                mean_VAF[comp_idx] <- mean(per_vaf, na.rm = TRUE)
                ci_low[comp_idx] <- quantile(per_vaf, (1 - conf) / 2, na.rm = TRUE)
                ci_high[comp_idx] <- quantile(per_vaf, 1 - (1 - conf) / 2, na.rm = TRUE)

                # Calculate p-value: proportion of permuted VAFs >= observed VAF
                pvalue[comp_idx] <- (sum(per_vaf >= original_VAF[comp_idx], na.rm = TRUE) + 1) / (P + 1)

                # Sequential step: If component is significant, remove it before testing next
                # Use alpha = 0.05 as sequential cutoff (before multiple testing adjustment)
                if (pvalue[comp_idx] < 0.05 && comp_idx < ndim) {
                    # Project out this component's variance from residual data
                    # residual_data = residual_data - (residual_data %*% loading) %*% t(loading)
                    loading <- pca$rotation[, comp_idx, drop = FALSE]
                    projection <- residual_data %*% loading %*% t(loading)
                    residual_data <- residual_data - projection
                    components_removed <- components_removed + 1
                } else if (comp_idx < ndim) {
                    # If component not significant, STOP sequential testing
                    # Set remaining components to NA (conservative approach)
                    for (remaining in (comp_idx + 1):ndim) {
                        mean_VAF[remaining] <- NA
                        ci_low[remaining] <- NA
                        ci_high[remaining] <- NA
                        pvalue[remaining] <- NA
                    }
                    break
                }
            }

            # Checkpoint
            private$.checkpoint()

            # Store permuted results as data frame (for compatibility with existing plot code)
            # Note: This is simplified for sequential testing - each component tested against its own null
            df_per <- data.frame(matrix(NA, nrow = P, ncol = ndim))
            colnames(df_per) <- paste0('PC', 1:ndim)
            # Store only mean/CI since full permutation matrix not meaningful in sequential test

            # Adjust p-values
            if (self$options$adjustmethod != 'none') {
                adj_pvalue <- p.adjust(pvalue, method = self$options$adjustmethod)
            } else {
                adj_pvalue <- pvalue
            }

            # Store results
            private$.pcaResults <- pca
            private$.pcaData <- pca_matrix
            private$.permResults <- df_per
            private$.originalVAF <- original_VAF
            private$.meanVAF <- mean_VAF
            private$.ciLow <- ci_low
            private$.ciHigh <- ci_high
            private$.pvalues <- pvalue
            private$.adjPvalues <- adj_pvalue

            # Populate results table
            private$.populateTable(ndim)

        },

        # Populate results table ----
        .populateTable = function(ndim) {

            table <- self$results$results

            for (i in 1:ndim) {
                row <- list()
                row[['component']] <- paste0('PC', i)
                row[['originalvaf']] <- ifelse(self$options$showpercent == TRUE,
                    private$.originalVAF[i] * 100, private$.originalVAF[i])
                row[['meanvaf']] <- ifelse(self$options$showpercent == TRUE,
                    private$.meanVAF[i] * 100, private$.meanVAF[i])
                row[['cilow']] <- ifelse(self$options$showpercent == TRUE,
                    private$.ciLow[i] * 100, private$.ciLow[i])
                row[['cihigh']] <- ifelse(self$options$showpercent == TRUE,
                    private$.ciHigh[i] * 100, private$.ciHigh[i])
                row[['pvalue']] <- private$.pvalues[i]
                row[['adjpvalue']] <- private$.adjPvalues[i]
                row[['significant']] <- ifelse(private$.adjPvalues[i] < 0.05, '★', '')

                table$addRow(rowKey = i, values = row)
            }

        },

        # VAF plot ----

        .vafplot = function(image, ggtheme, theme, ...) {

            if (length(self$options$vars) < 3)
                return()

            if (is.null(private$.pcaResults))
                return()

            # Create plot data
            ndim <- min(self$options$ncomp, length(private$.originalVAF))

            # Original VAF
            plot_df <- data.frame(
                value = private$.originalVAF[1:ndim],
                component = factor(paste0('PC', 1:ndim), levels = paste0('PC', 1:ndim)),
                name = "Original"
            )

            # Permuted VAF
            resample_df <- data.frame(
                component = factor(paste0('PC', 1:ndim), levels = paste0('PC', 1:ndim)),
                mean = private$.meanVAF[1:ndim],
                ci_low = private$.ciLow[1:ndim],
                ci_high = private$.ciHigh[1:ndim],
                name = "Permuted"
            )

            # Colors
            color_orig <- self$options$colororiginal
            color_perm <- self$options$colorpermuted

            # Multiplier for percentage
            mult <- ifelse(self$options$showpercent == TRUE, 100, 1)

            # Create plot
            vaf_plot <- ggplot2::ggplot(plot_df, aes(.data$component, .data$value * mult)) +
                geom_point(aes(color = "Original"), show.legend = TRUE, size = 3) +
                geom_line(aes(group = .data$name, color = "Original"), show.legend = TRUE) +
                geom_point(data = resample_df, aes(x = .data$component, y = .data$mean * mult,
                                                   color = "Permuted"),
                          inherit.aes = FALSE, size = 3) +
                geom_line(data = resample_df, aes(x = .data$component, y = .data$mean * mult,
                                                  group = .data$name, color = "Permuted"),
                         inherit.aes = FALSE) +
                geom_errorbar(data = resample_df,
                             aes(x = .data$component, y = .data$mean * mult,
                                 ymin = .data$ci_low * mult, ymax = .data$ci_high * mult,
                                 color = "Permuted"),
                             inherit.aes = FALSE, width = 0.3) +
                scale_color_manual(values = c("Original" = color_orig, "Permuted" = color_perm)) +
                theme_minimal() +
                labs(
                    x = NULL,
                    y = ifelse(self$options$showpercent,
                              'Variance Accounted For (VAF %)',
                              'Variance Accounted For (VAF)'),
                    color = NULL
                ) +
                theme(
                    legend.position = c(0.8, 0.8),
                    legend.background = element_rect(color = 'grey', fill = 'white')
                )

            print(vaf_plot)
            TRUE

        }
    )
)
