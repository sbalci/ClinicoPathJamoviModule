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
                This tool performs permutation-based significance testing for Principal Components.
                <br><br>
                <b>How it works:</b>
                <ul>
                <li>Tests which principal components explain more variance than random</li>
                <li>Uses nonparametric permutation to generate null distribution</li>
                <li>Provides p-values for objective component selection</li>
                </ul>
                <br>
                Based on Buja & Eyuboglu (1992) and syndRomics package methods.
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

            # Convert to numeric matrix
            pca_matrix <- as.matrix(sapply(pca_data, as.numeric))

            # Check for sufficient data
            if (nrow(pca_matrix) < 3) {
                stop('Insufficient data for PCA. Need at least 3 complete observations.')
            }

            # Run original PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # Calculate original VAF
            original_VAF <- pca$sdev^2 / sum(pca$sdev^2)

            # Number of components to test
            ndim <- min(self$options$ncomp, length(original_VAF))

            # Checkpoint
            private$.checkpoint()

            # Run permutations
            P <- self$options$nperm
            per_list <- list()

            for (i in 1:P) {
                # Checkpoint periodically
                if (i %% 50 == 0) {
                    private$.checkpoint()
                }

                # Permute each variable independently
                perm_data <- as.data.frame(pca_matrix)
                perm_data <- perm_data %>% mutate_all(.funs = sample)

                # Run PCA on permuted data
                tryCatch({
                    pca_per <- prcomp(perm_data, scale. = self$options$scale, center = self$options$center)
                    VAF_per <- pca_per$sdev^2 / sum(pca_per$sdev^2)
                    per_list[[i]] <- VAF_per[1:ndim]
                }, error = function(e) {
                    # Skip this permutation if error
                    per_list[[i]] <- rep(NA, ndim)
                })
            }

            # Checkpoint
            private$.checkpoint()

            # Convert to data frame
            df_per <- as.data.frame(do.call(rbind, per_list))
            colnames(df_per) <- paste0('PC', 1:ndim)

            # Calculate statistics
            mean_VAF <- colMeans(df_per, na.rm = TRUE)
            conf <- self$options$conflevel

            ci_low <- apply(df_per, 2, function(x) quantile(x, (1 - conf) / 2, na.rm = TRUE))
            ci_high <- apply(df_per, 2, function(x) quantile(x, 1 - (1 - conf) / 2, na.rm = TRUE))

            # Calculate p-values
            pvalue <- sapply(1:ndim, function(x) {
                (sum(df_per[, x] > original_VAF[x], na.rm = TRUE) + 1) / (P + 1)
            })

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
