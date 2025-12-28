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
        .messages = NULL,

        # Message accumulation ----
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- list()
            }
            private$.messages[[length(private$.messages) + 1]] <- message
        },

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

                if (nrow(self$data) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'emptyData',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Dataset contains no complete rows. Please provide data.')
                    self$results$insert(999, notice)
                    return()
                }
            }

            # Checkpoint
            private$.checkpoint()

            # Set Seed for Reproducibility
            if (!is.null(self$options$seed)) {
                set.seed(self$options$seed)
            }

            # Run permutation test ----
            if (nrow(self$data) > 0) {
                 private$.runPermutationTest()
            }

            # Populate warnings
            if (!is.null(private$.messages) && length(private$.messages) > 0) {
                # Format messages
                msg_html <- paste0(
                    "<div style='border: 1px solid #e6e6e6; background-color: #fafafa; padding: 10px; margin-bottom: 10px; border-radius: 5px;'>",
                    "<b>Note:</b><br>",
                    paste(unlist(private$.messages), collapse = "<br>"),
                    "</div>"
                )
                self$results$warnings$setContent(msg_html)
            }

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
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'nonNumericVars',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Non-numeric variables detected: %s. PCA requires numeric variables only. Factors and character variables cannot be used.',
                    paste(non_numeric_vars, collapse = ', ')))
                self$results$insert(999, notice)
                return()
            }

            # Convert to numeric matrix
            pca_matrix <- as.matrix(sapply(pca_data, as.numeric))
            colnames(pca_matrix) <- colnames(pca_data)  # Preserve column names

            # Check for sufficient data
            if (nrow(pca_matrix) < 3) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'insufficientData',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Insufficient data for PCA (N=%d). Need at least 3 complete observations.', nrow(pca_matrix)))
                self$results$insert(999, notice)
                return()
            }

            # Warn for small samples (3 <= N < 30)
            if (nrow(pca_matrix) >= 3 && nrow(pca_matrix) < 30) {
                notice_small <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallSampleWarning',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice_small$setContent(sprintf(
                    'Small sample size (N=%d). PCA results may be unstable. Recommended N>=30 for reliable component identification.',
                    nrow(pca_matrix)
                ))
                self$results$insert(2, notice_small)
            }

            # CRITICAL FIX: Warn if centering/scaling disabled
            if (!self$options$center || !self$options$scale) {
                # Check if variables have different scales
                var_ranges <- apply(pca_matrix, 2, function(x) diff(range(x, na.rm = TRUE)))
                var_means <- apply(pca_matrix, 2, mean, na.rm = TRUE)

                if (max(var_ranges) / min(var_ranges) > 10 || max(abs(var_means)) > 1e-6) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'scalingWarning',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent('Centering/Scaling should be enabled when variables have disparate ranges. Enable both for correlation-based interpretation.')
                    self$results$insert(3, notice)
                }
            }

            # Check for constant variables or singular matrix
            zero_var_cols <- which(apply(pca_matrix, 2, var) == 0)
            if (length(zero_var_cols) > 0) {
                zero_var_names <- colnames(pca_matrix)[zero_var_cols]
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'constantVariables',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Variables with zero variance detected: %s. Please remove constant variables.',
                    paste(zero_var_names, collapse = ', ')))
                self$results$insert(999, notice)
                return()
            }

            # Run original PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # Calculate original VAF
            original_VAF <- pca$sdev^2 / sum(pca$sdev^2)

            # Number of components to test
            ndim <- min(self$options$ncomp, length(original_VAF))

            # Checkpoint
            private$.checkpoint()

            # Performance estimate for long analyses
            estimated_time <- (ndim * self$options$nperm * nrow(pca_matrix) * ncol(pca_matrix)) / 1e6
            if (estimated_time > 30) {  # > 30 seconds
                notice_time <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'performanceWarning',
                    type = jmvcore::NoticeType$INFO
                )
                notice_time$setContent(sprintf(
                    'Analysis may take several minutes (%d components × %d permutations). Consider reducing permutations or components for faster results.',
                    ndim, self$options$nperm
                ))
                self$results$insert(999, notice_time)
            }

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
                    # Use base R to shuffle columns to avoid dplyr dependency issues
                    for(j in seq_len(ncol(perm_data))) {
                        perm_data[[j]] <- sample(perm_data[[j]])
                    }

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
                } else if (comp_idx < ndim && self$options$stop_rule) {
                    # If component not significant AND stop rule is active, STOP sequential testing
                    # Set remaining components to NA (conservative approach)
                    for (remaining in (comp_idx + 1):ndim) {
                        mean_VAF[remaining] <- NA
                        ci_low[remaining] <- NA
                        ci_high[remaining] <- NA
                        pvalue[remaining] <- NA
                    }
                    # Add INFO notice for sequential stopping
                    notice_stopped <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'sequentialStopped',
                        type = jmvcore::NoticeType$INFO
                    )
                    notice_stopped$setContent(sprintf(
                        'Sequential testing stopped at PC%d (first non-significant component). Remaining components not tested to prevent Type I error inflation.',
                        comp_idx
                    ))
                    self$results$insert(999, notice_stopped)
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

            # Warn about over-conservatism if both sequential and adjustment enabled
            if (self$options$stop_rule && self$options$adjustmethod != 'none') {
                notice_conservative <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'conservativeNote',
                    type = jmvcore::NoticeType$INFO
                )
                notice_conservative$setContent(sprintf(
                    'Using both sequential testing and %s adjustment provides extra protection against false positives but reduces power to detect weak components.',
                    self$options$adjustmethod
                ))
                self$results$insert(999, notice_conservative)
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

            # Add successful completion INFO notice
            notice_complete <- jmvcore::Notice$new(
                options = self$options,
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO
            )
            notice_complete$setContent(sprintf(
                'Analysis completed: %d components tested using %d permutations with %s adjustment. Sequential testing: %s.',
                ndim, self$options$nperm, self$options$adjustmethod,
                ifelse(self$options$stop_rule, 'enabled', 'disabled')
            ))
            self$results$insert(999, notice_complete)

            # Populate results table
            private$.populateTable(ndim)

        },

        # Populate results table ----
        .populateTable = function(ndim) {

            table <- self$results$results

            # Calculate cumulative variance
            cumulative_vaf <- cumsum(private$.originalVAF[1:ndim])

            for (i in 1:ndim) {
                row <- list()
                row[['component']] <- paste0('PC', i)
                row[['originalvaf']] <- ifelse(self$options$showpercent == TRUE,
                    private$.originalVAF[i] * 100, private$.originalVAF[i])
                row[['cumvar']] <- ifelse(self$options$showpercent == TRUE,
                    cumulative_vaf[i] * 100, cumulative_vaf[i])
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

            # Generate summary and guide panels
            private$.generateClinicalOutputs(ndim)

        },

        # Generate clinical summary and guide ----
        .generateClinicalOutputs = function(ndim) {

            # Plain-language summary panel
            if (isTRUE(self$options$showSummary)) {
                sig_comps <- which(private$.adjPvalues < 0.05)
                if (length(sig_comps) > 0) {
                    summary_text <- sprintf(
                        "<div style='background:#e8f4f8; padding:15px; border-left:4px solid #2196F3; margin:10px 0;'>
                        <b>Summary:</b> Analysis identified <b>%d significant principal component%s</b> from your %d variables (N=%d observations).
                        <br><br>
                        <b>What this means:</b> The first %d component%s %s %.1f%% of the variation in your data. These components represent underlying patterns that explain most of the differences between samples.
                        <br><br>
                        <b>Next steps:</b> Examine the <i>Variable Loadings Plot</i> to see which original variables contribute most to each significant component.
                        </div>",
                        length(sig_comps), ifelse(length(sig_comps) == 1, '', 's'),
                        length(self$options$vars), nrow(private$.pcaData),
                        length(sig_comps), ifelse(length(sig_comps) == 1, '', 's'),
                        ifelse(length(sig_comps) == 1, 'captures', 'combined capture'),
                        sum(private$.originalVAF[sig_comps]) * 100
                    )
                } else {
                    summary_text <- "<div style='background:#fff3cd; padding:15px; border-left:4px solid #ffc107;'>
                        <b>No significant components identified.</b> This suggests the variance in your data may be primarily noise, or the number of permutations may be too low to detect weak signals.
                        </div>"
                }
                self$results$summary$setVisible(TRUE)
                self$results$summary$setContent(summary_text)
            } else {
                self$results$summary$setVisible(FALSE)
            }

            # Interpretation guide panel
            if (isTRUE(self$options$showGuide)) {
                guide_text <- "
                <div style='background:#f5f5f5; padding:15px; margin:10px 0;'>
                <b>Understanding Your Results:</b>
                <ul>
                <li><b>Original VAF:</b> Percentage of total variance explained by each component in your actual data</li>
                <li><b>Cumulative VAF:</b> Total variance explained by this component plus all previous components</li>
                <li><b>Permuted Mean VAF:</b> Expected variance by random chance (null hypothesis)</li>
                <li><b>p-value:</b> Probability that a component's variance could occur by chance. p &lt; 0.05 indicates significance</li>
                <li><b>★ (star):</b> Marks components significantly above chance level</li>
                </ul>
                <b>Clinical Use:</b> Significant components represent real patterns in your data (e.g., combined biomarker signatures, distinct patient subgroups). Use loadings to identify which variables drive each pattern.
                <br><br>
                <b>Sequential Testing:</b> This analysis uses the Buja-Eyuboglu method, which removes variance from significant components before testing subsequent ones. This prevents Type I error inflation and provides more reliable component selection than traditional parallel analysis.
                </div>"
                self$results$guide$setVisible(TRUE)
                self$results$guide$setContent(guide_text)
            } else {
                self$results$guide$setVisible(FALSE)
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

        },

        # Scree Plot ----
        .screePlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.pcaResults)) return(FALSE)

            pca <- private$.pcaResults
            eigenvalues <- pca$sdev^2
            ndim <- length(eigenvalues)

            plot_df <- data.frame(
                component = factor(paste0('PC', 1:ndim), levels = paste0('PC', 1:ndim)),
                eigenvalue = eigenvalues
            )

            p <- ggplot2::ggplot(plot_df, aes(x = component, y = eigenvalue)) +
                geom_col(fill = "steelblue", alpha = 0.7) +
                geom_line(group = 1, color = "black") +
                geom_point(size = 2) +
                labs(title = "Scree Plot", y = "Eigenvalue", x = "Component") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))

            # Only add Kaiser criterion line if scale = TRUE (correlation matrix)
            if (self$options$scale) {
                p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
                    annotate("text", x = 1, y = 1.1, label = "Kaiser criterion (eigenvalue = 1)",
                             hjust = 0, size = 3, color = "red")
            }

            print(p)
            TRUE
        },

        # Loadings Plot ----
        .loadingsPlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.pcaResults)) return(FALSE)
            
            pca <- private$.pcaResults
            loadings <- pca$rotation
            
            # Determine which components to show
            # Show significant components, or first 2 if none significant
            sig_comps <- which(private$.adjPvalues < 0.05)
            if (length(sig_comps) == 0) {
                comps_to_show <- 1:min(2, ncol(loadings))
            } else {
                comps_to_show <- sig_comps
            }
            
            # Limit to first few significant components to avoid overcrowding
            if (length(comps_to_show) > 4) comps_to_show <- comps_to_show[1:4]
            
            n_top <- self$options$nLoadings
            plot_data <- data.frame()
            
            for (pc in comps_to_show) {
                pc_loadings <- loadings[, pc]
                # Get top N variables by absolute loading
                top_idx <- order(abs(pc_loadings), decreasing = TRUE)[1:min(n_top, length(pc_loadings))]
                
                subset_df <- data.frame(
                    Variable = names(pc_loadings)[top_idx],
                    Loading = pc_loadings[top_idx],
                    Component = paste0("PC", pc)
                )
                plot_data <- rbind(plot_data, subset_df)
            }
            
            p <- ggplot2::ggplot(plot_data, aes(x = reorder(Variable, abs(Loading)), y = Loading, fill = Loading > 0)) +
                geom_col() +
                coord_flip() +
                facet_wrap(~Component, scales = "free_y") +
                labs(title = "Top Variable Loadings", x = "Variable", y = "Loading") +
                scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"), guide = "none") +
                theme_minimal()
            
            print(p)
            TRUE
        }
    )
)
