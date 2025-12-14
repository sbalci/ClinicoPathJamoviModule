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

            # Populate clinical guide
            clinical_guide <- glue::glue("
<div style='background:#f8f9fa; padding:15px; border-left:4px solid #0066cc; margin:10px 0;'>
<h4 style='color:#0066cc; margin-top:0;'>Clinical Applications in Pathology & Oncology:</h4>
<ul style='margin:8px 0;'>
<li><b>Biomarker Panel Validation:</b> Identify which biomarkers significantly contribute to patient stratification or disease subtypes</li>
<li><b>IHC Profile Analysis:</b> Determine which immunohistochemical markers define distinct pathological subtypes</li>
<li><b>Genomic Signature Discovery:</b> Find genes that significantly load on prognostic or predictive principal components</li>
<li><b>Tissue Feature Clustering:</b> Identify morphological or molecular features that drive tissue classification</li>
</ul>

<h4 style='color:#0066cc;'>Interpretation Guide:</h4>
<table style='width:100%; border-collapse:collapse; margin:8px 0;'>
<tr style='background:#e9ecef;'><th style='padding:6px; text-align:left;'>Loading Magnitude</th><th style='padding:6px; text-align:left;'>Interpretation</th></tr>
<tr><td style='padding:6px;'><b>|loading| &lt; 0.3</b></td><td style='padding:6px;'>Weak contribution (likely not clinically meaningful)</td></tr>
<tr style='background:#f8f9fa;'><td style='padding:6px;'><b>|loading| 0.3–0.5</b></td><td style='padding:6px;'>Moderate contribution (consider in context)</td></tr>
<tr><td style='padding:6px;'><b>|loading| &gt; 0.5</b></td><td style='padding:6px;'>Strong contribution (clinically important if p &lt; 0.05)</td></tr>
<tr style='background:#f8f9fa;'><td style='padding:6px;'><b>FDR-adjusted p &lt; 0.05</b></td><td style='padding:6px;'>Significant after controlling for multiple testing (recommended threshold)</td></tr>
</table>

<h4 style='color:#0066cc;'>Sample Size Recommendations:</h4>
<ul style='margin:8px 0;'>
<li><b>Minimum N:</b> At least 30 complete observations for stable results</li>
<li><b>Sample-to-Variable Ratio:</b> Recommend N/variables ≥ 10:1 (e.g., N ≥ 100 for 10 biomarkers)</li>
<li><b>Permutations:</b> Default 1,000 permutations provides good precision; increase to 5,000 for final publication</li>
</ul>

<h4 style='color:#0066cc;'>Statistical Method:</h4>
<p style='margin:8px 0; font-size:0.95em;'>
This analysis uses the <b>permV method</b> (Linting et al., 2011) which permutes one variable at a time
rather than all variables simultaneously. This provides higher statistical power and proper Type I error control
through <b>Procrustes rotation</b> to handle sign/reflection indeterminacy in PCA solutions.
</p>
</div>
            ")
            self$results$clinicalGuide$setContent(clinical_guide)

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

                if (nrow(self$data) == 0) {
                    notice <- list(
                        name = "emptyData",
                        title = "Empty Dataset",
                        content = "Data contains no complete rows. Please check for missing values or provide a valid dataset.",
                        level = "ERROR"
                    )
                    self$results$appendNotice(notice)
                    return()
                }
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

            # Warn about small sample sizes
            if (nrow(pca_data) < 10) {
                notice <- list(
                    name = "lowSampleSize",
                    title = "Small Sample Size Warning",
                    content = sprintf('Very small sample size (N=%d complete observations). PCA loadings and permutation tests require larger samples for reliable inference. Recommend N >= 30 for stable results.', nrow(pca_data)),
                    level = "STRONG_WARNING"
                )
                self$results$appendNotice(notice)
            }

            # Check Events-Per-Variable (EPV) ratio
            epv_ratio <- nrow(pca_data) / length(vars)
            if (epv_ratio < 10) {
                notice <- list(
                    name = "lowEPV",
                    title = "Low Sample-to-Variable Ratio",
                    content = sprintf('Sample size to variable ratio is low (N=%d, variables=%d, ratio=%.1f:1). PCA loadings may be unstable with low sample-to-variable ratios. Recommend ratio >= 10:1 (N >= %d for %d variables).',
                        nrow(pca_data), length(vars), epv_ratio, length(vars) * 10, length(vars)),
                    level = "STRONG_WARNING"
                )
                self$results$appendNotice(notice)
            }

            # CRITICAL FIX: Validate numeric inputs (prevent silent factor coercion)
            non_numeric <- sapply(pca_data, function(x) !is.numeric(x))
            if (any(non_numeric)) {
                non_numeric_vars <- names(pca_data)[non_numeric]
                notice <- list(
                    name = "nonNumericVars",
                    title = "Non-Numeric Variables Detected",
                    content = sprintf('Non-numeric variables detected: %s. PCA Loading Test requires numeric variables only. Factors and character variables cannot be used. Please select only continuous numeric variables.', paste(non_numeric_vars, collapse = ', ')),
                    level = "ERROR"
                )
                self$results$appendNotice(notice)
                return()
            }

            # Convert to numeric matrix
            pca_matrix <- as.matrix(sapply(pca_data, as.numeric))

            # Check for multicollinearity
            if (ncol(pca_matrix) >= 2 && nrow(pca_matrix) > ncol(pca_matrix)) {
                cor_matrix <- cor(pca_matrix, use = "pairwise.complete.obs")
                max_cor <- max(abs(cor_matrix[upper.tri(cor_matrix)]), na.rm = TRUE)
                if (max_cor > 0.95) {
                    notice <- list(
                        name = "highMulticollinearity",
                        title = "High Multicollinearity Detected",
                        content = sprintf('Very high correlation detected between variables (max |r| = %.3f). This may indicate redundant variables or measurement error. Consider removing highly correlated variables before PCA to improve interpretability.', max_cor),
                        level = "WARNING"
                    )
                    self$results$appendNotice(notice)
                }
            }

            # Check for sufficient data
            if (nrow(pca_matrix) < 3) {
                notice <- list(
                    name = "insufficientData",
                    title = "Insufficient Data",
                    content = sprintf('Insufficient data for PCA (N=%d complete observations). Need at least 3 complete observations. Check for missing values.', nrow(pca_matrix)),
                    level = "ERROR"
                )
                self$results$appendNotice(notice)
                return()
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
                    # Add non-blocking WARNING notice
                    notice <- list(
                        name = "centerscale_warning",
                        title = "Centering/Scaling Disabled",
                        content = "Center/scale disabled with mixed variable scales: loadings reflect raw variance (not correlations). Variables with larger variance will dominate. Enable 'Center Variables' and 'Scale Variables' for standard PCA interpretation.",
                        level = "WARNING"
                    )
                    self$results$appendNotice(notice)
                }
            }

            # Run original PCA
            pca <- prcomp(pca_matrix, center = self$options$center, scale. = self$options$scale)

            # Extract standardized loadings
            original_loadings <- private$.stand_loadings(pca, pca_matrix)
            private$.varianceInfo <- pcaloadingtest_variance_info(pca, self$options$ncomp)

            # Warn if tested components explain low variance
            var_info <- private$.varianceInfo
            ncomp_tested <- min(self$options$ncomp, nrow(var_info))
            total_var_explained <- sum(var_info$variance[1:ncomp_tested])
            if (total_var_explained < 0.50) {
                notice <- list(
                    name = "lowVarianceExplained",
                    title = "Low Variance Explained",
                    content = sprintf('First %d components explain only %.1f%% of total variance. Consider whether PCA is appropriate for this data or increase the number of components tested. Low variance explained suggests weak underlying structure.',
                        ncomp_tested, total_var_explained * 100),
                    level = "WARNING"
                )
                self$results$appendNotice(notice)
            }

            # Number of components to test
            ndim <- min(self$options$ncomp, ncol(original_loadings))
            if (ndim < 1) {
                notice <- list(
                    name = "noComponents",
                    title = "No Components Available",
                    content = "No principal components available to test. This may occur with constant variables or perfect multicollinearity.",
                    level = "ERROR"
                )
                self$results$appendNotice(notice)
                return()
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
                            notice <- list(
                                name = "pracmaMissing",
                                title = "Required Package Missing",
                                content = "The pracma package is required for Procrustes rotation in the permV method but is not installed. Without Procrustes rotation, loading significance tests suffer from sign indeterminacy causing inflated Type I error rates. Install pracma with: install.packages('pracma')",
                                level = "ERROR"
                            )
                            self$results$appendNotice(notice)
                            return()
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

                    # Store both encoded (for jamovi tables) and original (for plots)
                    var_name <- rownames(original_loadings)[v]
                    results_list[[length(results_list) + 1]] <- list(
                        variable = jmvcore::toB64(var_name),
                        variable_display = var_name,
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

            # Success completion notice
            pca <- private$.pcaResults
            infoNotice <- list(
                name = "analysisComplete",
                title = "Analysis Complete",
                content = sprintf('Analysis completed: %d variables tested across %d components using %d permutations per variable (total %d permutation runs).', length(self$options$vars), min(self$options$ncomp, ncol(pca$rotation)), self$options$nperm, self$options$nperm * length(self$options$vars)),
                level = "INFO"
            )
            self$results$appendNotice(infoNotice)

            # Generate copy-ready report text
            results_df <- private$.resultsDF
            sig_vars <- results_df %>% filter(.data$adj_pvalue < 0.05)
            n_sig <- nrow(sig_vars)

            # Get most significant loadings for each component (top 3)
            top_loadings <- results_df %>%
                arrange(.data$adj_pvalue) %>%
                group_by(.data$component) %>%
                slice(1:3) %>%
                ungroup()

            # Dynamic preprocessing statement
            preprocess_text <- if (self$options$center && self$options$scale) {
                "Variables were centered and scaled prior to analysis."
            } else if (self$options$center) {
                "Variables were centered (but not scaled) prior to analysis."
            } else if (self$options$scale) {
                "Variables were scaled (but not centered) prior to analysis."
            } else {
                "Variables were analyzed without centering or scaling (raw data)."
            }

            report_text <- sprintf(
                "Principal component analysis with permutation-based loading significance testing (permV method; %d permutations per variable; N=%d complete observations) identified %d significant variable-component associations (FDR-adjusted p < 0.05 after Benjamini-Hochberg correction). %s The first %d components explained %.1f%% of total variance.",
                self$options$nperm,
                private$.rowInfo$rows_used,
                n_sig,
                preprocess_text,
                min(self$options$ncomp, ncol(pca$rotation)),
                sum(private$.varianceInfo$variance[1:min(self$options$ncomp, nrow(private$.varianceInfo))]) * 100
            )

            copy_ready_html <- glue::glue("
<div style='background:#f0f7ff; padding:15px; border:2px solid #0066cc; border-radius:5px; margin:15px 0;'>
<h4 style='color:#0066cc; margin-top:0;'>📋 Copy-Ready Report Text</h4>
<p style='background:white; padding:12px; border-radius:3px; font-family:serif; line-height:1.6;'>
{report_text}
</p>
<button style='background:#0066cc; color:white; border:none; padding:8px 16px; border-radius:4px; cursor:pointer; font-size:14px;'
        onclick='navigator.clipboard.writeText(this.previousElementSibling.innerText).then(() => {{this.innerText=\"✓ Copied!\"; setTimeout(() => this.innerText=\"📋 Copy to Clipboard\", 2000);}})'>
📋 Copy to Clipboard
</button>

<h5 style='color:#0066cc; margin-top:15px; margin-bottom:8px;'>Reference:</h5>
<p style='font-size:0.9em; font-style:italic; margin:5px 0;'>
Linting M, van Os BJ, Meulman JJ. (2011). Statistical Significance of the Contribution of Variables to the PCA solution:
An Alternative Permutation Strategy. <i>Psychometrika</i>, 76(3):440-460. doi:10.1007/s11336-011-9238-0
</p>
</div>
            ")

            # Append to todo output
            current_todo <- self$results$todo$state
            self$results$todo$setContent(paste0(current_todo, copy_ready_html))

        },

        # Extract standardized loadings ----
        .stand_loadings = function(pca, pca_data) {

            if (is.numeric(pca$scale)) {
                # Case 1: scale=TRUE (prcomp applied scaling)
                # rotation × sdev gives standardized loadings
                # These are equivalent to correlations when data are scaled
                loadings <- as.data.frame((pca$rotation %*% diag(pca$sdev)))
            } else {
                # Case 2: scale=FALSE (no scaling applied by prcomp)
                # Manually standardize by dividing by raw standard deviations
                # This ensures loadings are on comparable scale even with raw data
                # Without this, loadings would reflect raw variance contributions
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

            # Create barplot with error bars (use variable_display for human-readable labels)
            p <- ggplot2::ggplot(plot_data, aes(x = .data$variable_display, y = .data$original)) +
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
                                  aes(x = .data$variable_display,
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
