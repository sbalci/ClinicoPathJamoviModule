#' @title Clinical Heatmap with tidyheatmaps
#' @description
#' Advanced heatmap visualization for clinical and biomedical data using the tidyheatmaps package.
#' This function creates publication-ready heatmaps from tidy data with support for clinical annotations,
#' multiple scaling methods, and comprehensive customization options. Perfect for visualizing biomarker
#' expression patterns, treatment responses, genomic data, and quality control metrics.
#'
#' @details
#' The Clinical Heatmap module leverages the powerful tidyheatmaps package to create sophisticated
#' visualizations of multivariate clinical data. It supports various data types and use cases:
#'
#' \strong{Supported Data Types:}
#' \itemize{
#'   \item \strong{Biomarker Expression:} IHC scores, molecular markers, protein levels
#'   \item \strong{Genomic Data:} Gene expression, mutation status, copy number variations
#'   \item \strong{Clinical Metrics:} Laboratory values, vital signs, diagnostic scores
#'   \item \strong{Quality Control:} Batch effects, instrument performance, reproducibility
#'   \item \strong{Treatment Response:} Longitudinal measurements, dose-response relationships
#' }
#'
#' \strong{Key Features:}
#' \itemize{
#'   \item \strong{Tidy Data Integration:} Works directly with long-format clinical datasets
#'   \item \strong{Clinical Annotations:} Row and column annotations for patient/biomarker characteristics
#'   \item \strong{Flexible Scaling:} Row, column, or bidirectional Z-score normalization
#'   \item \strong{Hierarchical Clustering:} Automatic pattern discovery with customizable clustering
#'   \item \strong{Color Palettes:} Clinical-friendly color schemes including colorblind-safe options
#'   \item \strong{Missing Data Handling:} Multiple strategies for incomplete clinical datasets
#'   \item \strong{Export Options:} Publication-ready outputs with customizable dimensions
#' }
#'
#' @section Clinical Applications:
#' \itemize{
#'   \item \strong{Biomarker Profiling:} Visualize multi-marker expression panels across patient cohorts
#'   \item \strong{Precision Medicine:} Display genomic alterations and therapeutic targets
#'   \item \strong{Quality Assurance:} Monitor laboratory performance and batch effects
#'   \item \strong{Clinical Trials:} Show treatment response patterns and dose relationships
#'   \item \strong{Diagnostic Patterns:} Reveal disease subtypes and molecular classifications
#' }
#'
#' @section Data Format Requirements:
#' The function expects data in "tidy" (long) format with three essential variables:
#' \itemize{
#'   \item \strong{Row Variable:} Defines heatmap rows (e.g., patient IDs, gene names)
#'   \item \strong{Column Variable:} Defines heatmap columns (e.g., biomarkers, time points)
#'   \item \strong{Value Variable:} Numeric values to visualize (e.g., expression levels, scores)
#' }
#'
#' @section Scaling Methods:
#' \itemize{
#'   \item \strong{None:} Raw values displayed without transformation
#'   \item \strong{Row:} Z-score normalization within each row (standardizes across columns)
#'   \item \strong{Column:} Z-score normalization within each column (standardizes across rows)
#'   \item \strong{Both:} Global Z-score normalization across entire dataset
#' }
#'
#' @section Missing Data Strategies:
#' \itemize{
#'   \item \strong{Exclude:} Remove rows/columns with missing values (complete cases only)
#'   \item \strong{Mean:} Replace missing values with column/row means
#'   \item \strong{Median:} Replace missing values with column/row medians (robust to outliers)
#'   \item \strong{Zero:} Replace missing values with zero (appropriate for some clinical scales)
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom tidyheatmaps tidyheatmap
# tidyHeatmap support commented out due to Bioconductor dependencies
# #' @importFrom tidyHeatmap heatmap layer_point layer_text layer_diamond layer_square layer_star
#' @importFrom dplyr select mutate group_by summarise across arrange filter pull n inner_join
#' @importFrom tidyr pivot_wider complete
#' @importFrom tibble column_to_rownames
#' @importFrom rlang sym
#' @importFrom ggplot2 ggtitle theme element_text labs
#' @importFrom magrittr %>%
#' @importFrom stats complete.cases median na.omit sd hclust cutree dist
#' @importFrom utils head tail
#' @importFrom factoextra fviz_nbclust
#' @importFrom cluster silhouette
#' @importFrom survival Surv survdiff survfit
#' @importFrom survminer ggsurvplot
#'
#' @return A jamovi analysis object containing the clinical heatmap and supporting information
#' @export clinicalheatmapClass
#'
#' @examples
#' \dontrun{
#' # TODO: Add proper examples with working datasets
#' # Currently commented to prevent installation errors
#' # Need to use existing datasets from ClinicoPathDescriptives
#'
#' # # Example 1: Basic biomarker heatmap
#' # data(histopathology)
#' #
#' # # Create sample biomarker data in long format
#' # biomarker_data <- data.frame(
#' #   patient_id = rep(paste0("P", 1:50), each = 4),
#' #   biomarker = rep(c("ER", "PR", "HER2", "Ki67"), 50),
#' #   expression = rnorm(200, mean = 50, sd = 20)
#' # )
#' #
#' # clinicalheatmap(
#' #   data = biomarker_data,
#' #   rowVar = "patient_id",
#' #   colVar = "biomarker",
#' #   valueVar = "expression",
#' #   scaleMethod = "row",
#' #   colorPalette = "RdBu"
#' # )
#' #
#' # # Example 2: Gene expression with clinical annotations
#' # gene_data <- expand.grid(
#' #   sample_id = paste0("S", 1:30),
#' #   gene = paste0("Gene", 1:20)
#' # )
#' # gene_data$expression <- rnorm(nrow(gene_data), mean = 0, sd = 1)
#' # gene_data$tumor_type <- rep(c("TypeA", "TypeB", "TypeC"), length.out = 30)
#' #
#' # clinicalheatmap(
#' #   data = gene_data,
#' #   rowVar = "sample_id",
#' #   colVar = "gene",
#' #   valueVar = "expression",
#' #   annotationCols = "tumor_type",
#' #   scaleMethod = "column",
#' #   clusterRows = TRUE,
#' #   clusterCols = TRUE
#' # )
#' }

clinicalheatmapClass <- if (requireNamespace("jmvcore")) R6::R6Class("clinicalheatmapClass",
    inherit = clinicalheatmapBase,
    private = list(

        .escapeVar = function(x) {
            # Escape variable names with spaces/special characters
            if (is.null(x) || length(x) == 0) return(x)
            make.names(x)
        },

        .createHTMLSection = function(title, content, style = "info", icon = NULL) {
            # Helper function to create consistent HTML sections
            styles <- list(
                info = "background-color: #e3f2fd; color: #1976d2; border-left: 4px solid #2196f3;",
                warning = "background-color: #fff3e0; color: #f57c00; border-left: 4px solid #ff9800;",
                error = "background-color: #ffebee; color: #d32f2f; border-left: 4px solid #f44336;",
                success = "background-color: #e8f5e9; color: #388e3c; border-left: 4px solid #4caf50;",
                neutral = "background-color: #f5f5f5; color: #333; border-left: 4px solid #757575;"
            )

            icon_html <- if (!is.null(icon)) paste0(icon, " ") else ""

            paste0(
                "<div style='", styles[[style]], " padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                "<h4 style='margin-top: 0; font-weight: bold;'>", icon_html, title, "</h4>",
                content,
                "</div>"
            )
        },

        .run = function() {
            # Check if required variables have been selected
            if (is.null(self$options$rowVar) || is.null(self$options$colVar) || is.null(self$options$valueVar)) {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>🔥 Welcome to Clinical Heatmap Analysis!</h3>
                <p><strong>Advanced heatmap visualization for clinical and biomedical data</strong></p>
                <p>Create publication-ready heatmaps from tidy clinical datasets with comprehensive customization options</p>

                <h4 style='color: #1976d2;'>Required Variables:</h4>
                <ol>
                <li><strong>Row Variable:</strong> Defines heatmap rows (e.g., patient IDs, gene names, samples)</li>
                <li><strong>Column Variable:</strong> Defines heatmap columns (e.g., biomarkers, time points, treatments)</li>
                <li><strong>Value Variable:</strong> Numeric data to visualize (e.g., expression levels, scores, measurements)</li>
                </ol>

                <h4 style='color: #1976d2;'>Data Format:</h4>
                <p>Your data should be in <strong>tidy (long) format</strong> where each row represents a single observation:</p>
                <ul>
                <li><strong>Patient_ID | Biomarker | Expression_Level</strong></li>
                <li><strong>Sample | Gene | FPKM_Value</strong></li>
                <li><strong>Subject | TimePoint | Measurement</strong></li>
                </ul>

                <h4 style='color: #1976d2;'>Key Features:</h4>
                <ul>
                <li><strong>Clinical Annotations:</strong> Add patient/sample characteristics and biomarker groupings</li>
                <li><strong>Flexible Scaling:</strong> Row, column, or global Z-score normalization</li>
                <li><strong>Smart Clustering:</strong> Hierarchical clustering to reveal data patterns</li>
                <li><strong>Color Palettes:</strong> Clinical-friendly and colorblind-safe options</li>
                <li><strong>Missing Data:</strong> Multiple strategies for incomplete datasets</li>
                <li><strong>Export Ready:</strong> Publication-quality outputs with custom dimensions</li>
                </ul>

                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Biomarker Profiling:</strong> Multi-marker expression panels</li>
                <li><strong>Genomic Analysis:</strong> Gene expression and mutation landscapes</li>
                <li><strong>Quality Control:</strong> Batch effects and instrument performance</li>
                <li><strong>Clinical Trials:</strong> Treatment response patterns</li>
                <li><strong>Precision Medicine:</strong> Molecular subtyping and therapeutic targets</li>
                </ul>

                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Powered by tidyheatmaps - bringing tidy data principles to clinical heatmap visualization</em>
                </p>
                </div>"

                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                error_msg <- "
                <div style='color: #721c24; background-color: #f8d7da; padding: 20px; border-radius: 8px;'>
                <h4>📊 Dataset Error</h4>
                <p><strong>Problem:</strong> The provided dataset contains no rows.</p>
                <h5>Possible Solutions:</h5>
                <ul>
                <li><strong>Check Data Import:</strong> Ensure your data was imported correctly</li>
                <li><strong>Data Structure:</strong> Verify your data is in tidy (long) format</li>
                <li><strong>Filter Issues:</strong> Check if any applied filters excluded all data</li>
                <li><strong>Variable Selection:</strong> Try selecting different variables</li>
                </ul>
                <p><em>💡 Tip: Use data exploration tools to examine your dataset structure before creating heatmaps.</em></p>
                </div>"
                self$results$clinicalSummary$setContent(error_msg)
                return()
            }

            # Check for tidyheatmaps package
            if (!requireNamespace("tidyheatmaps", quietly = TRUE)) {
                error_msg <- "
                <div style='color: #721c24; background-color: #f8d7da; padding: 20px; border-radius: 8px;'>
                <h4>📦 Required Package Missing</h4>
                <p><strong>Problem:</strong> The 'tidyheatmaps' package is required for clinical heatmap functionality.</p>
                <h5>Solution:</h5>
                <ol>
                <li><strong>Install tidyheatmaps Package:</strong></li>
                <ul>
                <li>In R Console: <code style='background-color: #f1f1f1; padding: 2px;'>install.packages('tidyheatmaps')</code></li>
                <li>Or use RStudio's Package tab to install 'tidyheatmaps'</li>
                </ul>
                <li><strong>Restart R Session:</strong> After installation, restart R/RStudio</li>
                <li><strong>Try Analysis Again:</strong> Re-run the clinical heatmap analysis</li>
                </ol>
                <h5>About tidyheatmaps:</h5>
                <p>The <strong>tidyheatmaps</strong> package provides advanced heatmap functionality with:</p>
                <ul>
                <li>Tidy data integration and intuitive syntax</li>
                <li>Advanced clustering and scaling options</li>
                <li>Flexible annotation systems</li>
                <li>Publication-ready visualizations</li>
                </ul>
                <p><em>💡 Tip: tidyheatmaps integrates seamlessly with the tidyverse ecosystem for clinical data analysis.</em></p>
                </div>"
                self$results$clinicalSummary$setContent(error_msg)
                return()
            }

            # Generate explanatory content
            private$.generateAboutAnalysis()

            # Get data and variables - escape names for safety
            dataset <- self$data
            row_var <- private$.escapeVar(self$options$rowVar)
            col_var <- private$.escapeVar(self$options$colVar)
            value_var <- private$.escapeVar(self$options$valueVar)

            # Perform comprehensive input validation
            validation_results <- private$.validateInputs(dataset, row_var, col_var, value_var)

            # Show validation summary if there are issues
            if (length(validation_results$warnings) > 0 || length(validation_results$info) > 0) {
                validation_html <- private$.generateValidationSummary(validation_results)
                self$results$clinicalSummary$setContent(validation_html)
            }

            # Stop if critical errors found
            if (validation_results$should_stop) {
                return()
            }

            # Prepare the data for heatmap
            heatmap_data <- private$.prepareHeatmapData(dataset, row_var, col_var, value_var)

            if (is.null(heatmap_data)) {
                error_msg <- "
                <div style='color: #721c24; background-color: #f8d7da; padding: 20px; border-radius: 8px;'>
                <h4>📋 Data Preparation Failed</h4>
                <p><strong>Problem:</strong> Unable to prepare data for heatmap visualization.</p>
                <h5>Common Causes:</h5>
                <ul>
                <li><strong>Data Format:</strong> Data may not be in proper tidy (long) format</li>
                <li><strong>Missing Values:</strong> Too many missing values in selected variables</li>
                <li><strong>Variable Types:</strong> Selected variables may have incorrect data types</li>
                <li><strong>Duplicates:</strong> Multiple values for same row-column combinations</li>
                </ul>
                <p><em>💡 Tip: Ensure your data has one row per row-variable/column-variable combination.</em></p>
                </div>"
                self$results$clinicalSummary$setContent(error_msg)
                return()
            }

            # Generate data summary if requested
            if (self$options$showDataSummary) {
                private$.generateDataSummary(heatmap_data, row_var, col_var, value_var)
            }

            # Generate clinical summary and interpretation
            private$.generateClinicalSummary(heatmap_data, row_var, col_var, value_var)
            private$.generateReportSentences(heatmap_data, row_var, col_var, value_var)

            if (self$options$showInterpretation) {
                private$.generateInterpretationGuide()
            }

            private$.generateAssumptions()

            # Populate annotation summaries if annotations are used
            if (!is.null(self$options$annotationCols) && length(self$options$annotationCols) > 0) {
                col_ann_html <- paste0(
                    "<div style='background-color: #f0f8ff; padding: 10px; border-radius: 4px;'>",
                    "<p><strong>Column Annotations:</strong> ", paste(self$options$annotationCols, collapse = ", "), "</p>",
                    "</div>"
                )
                self$results$annotations$columnAnnotations$setContent(col_ann_html)
            }

            if (!is.null(self$options$annotationRows) && length(self$options$annotationRows) > 0) {
                row_ann_html <- paste0(
                    "<div style='background-color: #f0f8ff; padding: 10px; border-radius: 4px;'>",
                    "<p><strong>Row Annotations:</strong> ", paste(self$options$annotationRows, collapse = ", "), "</p>",
                    "</div>"
                )
                self$results$annotations$rowAnnotations$setContent(row_ann_html)
            }

            # Store plot data for visualization
            plot_data <- list(
                data = heatmap_data,
                row_var = row_var,
                col_var = col_var,
                value_var = value_var,
                options = list(
                    scale_method = self$options$scaleMethod,
                    cluster_rows = self$options$clusterRows,
                    cluster_cols = self$options$clusterCols,
                    cluster_distance_rows = self$options$clusterDistanceRows,
                    cluster_method_rows = self$options$clusterMethodRows,
                    cluster_distance_cols = self$options$clusterDistanceCols,
                    cluster_method_cols = self$options$clusterMethodCols,
                    color_palette = self$options$colorPalette,
                    show_rownames = self$options$showRownames,
                    show_colnames = self$options$showColnames,
                    annotation_cols = self$options$annotationCols,
                    annotation_rows = self$options$annotationRows,
                    annotation_type = self$options$annotationType,
                    add_layer = self$options$addLayer,
                    layer_type = self$options$layerType,
                    layer_filter = self$options$layerFilter,
                    split_rows = self$options$splitRows,
                    split_cols = self$options$splitCols
                )
            )

            self$results$heatmap$setState(plot_data)

            # === ADVANCED FEATURES IMPLEMENTATION ===

            # Perform optimal K analysis if requested
            if (self$options$findOptimalK && self$options$clusterRows) {
                private$.performOptimalKAnalysis(heatmap_data, row_var, col_var, value_var)
            }

            # Export cluster assignments if requested
            cluster_assignments <- NULL
            if ((self$options$exportRowClusters || self$options$exportColClusters) &&
                (self$options$clusterRows || self$options$clusterCols)) {
                cluster_assignments <- private$.extractClusterAssignments(
                    heatmap_data, row_var, col_var, value_var
                )
            }

            # Perform survival analysis by clusters if requested
            if (self$options$survivalAnalysis &&
                !is.null(self$options$survivalTime) &&
                !is.null(self$options$survivalEvent) &&
                !is.null(cluster_assignments$row_clusters)) {
                private$.performSurvivalAnalysis(
                    dataset, cluster_assignments$row_clusters, row_var
                )
            }

            # Perform cluster comparison if requested
            if (self$options$clusterComparison &&
                !is.null(self$options$comparisonVars) &&
                length(self$options$comparisonVars) > 0 &&
                !is.null(cluster_assignments$row_clusters)) {
                private$.performClusterComparison(
                    dataset, cluster_assignments$row_clusters, row_var
                )
            }
        },

        .plotHeatmap = function(image, ggtheme, theme, ...) {
            # Heatmap plotting function using tidyheatmaps
            plot_data <- image$state

            if (is.null(plot_data) || is.null(plot_data$data)) {
                return(FALSE)
            }

            # Extract plotting parameters
            data <- plot_data$data
            row_var <- plot_data$row_var
            col_var <- plot_data$col_var
            value_var <- plot_data$value_var
            options <- plot_data$options

            # Safely create heatmap with error handling
            tryCatch({
                # Load required packages
                if (!requireNamespace("rlang", quietly = TRUE)) {
                    stop("rlang package is required")
                }

                # Build base heatmap call arguments
                heatmap_args <- list(
                    df = data,
                    rows = rlang::sym(row_var),
                    columns = rlang::sym(col_var),
                    values = rlang::sym(value_var),
                    scale = if(options$scale_method == "none") "none" else options$scale_method,
                    cluster_rows = options$cluster_rows,
                    cluster_cols = options$cluster_cols,
                    show_rownames = options$show_rownames,
                    show_colnames = options$show_colnames
                )

                # Add clustering options if clustering is enabled
                if (options$cluster_rows) {
                    heatmap_args$clustering_distance_rows <- options$cluster_distance_rows
                    heatmap_args$clustering_method_rows <- options$cluster_method_rows
                }

                if (options$cluster_cols) {
                    heatmap_args$clustering_distance_cols <- options$cluster_distance_cols
                    heatmap_args$clustering_method_cols <- options$cluster_method_cols
                }

                # Add column annotations if specified
                if (!is.null(options$annotation_cols) && length(options$annotation_cols) > 0) {
                    # tidyheatmaps expects column names as symbols, can be a vector
                    col_ann_syms <- lapply(options$annotation_cols, rlang::sym)
                    heatmap_args$annotation_col <- col_ann_syms
                }

                # Add row annotations if specified
                if (!is.null(options$annotation_rows) && length(options$annotation_rows) > 0) {
                    # tidyheatmaps expects column names as symbols, can be a vector
                    row_ann_syms <- lapply(options$annotation_rows, rlang::sym)
                    heatmap_args$annotation_row <- row_ann_syms
                }

                # Add annotation type
                if (!is.null(options$annotation_type)) {
                    heatmap_args$annotation_type <- options$annotation_type
                }

                # Add split options if > 1
                if (!is.null(options$split_rows) && options$split_rows > 1) {
                    heatmap_args$row_split <- options$split_rows
                }

                if (!is.null(options$split_cols) && options$split_cols > 1) {
                    heatmap_args$column_split <- options$split_cols
                }

                # Create the base heatmap using tidyheatmaps
                p <- do.call(tidyheatmaps::tidyheatmap, heatmap_args)

                # === LAYER FUNCTIONALITY NOT SUPPORTED ===
                # The tidyheatmaps package does not support add_tile() or add_point()
                # layer functions. These are available in tidyHeatmap (different package)
                # but not in tidyheatmaps (jbengler). Layer options are disabled in UI.
                #
                # To implement layers, would need to either:
                # 1. Switch to tidyHeatmap package (stemangiola) which has layer_*() functions
                # 2. Or create custom overlay logic using ggplot2 after heatmap generation
                # 3. Or remove layer options entirely from the interface

                # Apply color palette if specified
                if (!is.null(options$color_palette) && options$color_palette != "viridis") {
                    # Build color palette function
                    palette_colors <- switch(
                        options$color_palette,
                        "plasma" = grDevices::hcl.colors(100, "Plasma"),
                        "inferno" = grDevices::hcl.colors(100, "Inferno"),
                        "RdYlBu" = grDevices::hcl.colors(100, "RdYlBu"),
                        "RdBu" = grDevices::hcl.colors(100, "RdBu"),
                        "Blues" = grDevices::hcl.colors(100, "Blues"),
                        "Reds" = grDevices::hcl.colors(100, "Reds"),
                        grDevices::hcl.colors(100, "Viridis")  # default
                    )

                    # Apply palette
                    tryCatch({
                        p <- p %>% tidyheatmaps::add_palette(palette_colors)
                    }, error = function(e) {
                        # If palette fails, continue with default
                    })
                }

                # Add title and apply theme
                p <- p +
                    ggplot2::ggtitle("Clinical Heatmap") +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
                    )

                # Apply jamovi theme
                p <- p + ggtheme

                print(p)
                return(TRUE)

            }, error = function(e) {
                # If tidyheatmaps fails, create a simple fallback message
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error creating heatmap:\n", e$message), cex = 1.2, col = "red")
                title("Clinical Heatmap - Error")
                return(TRUE)
            })
        },

        # tidyHeatmap support commented out due to Bioconductor dependencies
        # .plotHeatmapAdvanced = function(image, ggtheme, theme, ...) {
        #     # Heatmap plotting function using tidyHeatmap (advanced, ComplexHeatmap-based)
        #     plot_data <- image$state
        #
        #     if (is.null(plot_data) || is.null(plot_data$data)) {
        #         return(FALSE)
        #     }
        #
        #     # Extract plotting parameters
        #     data <- plot_data$data
        #     row_var <- plot_data$row_var
        #     col_var <- plot_data$col_var
        #     value_var <- plot_data$value_var
        #     options <- plot_data$options
        #
        #     # Safely create heatmap with error handling
        #     tryCatch({
        #         # Load required packages
        #         if (!requireNamespace("tidyHeatmap", quietly = TRUE)) {
        #             stop("tidyHeatmap package is required")
        #         }
        #
        #         # Build base heatmap using tidyHeatmap
        #         p <- data %>%
        #             tidyHeatmap::heatmap(
        #                 .row = !!rlang::sym(row_var),
        #                 .column = !!rlang::sym(col_var),
        #                 .value = !!rlang::sym(value_var),
        #                 scale = if(options$scale_method == "none") "none" else options$scale_method,
        #                 cluster_rows = options$cluster_rows,
        #                 cluster_columns = options$cluster_cols,
        #                 show_row_names = options$show_rownames,
        #                 show_column_names = options$show_colnames
        #             )
        #
        #         # Add row/column clustering options if enabled
        #         if (options$cluster_rows) {
        #             # tidyHeatmap uses ComplexHeatmap internally with different parameter names
        #             # Note: Advanced distance/method options may need custom ComplexHeatmap config
        #         }
        #
        #         # Add annotations using tidyHeatmap's annotation functions
        #         if (!is.null(options$annotation_cols) && length(options$annotation_cols) > 0) {
        #             for (ann_col in options$annotation_cols) {
        #                 annotation_func <- switch(
        #                     options$annotation_type,
        #                     "bar" = tidyHeatmap::annotation_bar,
        #                     "point" = tidyHeatmap::annotation_point,
        #                     "line" = tidyHeatmap::annotation_line,
        #                     tidyHeatmap::annotation_tile  # default tile
        #                 )
        #
        #                 p <- p %>% annotation_func(!!rlang::sym(ann_col))
        #             }
        #         }
        #
        #         # Add layer/symbol overlays if requested
        #         if (options$add_layer) {
        #             layer_type <- options$layer_type
        #             layer_filter <- options$layer_filter
        #
        #             # Build filter expression if provided
        #             if (!is.null(layer_filter) && nchar(layer_filter) > 0) {
        #                 # Parse user's filter expression (e.g., "> 0.5" becomes value > 0.5)
        #                 filter_expr <- paste(value_var, layer_filter)
        #
        #                 # Add appropriate layer based on type
        #                 p <- p %>%
        #                     switch(
        #                         layer_type,
        #                         "point" = tidyHeatmap::layer_point(!!rlang::parse_expr(filter_expr)),
        #                         "text" = tidyHeatmap::layer_text(!!rlang::parse_expr(filter_expr)),
        #                         "star" = tidyHeatmap::layer_star(!!rlang::parse_expr(filter_expr)),
        #                         "square" = tidyHeatmap::layer_square(!!rlang::parse_expr(filter_expr)),
        #                         "diamond" = tidyHeatmap::layer_diamond(!!rlang::parse_expr(filter_expr)),
        #                         "arrow_up" = tidyHeatmap::layer_arrow_up(!!rlang::parse_expr(filter_expr)),
        #                         "arrow_down" = tidyHeatmap::layer_down(!!rlang::parse_expr(filter_expr)),
        #                         tidyHeatmap::layer_point(!!rlang::parse_expr(filter_expr))  # default
        #                     )
        #             } else {
        #                 # No filter - add layer to all cells
        #                 p <- p %>%
        #                     switch(
        #                         layer_type,
        #                         "point" = tidyHeatmap::layer_point(),
        #                         "text" = tidyHeatmap::layer_text(),
        #                         "star" = tidyHeatmap::layer_star(),
        #                         "square" = tidyHeatmap::layer_square(),
        #                         "diamond" = tidyHeatmap::layer_diamond(),
        #                         tidyHeatmap::layer_point()  # default
        #                     )
        #             }
        #         }
        #
        #         # Apply color palette if specified
        #         if (!is.null(options$color_palette) && options$color_palette != "viridis") {
        #             # Build color palette
        #             palette_colors <- switch(
        #                 options$color_palette,
        #                 "plasma" = grDevices::hcl.colors(100, "Plasma"),
        #                 "inferno" = grDevices::hcl.colors(100, "Inferno"),
        #                 "RdYlBu" = grDevices::hcl.colors(100, "RdYlBu"),
        #                 "RdBu" = grDevices::hcl.colors(100, "RdBu"),
        #                 "Blues" = grDevices::hcl.colors(100, "Blues"),
        #                 "Reds" = grDevices::hcl.colors(100, "Reds"),
        #                 grDevices::hcl.colors(100, "Viridis")  # default
        #             )
        #
        #             # tidyHeatmap handles palettes differently than tidyheatmaps
        #             # May need to pass as a named vector or use add_palette if available
        #         }
        #
        #         # Add title
        #         # Note: tidyHeatmap returns a different object type (ComplexHeatmap)
        #         # Title handling may differ from ggplot2
        #
        #         print(p)
        #         return(TRUE)
        #
        #     }, error = function(e) {
        #         # If tidyHeatmap fails, create a simple fallback message
        #         plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        #         text(1, 1, paste("Error creating heatmap:\n", e$message), cex = 1.2, col = "red")
        #         title("Clinical Heatmap (tidyHeatmap) - Error")
        #         return(TRUE)
        #     })
        # },

        .prepareHeatmapData = function(dataset, row_var, col_var, value_var) {
            # Prepare data for heatmap visualization
            tryCatch({
                # Determine which columns to retain
                required_vars <- c(row_var, col_var, value_var)

                # Add annotation columns if specified
                annotation_cols <- self$options$annotationCols
                annotation_rows <- self$options$annotationRows

                # Combine all needed variables
                vars_to_keep <- unique(c(required_vars, annotation_cols, annotation_rows))

                # Select only the variables we need (core + annotations)
                analysis_data <- dataset %>%
                    dplyr::select(dplyr::all_of(vars_to_keep))

                # Handle missing data based on user selection
                na_handling <- self$options$naHandling

                if (na_handling == "exclude") {
                    analysis_data <- analysis_data %>%
                        dplyr::filter(stats::complete.cases(.))
                } else if (na_handling == "mean") {
                    analysis_data <- analysis_data %>%
                        dplyr::group_by(!!rlang::sym(col_var)) %>%
                        dplyr::mutate(!!rlang::sym(value_var) := ifelse(is.na(!!rlang::sym(value_var)),
                                                                      mean(!!rlang::sym(value_var), na.rm = TRUE),
                                                                      !!rlang::sym(value_var))) %>%
                        dplyr::ungroup()
                } else if (na_handling == "median") {
                    analysis_data <- analysis_data %>%
                        dplyr::group_by(!!rlang::sym(col_var)) %>%
                        dplyr::mutate(!!rlang::sym(value_var) := ifelse(is.na(!!rlang::sym(value_var)),
                                                                      stats::median(!!rlang::sym(value_var), na.rm = TRUE),
                                                                      !!rlang::sym(value_var))) %>%
                        dplyr::ungroup()
                } else if (na_handling == "zero") {
                    analysis_data <- analysis_data %>%
                        dplyr::mutate(!!rlang::sym(value_var) := ifelse(is.na(!!rlang::sym(value_var)), 0, !!rlang::sym(value_var)))
                }

                # Check if we have enough data after processing
                if (nrow(analysis_data) == 0) {
                    return(NULL)
                }

                # Ensure we have complete data
                analysis_data <- analysis_data %>%
                    dplyr::filter(!is.na(!!rlang::sym(value_var)))

                return(analysis_data)

            }, error = function(e) {
                return(NULL)
            })
        },

        .validateInputs = function(dataset, row_var, col_var, value_var) {
            validation_results <- list(
                errors = character(0),
                warnings = character(0),
                info = character(0),
                should_stop = FALSE
            )

            # Check dataset validity
            if (is.null(dataset) || !is.data.frame(dataset)) {
                validation_results$errors <- c(validation_results$errors, "Dataset is not a valid data frame")
                validation_results$should_stop <- TRUE
                return(validation_results)
            }

            if (nrow(dataset) == 0) {
                validation_results$errors <- c(validation_results$errors, "Dataset contains no rows")
                validation_results$should_stop <- TRUE
                return(validation_results)
            }

            # Check variable selection
            required_vars <- c(row_var, col_var, value_var)
            missing_vars <- setdiff(required_vars, names(dataset))
            if (length(missing_vars) > 0) {
                validation_results$errors <- c(validation_results$errors,
                    paste("Variables not found in dataset:", paste(missing_vars, collapse = ", ")))
                validation_results$should_stop <- TRUE
                return(validation_results)
            }

            # Check value variable is numeric
            if (!is.numeric(dataset[[value_var]])) {
                validation_results$errors <- c(validation_results$errors,
                    paste("Value variable", value_var, "must be numeric"))
                validation_results$should_stop <- TRUE
                return(validation_results)
            }

            # Check for missing data
            missing_pct <- sum(is.na(dataset[[value_var]])) / nrow(dataset) * 100
            if (missing_pct > 50) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Value variable has", round(missing_pct, 1), "% missing data. Consider data quality review."))
            } else if (missing_pct > 20) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Value variable has", round(missing_pct, 1), "% missing data. Results may be affected."))
            }

            # Check data dimensions
            n_rows <- length(unique(dataset[[row_var]]))
            n_cols <- length(unique(dataset[[col_var]]))

            if (n_rows < 2) {
                validation_results$warnings <- c(validation_results$warnings,
                    "Very few unique values in row variable. Heatmap may not be informative.")
            }

            if (n_cols < 2) {
                validation_results$warnings <- c(validation_results$warnings,
                    "Very few unique values in column variable. Heatmap may not be informative.")
            }

            if (n_rows > 100 || n_cols > 100) {
                validation_results$warnings <- c(validation_results$warnings,
                    "Large number of rows/columns. Consider filtering or aggregation for better visualization.")
            }

            # Add success message if no major issues
            if (length(validation_results$errors) == 0 && length(validation_results$warnings) == 0) {
                validation_results$info <- c(validation_results$info,
                    "✓ Data validation passed. Heatmap can be generated.")
            }

            return(validation_results)
        },

        .generateValidationSummary = function(validation_results) {
            html_content <- "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;'>"
            html_content <- paste0(html_content, "<h4 style='color: #495057; margin-top: 0;'>📋 Data Validation Summary</h4>")

            # Add errors
            if (length(validation_results$errors) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: #f8d7da; padding: 10px; border-radius: 4px; margin: 10px 0;'>")
                html_content <- paste0(html_content, "<h5 style='color: #721c24; margin-top: 0;'>❌ Errors (Analysis Stopped)</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (error in validation_results$errors) {
                    html_content <- paste0(html_content, "<li style='color: #721c24;'>", error, "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }

            # Add warnings
            if (length(validation_results$warnings) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: #fff3cd; padding: 10px; border-radius: 4px; margin: 10px 0;'>")
                html_content <- paste0(html_content, "<h5 style='color: #856404; margin-top: 0;'>⚠️ Warnings</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (warning in validation_results$warnings) {
                    html_content <- paste0(html_content, "<li style='color: #856404;'>", warning, "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }

            # Add info messages
            if (length(validation_results$info) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: #d1ecf1; padding: 10px; border-radius: 4px; margin: 10px 0;'>")
                html_content <- paste0(html_content, "<h5 style='color: #0c5460; margin-top: 0;'>ℹ️ Information</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (info in validation_results$info) {
                    html_content <- paste0(html_content, "<li style='color: #0c5460;'>", info, "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }

            html_content <- paste0(html_content, "</div>")
            return(html_content)
        },

        .generateAboutAnalysis = function() {
            about_content <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #2c3e50; margin-top: 0;'>🔥 About Clinical Heatmaps</h4>",
                "<p><strong>Purpose:</strong> Clinical heatmaps visualize multivariate patterns in biomedical data, ",
                "commonly used for biomarker profiling, genomic analysis, and quality control assessment.</p>",
                "<ul style='margin-left: 20px;'>",
                "<li><strong>Biomarker Expression:</strong> Multi-marker panels across patient cohorts</li>",
                "<li><strong>Genomic Analysis:</strong> Gene expression and mutation landscapes</li>",
                "<li><strong>Quality Control:</strong> Batch effects and instrument performance</li>",
                "<li><strong>Treatment Response:</strong> Longitudinal measurements and dose relationships</li>",
                "<li><strong>Precision Medicine:</strong> Molecular subtyping and therapeutic targets</li>",
                "</ul>",
                "<p><strong>How to Use:</strong></p>",
                "<ol style='margin-left: 20px;'>",
                "<li>Ensure your data is in tidy (long) format</li>",
                "<li>Select row variable (e.g., patients, samples)</li>",
                "<li>Select column variable (e.g., biomarkers, genes)</li>",
                "<li>Select value variable (numeric measurements)</li>",
                "<li>Configure scaling, clustering, and annotation options</li>",
                "<li>Interpret patterns using clustering and color intensity</li>",
                "</ol>",
                "</div>"
            )
            self$results$aboutAnalysis$setContent(about_content)
        },

        .generateDataSummary = function(data, row_var, col_var, value_var) {
            # Generate data structure summary
            n_rows <- length(unique(data[[row_var]]))
            n_cols <- length(unique(data[[col_var]]))
            n_obs <- nrow(data)

            structure_data <- data.frame(
                dimension = c("Total Observations", "Unique Rows", "Unique Columns", "Data Completeness"),
                value = c(
                    as.character(n_obs),
                    as.character(n_rows),
                    as.character(n_cols),
                    paste0(round((1 - sum(is.na(data[[value_var]])) / nrow(data)) * 100, 1), "%")
                ),
                stringsAsFactors = FALSE
            )

            for (i in seq_len(nrow(structure_data))) {
                self$results$dataSummary$dataStructure$addRow(rowKey = i, values = list(
                    dimension = structure_data$dimension[i],
                    value = structure_data$value[i]
                ))
            }

            # Generate value summary statistics
            value_stats <- data.frame(
                statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum"),
                value = c(
                    round(mean(data[[value_var]], na.rm = TRUE), 3),
                    round(stats::median(data[[value_var]], na.rm = TRUE), 3),
                    round(stats::sd(data[[value_var]], na.rm = TRUE), 3),
                    round(min(data[[value_var]], na.rm = TRUE), 3),
                    round(max(data[[value_var]], na.rm = TRUE), 3)
                ),
                stringsAsFactors = FALSE
            )

            for (i in seq_len(nrow(value_stats))) {
                self$results$dataSummary$valueSummary$addRow(rowKey = i, values = list(
                    statistic = value_stats$statistic[i],
                    value = value_stats$value[i]
                ))
            }

            # Missing data report
            missing_by_var <- data.frame(
                variable = c(row_var, col_var, value_var),
                missing_count = c(
                    sum(is.na(data[[row_var]])),
                    sum(is.na(data[[col_var]])),
                    sum(is.na(data[[value_var]]))
                ),
                stringsAsFactors = FALSE
            )
            missing_by_var$missing_pct <- round(missing_by_var$missing_count / nrow(data) * 100, 1)

            for (i in seq_len(nrow(missing_by_var))) {
                self$results$dataSummary$missingData$addRow(rowKey = i, values = list(
                    variable = missing_by_var$variable[i],
                    missing_count = missing_by_var$missing_count[i],
                    missing_pct = missing_by_var$missing_pct[i]
                ))
            }
        },

        .generateClinicalSummary = function(data, row_var, col_var, value_var) {
            n_rows <- length(unique(data[[row_var]]))
            n_cols <- length(unique(data[[col_var]]))
            n_obs <- nrow(data)

            mean_val <- round(mean(data[[value_var]], na.rm = TRUE), 2)
            sd_val <- round(stats::sd(data[[value_var]], na.rm = TRUE), 2)

            clinical_summary <- paste0(
                "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;'>",
                "<h4 style='color: #2980b9; margin-top: 0;'>📊 Clinical Summary</h4>",
                "<p><strong>Dataset Overview:</strong> ", n_obs, " observations across ", n_rows, " ", row_var, " and ", n_cols, " ", col_var, "</p>",
                "<p><strong>Value Distribution:</strong> Mean = ", mean_val, " (SD = ", sd_val, ")</p>",
                "<p><strong>Scaling Method:</strong> ", switch(self$options$scaleMethod,
                    "none" = "Raw values (no scaling)",
                    "row" = "Row-wise Z-score normalization",
                    "column" = "Column-wise Z-score normalization",
                    "both" = "Global Z-score normalization"), "</p>",
                "<p><strong>Clustering:</strong> ",
                ifelse(self$options$clusterRows, "Rows clustered", "Rows not clustered"), ", ",
                ifelse(self$options$clusterCols, "Columns clustered", "Columns not clustered"), "</p>",
                "<p><em>💡 Tip: Use clustering to identify patterns and the color intensity to assess magnitude of differences.</em></p>",
                "</div>"
            )

            self$results$clinicalSummary$setContent(clinical_summary)
        },

        .generateReportSentences = function(data, row_var, col_var, value_var) {
            n_rows <- length(unique(data[[row_var]]))
            n_cols <- length(unique(data[[col_var]]))
            n_obs <- nrow(data)

            scale_text <- switch(self$options$scaleMethod,
                "none" = "without scaling",
                "row" = "with row-wise normalization",
                "column" = "with column-wise normalization",
                "both" = "with global normalization")

            cluster_text <- paste0(
                ifelse(self$options$clusterRows, "Row", "No row"), " clustering and ",
                ifelse(self$options$clusterCols, "column", "no column"), " clustering were applied"
            )

            report_sentence <- paste0(
                "Clinical heatmap analysis was performed on ", n_obs, " observations representing ",
                n_rows, " ", row_var, " and ", n_cols, " ", col_var, ". ",
                "Data visualization was conducted ", scale_text, ". ",
                cluster_text, " to reveal underlying patterns in the data."
            )

            report_content <- paste0(
                "<div style='background-color: #f0f8f0; padding: 15px; border-radius: 5px; border-left: 4px solid #27ae60;'>",
                "<h4 style='color: #27ae60; margin-top: 0;'>📝 Copy-Ready Clinical Summary</h4>",
                "<div style='background-color: white; padding: 10px; border-radius: 3px; font-family: Georgia, serif; line-height: 1.6;'>",
                "<p>", report_sentence, "</p>",
                "</div>",
                "<p style='margin-top: 10px;'><small><em>",
                "Note: Copy the text above for direct use in clinical reports. Modify as needed for your specific context.",
                "</em></small></p>",
                "</div>"
            )

            self$results$reportSentences$setContent(report_content)
        },

        .generateInterpretationGuide = function() {
            interpretation_content <- paste0(
                "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #1976d2; margin-top: 0;'>🔍 Heatmap Interpretation Guide</h3>",

                "<h4 style='color: #1976d2;'>Color Interpretation:</h4>",
                "<ul>",
                "<li><strong>Intensity:</strong> Color intensity represents magnitude of values</li>",
                "<li><strong>Scale:</strong> ", switch(self$options$scaleMethod,
                    "none" = "Raw values displayed without transformation",
                    "row" = "Values standardized within each row (Z-scores)",
                    "column" = "Values standardized within each column (Z-scores)",
                    "both" = "Values standardized across entire dataset"), "</li>",
                "<li><strong>Missing Data:</strong> ", switch(self$options$naHandling,
                    "exclude" = "Rows/columns with missing values excluded",
                    "mean" = "Missing values replaced with column means",
                    "median" = "Missing values replaced with column medians",
                    "zero" = "Missing values replaced with zero"), "</li>",
                "</ul>",

                "<h4 style='color: #1976d2;'>Pattern Recognition:</h4>",
                "<ul>",
                "<li><strong>Clusters:</strong> Groups of similar rows/columns indicate related patterns</li>",
                "<li><strong>Blocks:</strong> Rectangular regions of similar colors suggest correlated features</li>",
                "<li><strong>Gradients:</strong> Smooth color transitions indicate progressive changes</li>",
                "<li><strong>Outliers:</strong> Isolated distinct colors may indicate unusual observations</li>",
                "</ul>",

                "<h4 style='color: #1976d2;'>Clinical Applications:</h4>",
                "<ul>",
                "<li><strong>Biomarker Co-expression:</strong> Identify markers that vary together</li>",
                "<li><strong>Patient Subtyping:</strong> Discover molecular or clinical subtypes</li>",
                "<li><strong>Quality Control:</strong> Detect batch effects or systematic biases</li>",
                "<li><strong>Treatment Response:</strong> Identify responder vs. non-responder patterns</li>",
                "</ul>",

                "<h4 style='color: #1976d2;'>Next Steps:</h4>",
                "<ul>",
                "<li>Statistical testing for significant patterns</li>",
                "<li>Pathway analysis for clustered biomarkers</li>",
                "<li>Clinical correlation with patient outcomes</li>",
                "<li>Validation in independent datasets</li>",
                "</ul>",
                "</div>"
            )

            self$results$interpretation$setContent(interpretation_content)
        },

        .generateAssumptions = function() {
            assumptions_content <- paste0(
                "<div style='background-color: #fff8dc; padding: 15px; border-radius: 5px; border-left: 4px solid #f39c12;'>",
                "<h4 style='color: #e67e22; margin-top: 0;'>📋 Assumptions & Technical Notes</h4>",

                "<h5>Data Assumptions:</h5>",
                "<ul style='margin-left: 20px;'>",
                "<li>Data is in tidy (long) format with one observation per row</li>",
                "<li>Row and column variables uniquely identify observations</li>",
                "<li>Value variable contains numeric measurements</li>",
                "<li>Missing data pattern is appropriate for chosen handling method</li>",
                "</ul>",

                "<h5>Visualization Assumptions:</h5>",
                "<ul style='margin-left: 20px;'>",
                "<li>Color intensity accurately reflects data magnitude</li>",
                "<li>Hierarchical clustering reveals meaningful biological relationships</li>",
                "<li>Scaling method is appropriate for the data type and research question</li>",
                "<li>Sample size is sufficient for pattern detection</li>",
                "</ul>",

                "<h5>Technical Notes:</h5>",
                "<ul style='margin-left: 20px;'>",
                "<li><strong>Package:</strong> Powered by tidyheatmaps for advanced visualization</li>",
                "<li><strong>Clustering:</strong> Uses hierarchical clustering with default distance measures</li>",
                "<li><strong>Performance:</strong> Large datasets (>1000 rows/columns) may require optimization</li>",
                "<li><strong>Export:</strong> High-resolution outputs available for publication</li>",
                "</ul>",

                "<h5>Clinical Considerations:</h5>",
                "<ul style='margin-left: 20px;'>",
                "<li>Patterns should be validated with independent data</li>",
                "<li>Clinical significance may differ from statistical significance</li>",
                "<li>Consider pre-analytical factors (sample handling, batch effects)</li>",
                "<li>Interpret results in context of study design and patient population</li>",
                "</ul>",
                "</div>"
            )

            self$results$assumptions$setContent(assumptions_content)
        },

        # === ADVANCED FEATURES IMPLEMENTATION ===

        .performOptimalKAnalysis = function(data, row_var, col_var, value_var) {
            tryCatch({
                # Parse K range
                k_range_str <- self$options$kRange
                k_range <- eval(parse(text = k_range_str))
                if (length(k_range) < 2) k_range <- 2:8

                # Prepare data matrix for clustering
                mat_data <- data %>%
                    dplyr::select(!!rlang::sym(row_var), !!rlang::sym(col_var), !!rlang::sym(value_var)) %>%
                    tidyr::pivot_wider(names_from = !!rlang::sym(col_var), values_from = !!rlang::sym(value_var)) %>%
                    tibble::column_to_rownames(row_var) %>%
                    as.matrix()

                # Scale if needed
                if (self$options$scaleMethod == "row") {
                    mat_data <- t(scale(t(mat_data)))
                } else if (self$options$scaleMethod == "column") {
                    mat_data <- scale(mat_data)
                }

                # Store data for plotting
                self$results$optimalKAnalysis$elbowPlot$setState(list(
                    data = mat_data,
                    k_range = k_range,
                    method = self$options$clusterMethodRows
                ))

                self$results$optimalKAnalysis$silhouettePlot$setState(list(
                    data = mat_data,
                    k_range = k_range
                ))

                # Calculate optimal K using different methods
                optimal_k_results <- data.frame(
                    method = character(),
                    optimal_k = integer(),
                    metric_value = numeric(),
                    stringsAsFactors = FALSE
                )

                # Within-cluster sum of squares (elbow method)
                wss <- sapply(k_range, function(k) {
                    km <- stats::kmeans(mat_data, centers = k, nstart = 25)
                    km$tot.withinss
                })

                # Find elbow using simple difference method
                elbow_k <- k_range[which.max(diff(diff(wss)))] + 1
                optimal_k_results <- rbind(optimal_k_results, data.frame(
                    method = "Elbow Method",
                    optimal_k = as.integer(elbow_k),
                    metric_value = wss[k_range == elbow_k]
                ))

                # Silhouette method
                sil_scores <- sapply(k_range[k_range > 1], function(k) {
                    km <- stats::kmeans(mat_data, centers = k, nstart = 25)
                    sil <- cluster::silhouette(km$cluster, stats::dist(mat_data))
                    mean(sil[, 3])
                })

                best_sil_k <- k_range[k_range > 1][which.max(sil_scores)]
                optimal_k_results <- rbind(optimal_k_results, data.frame(
                    method = "Silhouette",
                    optimal_k = as.integer(best_sil_k),
                    metric_value = max(sil_scores)
                ))

                # Populate table
                for (i in seq_len(nrow(optimal_k_results))) {
                    self$results$optimalKAnalysis$optimalKTable$addRow(rowKey = i, values = list(
                        method = optimal_k_results$method[i],
                        optimal_k = optimal_k_results$optimal_k[i],
                        metric_value = optimal_k_results$metric_value[i]
                    ))
                }

            }, error = function(e) {
                # Silently handle errors
            })
        },

        .extractClusterAssignments = function(data, row_var, col_var, value_var) {
            tryCatch({
                # Prepare data matrix
                mat_data <- data %>%
                    dplyr::select(!!rlang::sym(row_var), !!rlang::sym(col_var), !!rlang::sym(value_var)) %>%
                    tidyr::pivot_wider(names_from = !!rlang::sym(col_var), values_from = !!rlang::sym(value_var)) %>%
                    tibble::column_to_rownames(row_var) %>%
                    as.matrix()

                result <- list(row_clusters = NULL, col_clusters = NULL)

                # Extract row clusters if requested
                if (self$options$exportRowClusters && self$options$clusterRows) {
                    # Perform hierarchical clustering on rows
                    dist_rows <- stats::dist(mat_data, method = self$options$clusterDistanceRows)
                    hc_rows <- stats::hclust(dist_rows, method = self$options$clusterMethodRows)

                    # Cut tree - use splitRows if > 1, otherwise estimate optimal K
                    n_clusters <- self$options$splitRows
                    if (n_clusters <= 1) n_clusters <- min(3, nrow(mat_data) - 1)

                    row_clusters <- stats::cutree(hc_rows, k = n_clusters)
                    result$row_clusters <- data.frame(
                        row_id = names(row_clusters),
                        cluster = as.integer(row_clusters),
                        stringsAsFactors = FALSE
                    )

                    # Populate table
                    for (i in seq_len(nrow(result$row_clusters))) {
                        self$results$clusterAssignments$rowClusterTable$addRow(rowKey = i, values = list(
                            row_id = result$row_clusters$row_id[i],
                            cluster = result$row_clusters$cluster[i]
                        ))
                    }
                }

                # Extract column clusters if requested
                if (self$options$exportColClusters && self$options$clusterCols) {
                    # Perform hierarchical clustering on columns
                    dist_cols <- stats::dist(t(mat_data), method = self$options$clusterDistanceCols)
                    hc_cols <- stats::hclust(dist_cols, method = self$options$clusterMethodCols)

                    n_clusters <- self$options$splitCols
                    if (n_clusters <= 1) n_clusters <- min(3, ncol(mat_data) - 1)

                    col_clusters <- stats::cutree(hc_cols, k = n_clusters)
                    result$col_clusters <- data.frame(
                        col_id = names(col_clusters),
                        cluster = as.integer(col_clusters),
                        stringsAsFactors = FALSE
                    )

                    # Populate table
                    for (i in seq_len(nrow(result$col_clusters))) {
                        self$results$clusterAssignments$colClusterTable$addRow(rowKey = i, values = list(
                            col_id = result$col_clusters$col_id[i],
                            cluster = result$col_clusters$cluster[i]
                        ))
                    }
                }

                return(result)
            }, error = function(e) {
                return(list(row_clusters = NULL, col_clusters = NULL))
            })
        },

        .performSurvivalAnalysis = function(dataset, row_clusters, row_var) {
            tryCatch({
                # Get survival variables
                surv_time_var <- self$options$survivalTime
                surv_event_var <- self$options$survivalEvent
                event_level <- self$options$survivalEventLevel

                # Merge cluster assignments with dataset
                cluster_data <- row_clusters
                names(cluster_data) <- c(row_var, "cluster")

                surv_data <- dataset %>%
                    dplyr::inner_join(cluster_data, by = row_var) %>%
                    dplyr::filter(!is.na(!!rlang::sym(surv_time_var)),
                                  !is.na(!!rlang::sym(surv_event_var)))

                # Create binary event indicator
                if (!is.null(event_level)) {
                    surv_data <- surv_data %>%
                        dplyr::mutate(event = as.integer(!!rlang::sym(surv_event_var) == event_level))
                } else {
                    surv_data <- surv_data %>%
                        dplyr::mutate(event = as.integer(!!rlang::sym(surv_event_var)))
                }

                # Perform survival analysis
                surv_obj <- survival::Surv(surv_data[[surv_time_var]], surv_data$event)
                fit <- survival::survfit(surv_obj ~ cluster, data = surv_data)

                # Log-rank test
                log_rank <- survival::survdiff(surv_obj ~ cluster, data = surv_data)

                # Populate results
                self$results$clusterSurvival$logRankTest$addRow(rowKey = 1, values = list(
                    chisq = log_rank$chisq,
                    df = length(log_rank$n) - 1,
                    p = 1 - stats::pchisq(log_rank$chisq, length(log_rank$n) - 1)
                ))

                # Extract survival summary by cluster
                for (i in seq_along(fit$strata)) {
                    cluster_id <- gsub("cluster=", "", names(fit$strata)[i])
                    cluster_idx <- fit$strata[1:i]
                    if (i > 1) cluster_idx <- sum(fit$strata[1:(i-1)]) + 1:fit$strata[i]
                    else cluster_idx <- 1:fit$strata[i]

                    median_surv <- summary(fit)$table[i, "median"]
                    ci_lower <- summary(fit)$table[i, paste0("0.95LCL")]
                    ci_upper <- summary(fit)$table[i, paste0("0.95UCL")]

                    self$results$clusterSurvival$survivalTable$addRow(rowKey = i, values = list(
                        cluster = cluster_id,
                        n = log_rank$n[i],
                        events = log_rank$obs[i],
                        median_surv = median_surv,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper
                    ))
                }

                # Store plot data
                self$results$clusterSurvival$kmPlot$setState(list(
                    fit = fit,
                    data = surv_data
                ))

            }, error = function(e) {
                # Silently handle errors
            })
        },

        .performClusterComparison = function(dataset, row_clusters, row_var) {
            tryCatch({
                # Merge cluster assignments with dataset
                cluster_data <- row_clusters
                names(cluster_data) <- c(row_var, "cluster")

                comp_data <- dataset %>%
                    dplyr::inner_join(cluster_data, by = row_var)

                comparison_vars <- self$options$comparisonVars

                # Compare each variable across clusters
                for (var in comparison_vars) {
                    if (is.numeric(comp_data[[var]])) {
                        # Use ANOVA or Kruskal-Wallis for numeric variables
                        test_result <- tryCatch({
                            aov_result <- stats::aov(as.formula(paste(var, "~ cluster")), data = comp_data)
                            p_value <- summary(aov_result)[[1]][["Pr(>F)"]][1]

                            # Get mean by cluster
                            means <- comp_data %>%
                                dplyr::group_by(cluster) %>%
                                dplyr::summarise(mean_val = mean(!!rlang::sym(var), na.rm = TRUE), .groups = "drop")

                            for (i in seq_len(nrow(means))) {
                                self$results$clusterCharacteristics$characteristicTable$addRow(
                                    rowKey = paste(var, means$cluster[i], sep = "_"),
                                    values = list(
                                        variable = var,
                                        cluster = paste("Cluster", means$cluster[i]),
                                        summary = sprintf("Mean: %.2f", means$mean_val[i]),
                                        p_value = if (i == 1) p_value else NA
                                    )
                                )
                            }
                        }, error = function(e) NULL)

                    } else {
                        # Use chi-square for categorical variables
                        test_result <- tryCatch({
                            tbl <- table(comp_data$cluster, comp_data[[var]])
                            chi_result <- stats::chisq.test(tbl)
                            p_value <- chi_result$p.value

                            # Get frequencies by cluster
                            freqs <- comp_data %>%
                                dplyr::group_by(cluster, !!rlang::sym(var)) %>%
                                dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
                                dplyr::group_by(cluster) %>%
                                dplyr::mutate(pct = n / sum(n) * 100)

                            for (i in seq_len(nrow(freqs))) {
                                self$results$clusterCharacteristics$characteristicTable$addRow(
                                    rowKey = paste(var, freqs$cluster[i], freqs[[var]][i], sep = "_"),
                                    values = list(
                                        variable = var,
                                        cluster = paste("Cluster", freqs$cluster[i]),
                                        summary = sprintf("%s: %d (%.1f%%)",
                                                          freqs[[var]][i], freqs$n[i], freqs$pct[i]),
                                        p_value = if (i == 1) p_value else NA
                                    )
                                )
                            }
                        }, error = function(e) NULL)
                    }
                }

                # Add interpretation
                interp_html <- paste0(
                    "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 5px;'>",
                    "<h4>Cluster Characteristics Interpretation</h4>",
                    "<p>P-values indicate whether there are significant differences in each variable across clusters.</p>",
                    "<ul>",
                    "<li>P < 0.05 suggests significant differences between clusters</li>",
                    "<li>Review cluster-specific summaries to understand patterns</li>",
                    "<li>Consider clinical significance alongside statistical significance</li>",
                    "</ul>",
                    "</div>"
                )
                self$results$clusterCharacteristics$clusterInterpretation$setContent(interp_html)

            }, error = function(e) {
                # Silently handle errors
            })
        },

        # Plot functions for advanced features
        .elbowPlot = function(image, ggtheme, theme, ...) {
            plot_state <- image$state
            if (is.null(plot_state)) return(FALSE)

            tryCatch({
                mat_data <- plot_state$data
                k_range <- plot_state$k_range
                method <- plot_state$method

                # Calculate WSS for each K
                wss <- sapply(k_range, function(k) {
                    km <- stats::kmeans(mat_data, centers = k, nstart = 25)
                    km$tot.withinss
                })

                # Create elbow plot
                plot(k_range, wss, type = "b", pch = 19, frame = FALSE,
                     xlab = "Number of Clusters (K)",
                     ylab = "Total Within-Cluster Sum of Squares",
                     main = "Elbow Method for Optimal K",
                     col = "steelblue", lwd = 2)
                grid()

                return(TRUE)
            }, error = function(e) {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error creating elbow plot:\n", e$message), col = "red")
                return(TRUE)
            })
        },

        .silhouettePlot = function(image, ggtheme, theme, ...) {
            plot_state <- image$state
            if (is.null(plot_state)) return(FALSE)

            tryCatch({
                mat_data <- plot_state$data
                k_range <- plot_state$k_range

                # Calculate silhouette scores
                sil_scores <- sapply(k_range[k_range > 1], function(k) {
                    km <- stats::kmeans(mat_data, centers = k, nstart = 25)
                    sil <- cluster::silhouette(km$cluster, stats::dist(mat_data))
                    mean(sil[, 3])
                })

                # Create silhouette plot
                plot(k_range[k_range > 1], sil_scores, type = "b", pch = 19, frame = FALSE,
                     xlab = "Number of Clusters (K)",
                     ylab = "Average Silhouette Width",
                     main = "Silhouette Analysis for Optimal K",
                     col = "darkgreen", lwd = 2)
                grid()
                abline(h = 0.5, col = "red", lty = 2)

                return(TRUE)
            }, error = function(e) {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error creating silhouette plot:\n", e$message), col = "red")
                return(TRUE)
            })
        },

        .kmPlotClusters = function(image, ggtheme, theme, ...) {
            plot_state <- image$state
            if (is.null(plot_state)) return(FALSE)

            tryCatch({
                requireNamespace("survminer", quietly = TRUE)

                fit <- plot_state$fit
                data <- plot_state$data

                # Create KM plot using survminer
                p <- survminer::ggsurvplot(
                    fit,
                    data = data,
                    pval = TRUE,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    risk.table.height = 0.25,
                    ggtheme = theme_minimal(),
                    palette = "jco",
                    legend.title = "Cluster",
                    legend.labs = paste("Cluster", unique(data$cluster)),
                    title = "Kaplan-Meier Survival by Cluster"
                )

                print(p$plot)
                return(TRUE)
            }, error = function(e) {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error creating KM plot:\n", e$message), col = "red")
                return(TRUE)
            })
        }
    )
)