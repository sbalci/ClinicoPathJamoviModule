#' @title Genomic Landscape Visualization
#' @description Creates oncoplots (mutation landscapes) to visualize genomic alterations across genes and samples with optional clinical annotations
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import tidyr
#' @import scales
#' @import patchwork
#' @param data Data frame containing mutation and clinical data
#' @param sampleVar Column name for sample identifiers
#' @param geneVars Column names for gene mutation variables
#' @param clinicalVars Optional column names for clinical annotation variables
#' @param plotType Type of plot to generate (oncoplot, frequency, cooccurrence)
#' @return A list containing plot object and summary statistics
#' @examples
#' data <- data.frame(
#'   SampleID = paste0("S", 1:10),
#'   TP53 = c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1),
#'   KRAS = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0)
#' )
#' jjoncoplot(data, "SampleID", c("TP53", "KRAS"))
#     selkamand/ggoncoplot
#' @export jjoncoplotClass

jjoncoplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjoncoplotClass",
    inherit = jjoncoplotBase,
    private = list(
        .guidanceNotes = NULL,
        .effectiveOptions = NULL,

        .optionsWithPreset = function() {
            # Build an effective options list without mutating self$options
            opts <- list(
                sampleVar = self$options$sampleVar,
                geneVars = self$options$geneVars,
                clinicalVars = self$options$clinicalVars,
                mutationTypeVar = self$options$mutationTypeVar,
                clinicalPreset = self$options$clinicalPreset %||% "custom",
                plotType = self$options$plotType %||% "oncoplot",
                topn = self$options$topn %||% 10,
                genesToInclude = self$options$genesToInclude,
                genesToIgnore = self$options$genesToIgnore,
                maxSamples = self$options$maxSamples %||% 50,
                sortBy = self$options$sortBy %||% "hierarchical",
                colorScheme = self$options$colorScheme %||% "default",
                showMutationLoad = isTRUE(self$options$showMutationLoad),
                showTMBTable = isTRUE(self$options$showTMBTable),
                showGeneFreq = isTRUE(self$options$showGeneFreq),
                drawMarginalPlots = isTRUE(self$options$drawMarginalPlots),
                showTMB = isTRUE(self$options$showTMB),
                log10TransformTMB = isTRUE(self$options$log10TransformTMB),
                showClinicalAnnotation = isTRUE(self$options$showClinicalAnnotation),
                plotWidth = self$options$plotWidth %||% 800,
                plotHeight = self$options$plotHeight %||% 600,
                fontSize = self$options$fontSize %||% 10,
                showLegend = isTRUE(self$options$showLegend)
            )

            preset <- opts$clinicalPreset
            if (!is.null(preset) && preset != "custom") {
                if (preset == "tumor_profiling") {
                    opts$plotType <- "oncoplot"
                    opts$topn <- max(opts$topn, 20)
                    opts$showMutationLoad <- TRUE
                    opts$showGeneFreq <- TRUE
                    opts$showTMB <- TRUE
                    opts$drawMarginalPlots <- TRUE
                } else if (preset == "biomarker_discovery") {
                    opts$plotType <- "frequency"
                    opts$topn <- max(opts$topn, 30)
                    opts$showGeneFreq <- TRUE
                    opts$showMutationLoad <- FALSE
                } else if (preset == "therapeutic_targets") {
                    opts$plotType <- "oncoplot"
                    opts$topn <- opts$topn
                    opts$showMutationLoad <- TRUE
                    opts$showLegend <- TRUE
                } else if (preset == "pathway_analysis") {
                    opts$plotType <- "cooccurrence"
                    opts$showLegend <- TRUE
                }
            }

            # Log transformation is only meaningful when TMB is displayed
            if (!isTRUE(opts$showTMB)) {
                opts$log10TransformTMB <- FALSE
            }

            private$.effectiveOptions <- opts
            return(opts)
        },

        .calculateHierarchicalScore = function(df, selected_genes, gene_frequencies) {
            # ggoncoplot hierarchical sorting algorithm
            # Uses base-2 exponential weighting based on gene frequency rank
            
            # Rank genes by frequency (most frequent = rank 1)
            ranked_genes <- gene_frequencies$gene
            n_genes <- length(ranked_genes)
            
            # Calculate hierarchical score for each sample
            scores <- rep(0, nrow(df))
            
            for (i in seq_along(ranked_genes)) {
                gene <- ranked_genes[i]
                # Weight decreases exponentially: 2^(n_genes - rank)
                weight <- 2^(n_genes - i)
                # Add weighted mutation status to score
                scores <- scores + (df[[gene]] * weight)
            }
            
            return(scores)
        },

        .calculateCooccurrence = function(prepared_data) {
            # Calculate co-occurrence statistics for genes
            genes <- prepared_data$selected_genes
            data <- prepared_data$data
            
            if (length(genes) < 2) {
                return(NULL)
            }
            
            # Initialize results data frame
            cooccurrence_results <- data.frame()
            
            # Calculate pairwise co-occurrence
            for (i in 1:(length(genes) - 1)) {
                for (j in (i + 1):length(genes)) {
                    gene1 <- genes[i]
                    gene2 <- genes[j]
                    
                    # Count co-occurrences (exclude missing)
                    both_mutated <- sum(data[[gene1]] == 1 & data[[gene2]] == 1, na.rm = TRUE)
                    only_gene1 <- sum(data[[gene1]] == 1 & data[[gene2]] == 0, na.rm = TRUE)
                    only_gene2 <- sum(data[[gene1]] == 0 & data[[gene2]] == 1, na.rm = TRUE)
                    neither <- sum(data[[gene1]] == 0 & data[[gene2]] == 0, na.rm = TRUE)
                    
                    # Check for zero variance or empty cells to avoid errors
                    if ((both_mutated + only_gene1 == 0) || (both_mutated + only_gene2 == 0) ||
                        (only_gene1 + neither == 0) || (only_gene2 + neither == 0)) {
                        association <- "Indeterminate"
                        p_value <- 1.0
                        odds_ratio <- NA_real_
                    } else {
                        # Fisher's exact test for association
                        contingency_table <- matrix(c(both_mutated, only_gene1, only_gene2, neither), nrow = 2)
                        fisher_test <- tryCatch({
                            fisher.test(contingency_table)
                        }, error = function(e) {
                            list(p.value = 1.0, estimate = 1.0)
                        })
                        
                        p_value <- fisher_test$p.value
                        odds_ratio <- as.numeric(fisher_test$estimate)
                        
                        # Determine association type
                        association <- if (p_value < 0.05) {
                            if (odds_ratio > 1) "Co-occurring" else "Mutually exclusive"
                        } else {
                            "Independent"
                        }
                    }
                    
                    # Add to results
                    cooccurrence_results <- rbind(cooccurrence_results, data.frame(
                        gene1 = gene1,
                        gene2 = gene2,
                        cooccurrence_count = both_mutated,
                        exclusivity_count = only_gene1 + only_gene2,
                        association_type = association,
                        odds_ratio = odds_ratio,
                        p_value = p_value
                    ))
                }
            }
            
            if (nrow(cooccurrence_results) > 0) {
                cooccurrence_results$fdr <- p.adjust(cooccurrence_results$p_value, method = "BH")
            }
            
            return(cooccurrence_results)
        },

        .getColorPalette = function(colorScheme, plotType = "oncoplot") {
            # Centralized color palette configuration for all plot types
            # Returns appropriate colors based on color scheme and plot type

            palettes <- list(
                default = list(
                    oncoplot = c("Wild-type" = "#f0f0f0", "Mutation" = "#1f77b4", "Missing" = "#d9d9d9"),
                    bar = "#1f77b4",
                    gradient = list(low = "#3182bd", mid = "#f0f0f0", high = "#d62728"),
                    mutation_type = c(
                        "Wild-type" = "#f7f7f7",
                        "Missense" = "#1f77b4",
                        "Nonsense" = "#d62728",
                        "Frame_Shift" = "#ff7f0e",
                        "Splice_Site" = "#2ca02c",
                        "In_Frame" = "#9467bd",
                        "SNV" = "#17becf",
                        "CNV" = "#8c564b",
                        "Fusion" = "#e377c2",
                        "Mutation" = "#1f77b4",
                        "Missing" = "#d9d9d9"
                    )
                ),
                clinical = list(
                    oncoplot = c("Wild-type" = "#e5f5f9", "Mutation" = "#2ca02c", "Missing" = "#d9d9d9"),
                    bar = "#2ca02c",
                    gradient = list(low = "#2ca02c", mid = "#f0f0f0", high = "#d62728")
                ),
                viridis = list(
                    oncoplot = c("Wild-type" = "#f0f0f0", "Mutation" = "#440154", "Missing" = "#d9d9d9"),
                    bar = "#440154",
                    gradient = list(low = "#440154", mid = "#21908c", high = "#fde725")
                ),
                high_contrast = list(
                    oncoplot = c("Wild-type" = "#ffffff", "Mutation" = "#000000", "Missing" = "#d9d9d9"),
                    bar = "#000000",
                    gradient = list(low = "#000000", mid = "#808080", high = "#ffffff")
                ),
                mutation_type = list(
                    oncoplot = c("Wild-type" = "#f7f7f7", "Mutation" = "#1f77b4", "Missing" = "#d9d9d9"),
                    bar = "#1f77b4",
                    gradient = list(low = "#1f77b4", mid = "#f0f0f0", high = "#ff7f0e"),
                    mutation_type = c(
                        "Wild-type" = "#f7f7f7",
                        "Missense" = "#1f77b4",
                        "Nonsense" = "#d62728",
                        "Frame_Shift" = "#ff7f0e",
                        "Splice_Site" = "#2ca02c",
                        "In_Frame" = "#9467bd",
                        "SNV" = "#17becf",
                        "CNV" = "#8c564b",
                        "Fusion" = "#e377c2",
                        "Mutation" = "#1f77b4",
                        "Missing" = "#d9d9d9"
                    )
                )
            )

            # Return appropriate palette, with fallback to default
            if (colorScheme %in% names(palettes)) {
                if (plotType %in% names(palettes[[colorScheme]])) {
                    return(palettes[[colorScheme]][[plotType]])
                }
            }

            # Fallback to default if scheme or type not found
            return(palettes$default[[plotType]])
        },

        .updateGuidanceMessage = function() {
            sampleVar <- self$options$sampleVar
            geneVars <- self$options$geneVars
            data <- self$data
            notes <- private$.guidanceNotes %||% character()
            
            # Build helpful guidance HTML
            guidance_html <- "<div style='padding: 15px; background-color: #f8f9fa; border-radius: 8px; margin: 10px 0;'>"
            
            if (is.null(sampleVar) || length(sampleVar) == 0) {
                guidance_html <- paste0(guidance_html,
                    "<h4 style='color: #0066cc; margin-top: 0;'>📊 Step 1: Select Sample ID Variable</h4>",
                    "<p style='margin: 8px 0;'>Please choose a variable that contains unique identifiers for each sample (e.g., patient IDs, sample names).</p>",
                    "<p style='margin: 8px 0; font-style: italic;'>👆 Look for variables like: SampleID, PatientID, Sample_Name</p>"
                )
            } else {
                guidance_html <- paste0(guidance_html,
                    "<p style='color: #28a745; margin: 5px 0;'>✅ <strong>Sample ID Variable:</strong> ", sampleVar, " selected</p>"
                )
            }
            
            if (is.null(geneVars) || length(geneVars) == 0) {
                guidance_html <- paste0(guidance_html,
                    "<h4 style='color: #0066cc; margin-top: 15px;'>🧬 Step 2: Select Gene Variables</h4>",
                    "<p style='margin: 8px 0;'>Choose variables representing mutation status. These should use 0/1 coding:</p>",
                    "<ul style='margin: 8px 0; padding-left: 20px;'>",
                    "<li><strong>0</strong> = Wild-type (no mutation)</li>",
                    "<li><strong>1</strong> = Mutated</li>",
                    "</ul>",
                    "<p style='margin: 8px 0; font-style: italic;'>👆 Look for gene names like: TP53, KRAS, PIK3CA, EGFR</p>"
                )
            } else {
                guidance_html <- paste0(guidance_html,
                    "<p style='color: #28a745; margin: 5px 0;'>✅ <strong>Gene Variables:</strong> ", length(geneVars), " genes selected</p>"
                )
            }
            
            # Check data and variables exist
            if (!is.null(data) && nrow(data) > 0 && !is.null(sampleVar) && !is.null(geneVars)) {
                missing_vars <- c()
                if (!sampleVar %in% names(data)) {
                    missing_vars <- c(missing_vars, sampleVar)
                }
                for (var in geneVars) {
                    if (!var %in% names(data)) {
                        missing_vars <- c(missing_vars, var)
                    }
                }
                
                if (length(missing_vars) > 0) {
                    guidance_html <- paste0(guidance_html,
                        "<h4 style='color: #dc3545; margin-top: 15px;'>🔍 Variable Check</h4>",
                        "<p style='margin: 8px 0;'>These variables were not found in your dataset:</p>",
                        "<p style='background-color: #ffe6e6; padding: 8px; border-radius: 4px; margin: 8px 0;'>",
                        paste(missing_vars, collapse=", "), "</p>",
                        "<p style='margin: 8px 0;'><strong>💡 Tip:</strong> Check variable names for typos or case sensitivity</p>"
                    )
                }
                
                if (nrow(data) < 2) {
                    guidance_html <- paste0(guidance_html,
                        "<h4 style='color: #dc3545; margin-top: 15px;'>📈 Data Requirements</h4>",
                        "<p style='margin: 8px 0;'>At least 2 samples are needed for oncoplot visualization.</p>",
                        "<p style='margin: 8px 0;'>Your dataset has ", nrow(data), " row(s).</p>"
                    )
                }
                
                if (length(notes) > 0) {
                    guidance_html <- paste0(guidance_html,
                        "<div style='background-color: #fff3cd; border-left: 4px solid #f39c12; padding: 10px; margin: 10px 0;'>",
                        "<h4 style='color: #e67e22; margin-top: 0;'>⚠️ Data Warnings</h4>",
                        "<ul style='margin: 5px 0;'>",
                        paste0("<li>", notes, "</li>", collapse = ""),
                        "</ul>",
                        "</div>"
                    )
                }
            }
            
            if (is.null(data) || nrow(data) == 0) {
                guidance_html <- paste0(guidance_html,
                    "<h4 style='color: #ffc107; margin-top: 15px;'>📁 Data Status</h4>",
                    "<p style='margin: 8px 0;'>Please load a dataset first to begin analysis.</p>",
                    "<p style='margin: 8px 0;'><strong>💡 Tip:</strong> Use the CSV file from the data/ folder for testing</p>"
                )
            }
            
            # Show ready status
            if (!is.null(sampleVar) && !is.null(geneVars) && length(geneVars) > 0) {
                guidance_html <- paste0(guidance_html,
                    "<div style='background-color: #d4edda; padding: 10px; border-radius: 4px; margin-top: 15px;'>",
                    "<h4 style='color: #155724; margin-top: 0;'>🎉 Ready for Analysis!</h4>",
                    "<p style='margin: 5px 0; color: #155724;'>Minimum requirements met. The oncoplot will appear below.</p>",
                    "</div>"
                )
            }
            
            guidance_html <- paste0(guidance_html, "</div>")
            
            self$results$setupGuidance$setContent(guidance_html)
        },
        
        .updateVisibility = function() {
            opts <- private$.effectiveOptions %||% private$.optionsWithPreset()
            sampleVar <- opts$sampleVar
            geneVars <- opts$geneVars
            clinicalVars <- opts$clinicalVars
            showMutationLoad <- opts$showMutationLoad
            plotType <- opts$plotType
            
            # Main plot and basic tables - visible only when minimum requirements met
            hasMinimumRequirements <- !is.null(sampleVar) && length(sampleVar) > 0 && 
                                     !is.null(geneVars) && length(geneVars) > 0
            
            self$results$main$setVisible(hasMinimumRequirements)
            self$results$mutationSummary$setVisible(hasMinimumRequirements)
            self$results$plotInfo$setVisible(hasMinimumRequirements)
            
            # Sample summary - visible when mutation load option is enabled and minimum requirements met
            self$results$sampleSummary$setVisible(showMutationLoad && hasMinimumRequirements)
            
            # Clinical summary - visible when clinical variables are selected
            self$results$clinicalSummary$setVisible(!is.null(clinicalVars) && length(clinicalVars) > 0)
            
            # Cooccurrence - visible when plot type is cooccurrence and minimum requirements met
            self$results$cooccurrence$setVisible(plotType == "cooccurrence" && hasMinimumRequirements)
        },
        
        # Plot render function (jamovi pattern)
        .plotMain = function(image, ggtheme, theme, ...) {
            # Retrieve state data
            prepared_data <- image$state
            
            if (is.null(prepared_data)) {
                return(FALSE)
            }

            opts <- prepared_data$effectiveOptions
            
            # Get plot options
            plotType <- opts$plotType
            colorScheme <- opts$colorScheme
            showMutationLoad <- opts$showMutationLoad
            showGeneFreq <- opts$showGeneFreq
            showTMB <- opts$showTMB
            log10TransformTMB <- opts$log10TransformTMB
            drawMarginalPlots <- opts$drawMarginalPlots
            showClinicalAnnotation <- opts$showClinicalAnnotation
            fontSize <- opts$fontSize
            showLegend <- opts$showLegend

            # Extract data components
            mutation_matrix <- prepared_data$mutation_matrix
            selected_genes <- prepared_data$selected_genes
            clinical_data <- prepared_data$clinical_data
            safe_sampleVar <- prepared_data$safe_sampleVar

            if (plotType == "oncoplot") {
                # Create classic oncoplot
                p <- mutation_matrix %>%
                    ggplot2::ggplot(ggplot2::aes(x = gene, y = !!rlang::sym(safe_sampleVar), fill = mutation_type)) +
                    ggplot2::geom_tile(color = "white", size = 0.1) +
                    ggplot2::scale_x_discrete(position = "top") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 0, size = fontSize),
                        axis.text.y = ggplot2::element_text(size = fontSize - 2),
                        axis.title = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        legend.position = if(showLegend) "bottom" else "none"
                    ) +
                    ggplot2::labs(
                        title = "Genomic Landscape (Oncoplot)",
                        fill = "Mutation Status"
                    )

                # Apply clinical color schemes (color-blind safe options)
                if (colorScheme == "mutation_type") {
                    # For mutation_type, filter palette to only types present in data
                    unique_types <- unique(mutation_matrix$mutation_type)
                    mutation_colors <- private$.getColorPalette(colorScheme, "mutation_type")
                    available_colors <- mutation_colors[names(mutation_colors) %in% unique_types]
                    p <- p + ggplot2::scale_fill_manual(values = available_colors)
                } else {
                    # Use centralized palette for other schemes
                    p <- p + ggplot2::scale_fill_manual(values = private$.getColorPalette(colorScheme, "oncoplot"))
                }
                
            } else if (plotType == "frequency") {
                # Create gene frequency plot with color scheme support
                freq_color <- private$.getColorPalette(colorScheme, "bar")
                
                p <- prepared_data$gene_frequencies %>%
                    ggplot2::ggplot(ggplot2::aes(x = reorder(gene, frequency), y = frequency)) +
                    ggplot2::geom_col(fill = freq_color, alpha = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text = ggplot2::element_text(size = fontSize),
                        axis.title = ggplot2::element_text(size = fontSize + 1)
                    ) +
                    ggplot2::labs(
                        title = "Gene Mutation Frequencies",
                        x = "Gene",
                        y = "Mutation Frequency"
                    ) +
                    ggplot2::scale_y_continuous(labels = scales::percent_format())

            } else if (plotType == "cooccurrence") {
                # Create co-occurrence heatmap based on Fisher results (log2 OR)
                co_results <- prepared_data$cooccurrence_results
                genes <- prepared_data$selected_genes
                
                if (is.null(co_results) || nrow(co_results) == 0 || length(genes) < 2) {
                    p <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Not enough genes for co-occurrence")
                } else {
                    # Build symmetric matrix of log2 odds ratios
                    log_or <- matrix(0, nrow = length(genes), ncol = length(genes), dimnames = list(genes, genes))
                    for (k in seq_len(nrow(co_results))) {
                        g1 <- co_results$gene1[k]
                        g2 <- co_results$gene2[k]
                        lor <- ifelse(is.na(co_results$odds_ratio[k]) || co_results$odds_ratio[k] <= 0, 0, log2(co_results$odds_ratio[k]))
                        log_or[g1, g2] <- lor
                        log_or[g2, g1] <- lor
                    }
                    cooccurrence_df <- expand.grid(
                        Gene1 = factor(rownames(log_or), levels = rownames(log_or)),
                        Gene2 = factor(colnames(log_or), levels = colnames(log_or)),
                        stringsAsFactors = FALSE
                    )
                    cooccurrence_df$Log2OR <- as.numeric(as.vector(log_or))

                    color_scheme_gradient <- private$.getColorPalette(colorScheme, "gradient")

                    p <- cooccurrence_df %>%
                        ggplot2::ggplot(ggplot2::aes(x = Gene1, y = Gene2, fill = Log2OR)) +
                        ggplot2::geom_tile(color = "white", size = 0.1) +
                        ggplot2::scale_fill_gradient2(
                            low = color_scheme_gradient$low, 
                            mid = color_scheme_gradient$mid, 
                            high = color_scheme_gradient$high, 
                            midpoint = 0,
                            limits = c(-3, 3),
                            na.value = "grey90",
                            name = "log2(OR)"
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = fontSize),
                            axis.text.y = ggplot2::element_text(size = fontSize),
                            axis.title = ggplot2::element_blank(),
                            panel.grid = ggplot2::element_blank(),
                            legend.position = if(showLegend) "bottom" else "none"
                        ) +
                        ggplot2::labs(
                            title = "Gene Co-occurrence Analysis",
                            fill = "log2(OR)"
                        )
                }
            } else {
                # Default to frequency plot with color scheme support
                freq_color <- private$.getColorPalette(colorScheme, "bar")
                
                p <- prepared_data$gene_frequencies %>%
                    ggplot2::ggplot(ggplot2::aes(x = reorder(gene, frequency), y = frequency)) +
                    ggplot2::geom_col(fill = freq_color, alpha = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        title = "Gene Mutation Frequencies",
                        x = "Gene",
                        y = "Mutation Frequency"
                    )
            }
            
            # Create marginal plots if requested (ggoncoplot style)
            if (drawMarginalPlots && plotType == "oncoplot") {
                # Get color for marginal plots
                marginal_color <- private$.getColorPalette(colorScheme, "bar")
                
                # Create gene frequency marginal plot (right side)
                gene_plot <- prepared_data$gene_frequencies %>%
                    ggplot2::ggplot(ggplot2::aes(x = frequency, y = reorder(gene, frequency))) +
                    ggplot2::geom_col(fill = marginal_color, alpha = 0.7) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text.y = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_text(size = fontSize - 2),
                        axis.title.x = ggplot2::element_text(size = fontSize - 1),
                        panel.grid = ggplot2::element_blank(),
                        plot.margin = ggplot2::margin(0, 5, 0, 0)
                    ) +
                    ggplot2::labs(x = "Frequency") +
                    ggplot2::scale_x_continuous(labels = scales::percent_format())
                
                # Create TMB plot if enabled (top)
                if (showTMB) {
                    # Calculate TMB per sample
                    sample_tmb <- prepared_data$data %>%
                        dplyr::mutate(
                            total_mutations = rowSums(dplyr::select(., dplyr::all_of(selected_genes)), na.rm = TRUE)
                        ) %>%
                        dplyr::select(!!rlang::sym(safe_sampleVar), total_mutations)
                    
                    # Apply log transformation if requested
                    if (log10TransformTMB) {
                        sample_tmb$total_mutations <- log10(sample_tmb$total_mutations + 1)  # +1 to handle zeros
                        y_label <- "Log10(TMB + 1)"
                    } else {
                        y_label <- "Total Mutations"
                    }
                    
                    # Get TMB color (use a complementary color or same scheme)
                    tmb_color <- switch(colorScheme,
                        "default" = "#ff7f0e",
                        "clinical" = "#27ae60", 
                        "viridis" = "#21908c",
                        "high_contrast" = "#333333",
                        "mutation_type" = "#ff7f0e",
                        "#ff7f0e"  # fallback
                    )
                    
                    tmb_plot <- sample_tmb %>%
                        ggplot2::ggplot(ggplot2::aes(x = reorder(!!rlang::sym(safe_sampleVar), total_mutations), y = total_mutations)) +
                        ggplot2::geom_col(fill = tmb_color, alpha = 0.7) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            axis.text.x = ggplot2::element_blank(),
                            axis.title.x = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_text(size = fontSize - 2),
                            axis.title.y = ggplot2::element_text(size = fontSize - 1),
                            panel.grid = ggplot2::element_blank(),
                            plot.margin = ggplot2::margin(0, 0, 5, 0)
                        ) +
                        ggplot2::labs(y = y_label)
                    
                    # Combine plots with patchwork (ggoncoplot style layout)
                    p <- tmb_plot / (p | gene_plot) + 
                        patchwork::plot_layout(heights = c(1, 4), widths = c(4, 1))
                } else {
                    # Just combine main plot with gene frequency
                    p <- p | gene_plot + patchwork::plot_layout(widths = c(4, 1))
                }
            }
            
            # Add clinical annotations if requested and available
            if (plotType == "oncoplot" && showClinicalAnnotation && !is.null(clinical_data) && ncol(clinical_data) > 1) {
                # Reshape clinical data for plotting
                clinical_long <- clinical_data %>%
                    tidyr::pivot_longer(
                        cols = -!!rlang::sym(safe_sampleVar),
                        names_to = "variable",
                        values_to = "value"
                    )

                # Create clinical heatmap
                clinical_plot <- clinical_long %>%
                    ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(safe_sampleVar), y = variable, fill = value)) +
                    ggplot2::geom_tile(color = "white", size = 0.1) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_blank(),
                        axis.title.x = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_text(size = fontSize - 2),
                        axis.title.y = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        legend.position = "right",
                        plot.margin = ggplot2::margin(5, 0, 0, 0)
                    ) +
                    ggplot2::labs(fill = "Clinical")
                
                # Combine with main plot using patchwork
                # Adjust layout to put clinical plot at the bottom
                p <- p / clinical_plot + patchwork::plot_layout(heights = c(4, 1))
            }
            
            # Apply jamovi theme
            p <- p + ggtheme
            
            # Print plot (required for jamovi)
            print(p)
            return(TRUE)
        },

        .validateData = function() {
            sampleVar <- self$options$sampleVar
            geneVars <- self$options$geneVars
            data <- self$data
            private$.guidanceNotes <- character()

            # Update guidance message instead of throwing errors
            private$.updateGuidanceMessage()

            # ERROR: Missing sample variable
            if (is.null(sampleVar) || length(sampleVar) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingSampleVar',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Sample ID variable is required. Please select a variable containing unique patient or sample identifiers (e.g., PatientID, SampleID, TCGA-ID).')
                self$results$insert(999, notice)
                return(FALSE)
            }

            # ERROR: Missing gene variables
            if (is.null(geneVars) || length(geneVars) < 1) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingGeneVars',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('At least one gene variable is required. Please select variables representing mutation status (0 = wild-type, 1 = mutated). Common examples: TP53, KRAS, PIK3CA, EGFR, BRAF.')
                self$results$insert(999, notice)
                return(FALSE)
            }

            # ERROR: No data loaded
            if (is.null(data) || nrow(data) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noData',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('No data loaded. Please open a dataset containing genomic mutation information to begin analysis.')
                self$results$insert(999, notice)
                return(FALSE)
            }

            # Check if variables exist in data
            missing_vars <- c()
            if (!sampleVar %in% names(data)) {
                missing_vars <- c(missing_vars, sampleVar)
            }
            for (var in geneVars) {
                if (!var %in% names(data)) {
                    missing_vars <- c(missing_vars, var)
                }
            }

            # ERROR: Variables not found in dataset
            if (length(missing_vars) > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'variablesNotFound',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('The following variables were not found in your dataset: %s. Please check variable names for typos, case sensitivity, or ensure your dataset contains these columns.', paste(missing_vars, collapse = ', ')))
                self$results$insert(999, notice)
                return(FALSE)
            }

            # ERROR: Insufficient data
            if (nrow(data) < 2) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'insufficientData',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('At least 2 samples are required for oncoplot visualization. Your dataset contains %d sample(s). Please load a dataset with multiple samples.', nrow(data)))
                self$results$insert(999, notice)
                return(FALSE)
            }

            # STRONG_WARNING: Duplicate sample IDs
            if (anyDuplicated(data[[sampleVar]]) > 0) {
                dup_count <- sum(duplicated(data[[sampleVar]]))
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'duplicateSamples',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf('%d duplicate sample IDs detected in the %s variable. Only the first occurrence of each sample will be retained for analysis. Consider deduplicating your data before analysis to ensure data integrity.', dup_count, sampleVar))
                self$results$insert(999, notice)
                private$.guidanceNotes <- c(private$.guidanceNotes, sprintf("Duplicate sample IDs (n=%d) will be removed", dup_count))
            }

            # STRONG_WARNING: Non-binary mutation values
            nonbinary <- vapply(geneVars, function(v) {
                any(!is.na(data[[v]]) & !(data[[v]] %in% c(0, 1)), na.rm = TRUE)
            }, logical(1))
            if (any(nonbinary)) {
                bad_genes <- paste(geneVars[nonbinary], collapse = ", ")
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'nonBinaryMutations',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf('Non-binary mutation values detected in the following genes: %s. Expected values are 0 (wild-type) or 1 (mutated). All non-zero values will be clipped to 1, and zero/negative values to 0 for analysis.', bad_genes))
                self$results$insert(999, notice)
                private$.guidanceNotes <- c(private$.guidanceNotes, sprintf("Non-binary values in %d genes", sum(nonbinary)))
            }

            # STRONG_WARNING: Small sample size
            if (nrow(data) < 10) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallSampleSize',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf('Small sample size detected (n = %d). For robust mutation frequency analysis and statistical testing, a minimum of 10-20 samples is recommended. Findings should be interpreted with caution and validated in larger cohorts.', nrow(data)))
                self$results$insert(999, notice)
            }

            return(TRUE)
        },

        .prepareData = function(effectiveOptions) {
            data <- self$data
            sampleVar <- effectiveOptions$sampleVar
            geneVars <- effectiveOptions$geneVars
            clinicalVars <- effectiveOptions$clinicalVars
            mutationTypeVar <- effectiveOptions$mutationTypeVar
            topn <- effectiveOptions$topn
            maxSamples <- effectiveOptions$maxSamples
            genesToInclude <- effectiveOptions$genesToInclude
            genesToIgnore <- effectiveOptions$genesToIgnore

            # Create safe variable names for use in formulas and rlang (handles spaces, special chars)
            safe_sampleVar <- jmvcore::composeTerm(sampleVar)

            # Convert to data frame and handle missing values
            df <- as.data.frame(data)

            # Convert sample variable to character
            df[[sampleVar]] <- as.character(df[[sampleVar]])
            # Drop duplicate samples keeping first
            df <- df[!duplicated(df[[sampleVar]]), , drop = FALSE]

            # Process gene variables - convert to numeric (0/1) - VECTORIZED
            if (length(geneVars) > 0) {
                gene_matrix <- as.matrix(df[, geneVars, drop = FALSE])
                gene_matrix <- apply(gene_matrix, 2, function(x) {
                    x_num <- as.numeric(as.character(x))
                    ifelse(x_num > 0, 1, ifelse(x_num <= 0, 0, x_num))
                })
                df[, geneVars] <- as.data.frame(gene_matrix)
            }

            # Apply gene filtering logic (ggoncoplot style)
            available_genes <- geneVars
            
            # Parse genes to ignore
            if (!is.null(genesToIgnore) && nchar(genesToIgnore) > 0) {
                genes_to_ignore <- trimws(strsplit(genesToIgnore, ",")[[1]])
                available_genes <- setdiff(available_genes, genes_to_ignore)
            }
            
            # Handle specific genes to include (overrides topn)
            if (!is.null(genesToInclude) && nchar(genesToInclude) > 0) {
                genes_to_include <- trimws(strsplit(genesToInclude, ",")[[1]])
                # Filter to genes that exist in data
                selected_genes <- intersect(genes_to_include, available_genes)

                # ERROR: No requested genes found in dataset
                if (length(selected_genes) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'noGenesFound',
                        type = jmvcore::NoticeType$ERROR
                    )
                    missing_genes <- setdiff(genes_to_include, available_genes)
                    available_sample <- paste(head(available_genes, 10), collapse = ", ")
                    if (length(available_genes) > 10) {
                        available_sample <- paste0(available_sample, ", ...")
                    }
                    notice$setContent(sprintf(
                        'None of the requested genes (%s) were found in your dataset. Available genes include: %s. Please check gene names for typos (they are case-sensitive) or select genes from the available list.',
                        paste(missing_genes, collapse = ', '),
                        available_sample
                    ))
                    self$results$insert(999, notice)
                    return(NULL)
                }

                # WARNING: Partial gene match
                if (length(selected_genes) < length(genes_to_include)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'partialGeneMatch',
                        type = jmvcore::NoticeType$WARNING
                    )
                    missing_genes <- setdiff(genes_to_include, selected_genes)
                    notice$setContent(sprintf(
                        'Some requested genes were not found in your dataset. Found: %s. Missing: %s. Gene names are case-sensitive - please verify spelling and capitalization.',
                        paste(selected_genes, collapse = ', '),
                        paste(missing_genes, collapse = ', ')
                    ))
                    self$results$insert(999, notice)
                }
            } else {
                # Calculate mutation frequencies and select top N genes
                gene_frequencies_calc <- df %>%
                    dplyr::select(dplyr::all_of(available_genes)) %>%
                    dplyr::summarise(
                        dplyr::across(everything(), ~sum(.x == 1, na.rm = TRUE)),
                        n_non_missing = dplyr::across(everything(), ~sum(!is.na(.x)))
                    )
                counts <- gene_frequencies_calc[1, seq_along(available_genes)]
                n_non_missing <- vapply(available_genes, function(g) sum(!is.na(df[[g]])), numeric(1))
                gene_frequencies_calc <- data.frame(
                    gene = available_genes,
                    frequency = ifelse(n_non_missing > 0, as.numeric(counts) / n_non_missing, NA_real_)
                ) %>%
                    dplyr::arrange(dplyr::desc(frequency)) %>%
                    dplyr::slice_head(n = topn)
                
                selected_genes <- gene_frequencies_calc$gene
            }
            
            # Calculate final gene frequencies for selected genes (respect missing)
            gene_frequencies <- df %>%
                dplyr::select(dplyr::all_of(selected_genes)) %>%
                dplyr::summarise(
                    dplyr::across(everything(), ~sum(.x == 1, na.rm = TRUE)),
                    n_non_missing = dplyr::across(everything(), ~sum(!is.na(.x)))
                )
            # gene_frequencies now has two sets of columns; pivot properly
            mutated_counts <- gene_frequencies[1, seq_along(selected_genes)]
            non_missing_counts <- vapply(selected_genes, function(g) {
                sum(!is.na(df[[g]]))
            }, numeric(1))
            gene_frequencies <- data.frame(
                gene = selected_genes,
                mutated_samples = as.numeric(mutated_counts),
                total_samples = non_missing_counts,
                frequency = ifelse(non_missing_counts > 0, as.numeric(mutated_counts) / non_missing_counts, NA_real_)
            ) %>%
                dplyr::arrange(desc(frequency))

            # Calculate sample mutation burden and select samples
            df$mutation_count <- rowSums(df[selected_genes] == 1, na.rm = TRUE)

            # Implement all sorting options including hierarchical (ggoncoplot style)
            if (effectiveOptions$sortBy == "hierarchical") {
                # ggoncoplot hierarchical sorting using base-2 scoring
                df$hierarchical_score <- private$.calculateHierarchicalScore(df, selected_genes, gene_frequencies)
                df <- df %>% dplyr::arrange(desc(hierarchical_score))
            } else if (effectiveOptions$sortBy == "mutation_count") {
                df <- df %>% dplyr::arrange(desc(mutation_count))
            } else if (effectiveOptions$sortBy == "sample_id") {
                df <- df %>% dplyr::arrange(!!rlang::sym(sampleVar))
            } else if (effectiveOptions$sortBy == "clinical" && !is.null(clinicalVars) && length(clinicalVars) > 0) {
                # Sort by first clinical variable
                df <- df %>% dplyr::arrange(!!rlang::sym(clinicalVars[1]))
            }

            # Limit number of samples
            if (nrow(df) > maxSamples) {
                df <- df %>% dplyr::slice_head(n = maxSamples)
            }

            # Prepare clinical data if provided
            clinical_data <- NULL
            if (!is.null(clinicalVars) && length(clinicalVars) > 0) {
                clinical_data <- df %>%
                    dplyr::select(dplyr::all_of(c(sampleVar, clinicalVars)))
            }

            # Prepare mutation matrix with mutation type support
            mutation_matrix <- df %>%
                dplyr::select(dplyr::all_of(c(sampleVar, selected_genes))) %>%
                tidyr::pivot_longer(-!!rlang::sym(sampleVar), names_to = "gene", values_to = "mutation")
                
            # Add mutation type information if available
            if (!is.null(mutationTypeVar) && mutationTypeVar %in% names(df)) {
                # Join with mutation type data
                mutation_type_data <- df %>%
                    dplyr::select(!!rlang::sym(sampleVar), !!rlang::sym(mutationTypeVar))
                
                mutation_matrix <- mutation_matrix %>%
                    dplyr::left_join(mutation_type_data, by = sampleVar) %>%
                    dplyr::mutate(
                        mutation_type = dplyr::case_when(
                            is.na(mutation) ~ "Missing",
                            mutation == 1 ~ as.character(!!rlang::sym(mutationTypeVar)),
                            TRUE ~ "Wild-type"
                        )
                    )
            } else {
                mutation_matrix <- mutation_matrix %>%
                    dplyr::mutate(
                        mutation_type = dplyr::case_when(
                            is.na(mutation) ~ "Missing",
                            mutation == 1 ~ "Mutation",
                            TRUE ~ "Wild-type"
                        )
                    )
            }

            return(list(
                data = df,
                mutation_matrix = mutation_matrix,
                selected_genes = selected_genes,
                gene_frequencies = gene_frequencies,
                clinical_data = clinical_data,
                sampleVar = sampleVar,
                safe_sampleVar = safe_sampleVar,
                effectiveOptions = effectiveOptions
            ))
        },


        .init = function() {
            # Set up clinical context and instructions
            instructions_html <- "
            <div style='background-color: #f8f9fa; border-radius: 8px; padding: 15px; margin: 10px 0;'>
            <h3 style='color: #2c3e50; margin-top: 0;'>🧬 Genomic Landscape Visualization for Clinical Research</h3>
            
            <div style='background-color: #e8f4fd; border-left: 4px solid #3498db; padding: 10px; margin: 10px 0;'>
            <h4 style='color: #2980b9; margin-top: 0;'>📋 Clinical Applications</h4>
            <p><strong>Oncoplots</strong> help pathologists and oncologists visualize mutation landscapes across cancer samples to:</p>
            <ul style='margin-bottom: 5px;'>
            <li><strong>Identify Driver Mutations:</strong> Frequently mutated genes (e.g., TP53, KRAS) driving cancer progression</li>
            <li><strong>Discover Therapeutic Targets:</strong> Actionable mutations for precision medicine and targeted therapy</li>
            <li><strong>Analyze Mutation Patterns:</strong> Co-occurring vs mutually exclusive alterations indicating pathway dependencies</li>
            <li><strong>Assess Tumor Mutation Burden (TMB):</strong> High TMB may predict immunotherapy response</li>
            <li><strong>Stratify Patients:</strong> Group patients by molecular profiles for clinical trials</li>
            </ul>
            </div>

            <div style='background-color: #fff3cd; border-left: 4px solid #f39c12; padding: 10px; margin: 10px 0;'>
            <h4 style='color: #e67e22; margin-top: 0;'>🔬 When to Use This Analysis</h4>
            <ul style='margin-bottom: 5px;'>
            <li><strong>Tumor Profiling:</strong> Characterizing mutation landscapes in cancer cohorts</li>
            <li><strong>Biomarker Discovery:</strong> Identifying prognostic or predictive molecular markers</li>
            <li><strong>Clinical Trial Design:</strong> Patient stratification based on genomic profiles</li>
            <li><strong>Pathway Analysis:</strong> Understanding oncogenic pathway alterations</li>
            <li><strong>Therapeutic Planning:</strong> Matching patients to targeted therapies</li>
            </ul>
            </div>

            <div style='background-color: #d4edda; border-left: 4px solid #28a745; padding: 10px; margin: 10px 0;'>
            <h4 style='color: #27ae60; margin-top: 0;'>📊 Data Requirements</h4>
            <ul style='margin-bottom: 5px;'>
            <li><strong>Sample ID:</strong> Patient/tumor identifiers (e.g., TCGA-AA-3818, P001)</li>
            <li><strong>Gene Variables:</strong> Binary mutation status (0 = wild-type, 1 = mutated)</li>
            <li><strong>Clinical Data:</strong> Age, stage, grade, treatment response (optional)</li>
            <li><strong>Mutation Types:</strong> SNV, CNV, Indel classifications (optional)</li>
            </ul>
            </div>

            <div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0;'>
            <h4 style='color: #c0392b; margin-top: 0;'>⚠️ Important Considerations</h4>
            <ul style='margin-bottom: 5px;'>
            <li><strong>Sample Size:</strong> Minimum 10 samples recommended for meaningful patterns</li>
            <li><strong>Data Quality:</strong> Ensure consistent mutation calling and annotation</li>
            <li><strong>Clinical Context:</strong> Consider tumor type, stage, and treatment history</li>
            <li><strong>Statistical Power:</strong> Larger cohorts needed for rare mutation analysis</li>
            </ul>
            </div>
            </div>
            "

            self$results$instructions$setContent(instructions_html)
            
            # Initialize visibility of result elements
            private$.updateVisibility()
        },

        .run = function() {
            effectiveOptions <- private$.optionsWithPreset()

            # Always update guidance message and visibility first
            private$.updateGuidanceMessage()
            private$.updateVisibility()
            
            # Early exit if minimum requirements not met (no error messages)
            sampleVar <- self$options$sampleVar
            geneVars <- self$options$geneVars
            
            if (is.null(sampleVar) || length(sampleVar) == 0 || 
                is.null(geneVars) || length(geneVars) == 0) {
                return()
            }
            
            if (!private$.validateData()) {
                return()
            }

            # Prepare data
            prepared_data <- private$.prepareData(effectiveOptions)
            
            if (is.null(prepared_data)) {
                return()
            }

            # Prepare plot data and set state for rendering
            plotWidth <- effectiveOptions$plotWidth
            plotHeight <- effectiveOptions$plotHeight
            self$results$main$setSize(plotWidth, plotHeight)
            
            # If cooccurrence plot, precompute associations and attach for plotting/table
            if (effectiveOptions$plotType == "cooccurrence") {
                prepared_data$cooccurrence_results <- private$.calculateCooccurrence(prepared_data)
            }
            
            # Set plot data state (jamovi pattern: setState with data, not plot object)
            image <- self$results$main
            image$setState(prepared_data)

            # Update mutation summary table
            mutation_summary <- prepared_data$gene_frequencies %>%
                dplyr::mutate(
                    mutation_type = "SNV"
                ) %>%
                dplyr::select(gene, mutated_samples, total_samples, mutation_frequency = frequency, mutation_type)

            # Set mutation summary data safely
            for (i in seq_len(nrow(mutation_summary))) {
                self$results$mutationSummary$addRow(rowKey = i, values = mutation_summary[i, ])
            }

            # Update sample summary if enabled with corrected logic
            if (effectiveOptions$showMutationLoad) {
                sample_summary <- prepared_data$data %>%
                    dplyr::rowwise() %>%
                    dplyr::mutate(
                        genes_mutated = sum(c_across(prepared_data$selected_genes) > 0, na.rm = TRUE)
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::select(sample = !!rlang::sym(prepared_data$safe_sampleVar), mutation_count, genes_mutated) %>%
                    dplyr::mutate(
                        total_mutations = mutation_count,
                        mutation_burden = genes_mutated / length(prepared_data$selected_genes)
                    ) %>%
                    dplyr::select(sample, total_mutations, genes_mutated, mutation_burden)

                # Set sample summary data safely
                for (i in seq_len(nrow(sample_summary))) {
                    self$results$sampleSummary$addRow(rowKey = i, values = sample_summary[i, ])
                }
            }

            # Optional per-sample mutation burden table (aligned with TMB panel and mutation load)
            if (effectiveOptions$showTMB && effectiveOptions$showMutationLoad && isTRUE(effectiveOptions$showTMBTable)) {
                sample_tmb <- prepared_data$data %>%
                    dplyr::mutate(
                        total_mutations = rowSums(dplyr::select(., dplyr::all_of(prepared_data$selected_genes)), na.rm = TRUE)
                    ) %>%
                    dplyr::select(sample = !!rlang::sym(prepared_data$safe_sampleVar), total_mutations)

                sample_tmb <- sample_tmb %>%
                    dplyr::mutate(
                        tmb_log10 = if (isTRUE(effectiveOptions$log10TransformTMB)) log10(total_mutations + 1) else NA_real_
                    )

                for (i in seq_len(nrow(sample_tmb))) {
                    self$results$tmbTable$addRow(rowKey = i, values = sample_tmb[i, ])
                }
            }

            # Update clinical summary with actual analysis
            if (!is.null(effectiveOptions$clinicalVars) && length(effectiveOptions$clinicalVars) > 0) {
                clinical_vars <- effectiveOptions$clinicalVars
                clinical_summary_list <- list()
                
                for (var in clinical_vars) {
                    var_data <- prepared_data$data[[var]]
                    categories <- length(unique(na.omit(var_data)))
                    missing <- sum(is.na(var_data))
                    complete <- sum(!is.na(var_data))
                    
                    clinical_summary_list[[var]] <- data.frame(
                        variable = var,
                        categories = paste0(categories, " categories"),
                        missing = missing,
                        complete = complete
                    )
                }
                
                clinical_summary <- do.call(rbind, clinical_summary_list)
                
                # Set clinical summary data safely
                for (i in seq_len(nrow(clinical_summary))) {
                    self$results$clinicalSummary$addRow(rowKey = i, values = clinical_summary[i, ])
                }
            }

            # Populate co-occurrence table for co-occurrence plot type
            if (effectiveOptions$plotType == "cooccurrence") {
                cooccurrence_results <- private$.calculateCooccurrence(prepared_data)
                if (!is.null(cooccurrence_results)) {
                    # Set cooccurrence data safely
                    for (i in seq_len(nrow(cooccurrence_results))) {
                        self$results$cooccurrence$addRow(rowKey = i, values = cooccurrence_results[i, ])
                    }
                }
            }

            # Update plot info
            plot_info <- data.frame(
                parameter = c("Total Samples", "Total Genes", "Plot Type", "Color Scheme", "Width", "Height"),
                value = c(
                    nrow(prepared_data$data),
                    length(prepared_data$selected_genes),
                    effectiveOptions$plotType,
                    effectiveOptions$colorScheme,
                    plotWidth,
                    plotHeight
                )
            )
            # Set plot info data safely
            for (i in seq_len(nrow(plot_info))) {
                self$results$plotInfo$addRow(rowKey = i, values = plot_info[i, ])
            }

            # INFO: Hierarchical sorting explanation
            if (effectiveOptions$sortBy == "hierarchical") {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'hierarchicalSorting',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent('Hierarchical sorting algorithm applied: Samples are ordered using ggoncoplot\'s base-2 exponential weighting based on gene frequency rank. Most frequently mutated genes contribute exponentially more weight (2^rank) to sample ordering, creating intuitive visual patterns.')
                self$results$insert(999, notice)
            }

            # INFO: Gene selection method
            if (!is.null(self$options$genesToInclude) && nchar(self$options$genesToInclude) > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'geneSelectionMethod',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(sprintf('Gene selection: Specific genes requested (%s). Displaying %d genes that were found in your dataset. Top N setting is overridden when specific genes are requested.', self$options$genesToInclude, length(prepared_data$selected_genes)))
                self$results$insert(999, notice)
            } else {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'geneSelectionMethod',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(sprintf('Gene selection: Displaying top %d most frequently mutated genes from your dataset (%d total gene variables). Use "Specific Genes to Include" to focus on genes of clinical interest.', effectiveOptions$topn, length(self$options$geneVars)))
                self$results$insert(999, notice)
            }

            # INFO: TMB transformation
            if (effectiveOptions$showTMB && effectiveOptions$log10TransformTMB) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'tmbTransformation',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent('Tumor Mutation Burden (TMB) values displayed using log10(TMB + 1) transformation. This transformation improves visualization when datasets contain hypermutator samples with extremely high mutation counts. Disable "Log10 Transform TMB" to view raw counts.')
                self$results$insert(999, notice)
            }

            # WARNING: High missing rate in clinical variables
            if (!is.null(effectiveOptions$clinicalVars) && length(effectiveOptions$clinicalVars) > 0) {
                for (var in effectiveOptions$clinicalVars) {
                    var_data <- prepared_data$data[[var]]
                    missing_pct <- sum(is.na(var_data)) / length(var_data) * 100

                    if (missing_pct > 50) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = paste0('highMissing_', var),
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(sprintf('Clinical variable "%s" has high missing data rate (%.1f%% missing). Consider imputation or removal of this variable for more reliable clinical correlations.', var, missing_pct))
                        self$results$insert(500, notice)
                    }
                }
            }

            # Generate clinical interpretation
            private$.generateClinicalInterpretation(prepared_data, effectiveOptions)
        },
        
        .generateClinicalInterpretation = function(prepared_data, effectiveOptions) {
            # Generate automated clinical summary based on mutation patterns
            mutation_summary <- prepared_data$data %>%
                dplyr::select(dplyr::all_of(prepared_data$selected_genes)) %>%
                dplyr::summarise(dplyr::across(everything(), ~sum(.x > 0, na.rm = TRUE))) %>%
                tidyr::pivot_longer(everything(), names_to = "gene", values_to = "mutated_samples") %>%
                dplyr::mutate(
                    frequency = mutated_samples / nrow(prepared_data$data),
                    frequency_pct = round(frequency * 100, 1)
                ) %>%
                dplyr::arrange(dplyr::desc(frequency))
            
            total_samples <- nrow(prepared_data$data)
            top_genes <- head(mutation_summary, 5)
            
            # Generate clinical interpretation text
            clinical_text <- ""
            
            # Overall summary
            clinical_text <- paste0(clinical_text, 
                "<div style='background-color: #f8f9fa; border-radius: 8px; padding: 15px; margin: 15px 0;'>\n",
                "<h3 style='color: #2c3e50; margin-top: 0;'>🧬 Clinical Interpretation</h3>\n")
            
            # Most frequently mutated genes
            if (nrow(top_genes) > 0) {
                clinical_text <- paste0(clinical_text,
                    "<div style='background-color: #e8f4fd; border-left: 4px solid #3498db; padding: 10px; margin: 10px 0;'>\n",
                    "<h4 style='color: #2980b9; margin-top: 0;'>📊 Most Frequently Mutated Genes</h4>\n",
                    "<ul style='margin-bottom: 5px;'>\n")
                
                for (i in 1:min(3, nrow(top_genes))) {
                    gene <- top_genes$gene[i]
                    freq_pct <- top_genes$frequency_pct[i]
                    count <- top_genes$mutated_samples[i]
                    
                    clinical_significance <- private$.getGeneSignificance(gene)
                    
                    clinical_text <- paste0(clinical_text,
                        "<li><strong>", gene, "</strong>: ", count, "/", total_samples, " samples (", freq_pct, 
                        "%) - ", clinical_significance, "</li>\n")
                }
                clinical_text <- paste0(clinical_text, "</ul>\n</div>\n")
            }
            
            # TMB analysis if available
            if (effectiveOptions$showTMB) {
                tmb_data <- prepared_data$data %>%
                    dplyr::mutate(
                        total_mutations = rowSums(dplyr::select(., dplyr::all_of(prepared_data$selected_genes)) == 1, na.rm = TRUE)
                    )
                
                avg_tmb <- mean(tmb_data$total_mutations, na.rm = TRUE)
                high_tmb_samples <- sum(tmb_data$total_mutations >= 10, na.rm = TRUE)  # heuristic threshold
                
                clinical_text <- paste0(clinical_text,
                    "<div style='background-color: #e8f5e8; border-left: 4px solid #27ae60; padding: 10px; margin: 10px 0;'>\n",
                    "<h4 style='color: #27ae60; margin-top: 0;'>🧬 Mutation Burden Analysis</h4>\n",
                    "<p><strong>Average mutated genes per sample:</strong> ", round(avg_tmb, 2), "</p>\n",
                    "<p><strong>Samples with ≥10 mutated genes:</strong> ", high_tmb_samples, "/", total_samples, 
                    " (", round((high_tmb_samples/total_samples)*100, 1), "%)</p>\n",
                    "<p style='font-style: italic; color: #666;'>💡 Counts are based on the selected genes (panel size dependent).</p>\n",
                    "</div>\n")
            }
            
            # Clinical recommendations based on mutation patterns
            clinical_text <- paste0(clinical_text,
                "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin: 10px 0;'>\n",
                "<h4 style='color: #856404; margin-top: 0;'>🎯 Clinical Recommendations</h4>\n")
            
            # Check for actionable mutations
            actionable_genes <- c("EGFR", "KRAS", "BRAF", "ALK", "ROS1", "NTRK1", "NTRK2", "NTRK3", "PIK3CA", "ERBB2", "MET")
            present_actionable <- intersect(top_genes$gene[top_genes$frequency > 0], actionable_genes)
            
            if (length(present_actionable) > 0) {
                clinical_text <- paste0(clinical_text,
                    "<p><strong>Actionable mutations detected:</strong> ", paste(present_actionable, collapse = ", "), "</p>\n",
                    "<p>• Consider targeted therapy options and clinical trial eligibility</p>\n",
                    "<p>• Review FDA-approved drugs and companion diagnostics</p>\n")
            }
            
            # Co-occurrence patterns
            if (nrow(top_genes) >= 2) {
                clinical_text <- paste0(clinical_text,
                    "<p><strong>Mutation co-occurrence analysis recommended:</strong></p>\n",
                    "<p>• Use co-occurrence plot to identify mutually exclusive or concurrent mutations</p>\n",
                    "<p>• Consider pathway analysis for therapeutic strategy</p>\n")
            }
            
            clinical_text <- paste0(clinical_text, "</div>\n")
            
            # Sample stratification insights
            if (total_samples >= 10) {
                clinical_text <- paste0(clinical_text,
                    "<div style='background-color: #f3e5f5; border-left: 4px solid #9c27b0; padding: 10px; margin: 10px 0;'>\n",
                    "<h4 style='color: #7b1fa2; margin-top: 0;'>👥 Sample Stratification Insights</h4>\n",
                    "<p><strong>Sample size:</strong> ", total_samples, " samples analyzed</p>\n")
                
                if (total_samples >= 50) {
                    clinical_text <- paste0(clinical_text,
                        "<p>✅ <strong>Adequate power</strong> for mutation frequency analysis and biomarker discovery</p>\n")
                } else if (total_samples >= 20) {
                    clinical_text <- paste0(clinical_text,
                        "<p>⚠️ <strong>Moderate power</strong> - consider pooling with additional cohorts for robust statistics</p>\n")
                } else {
                    clinical_text <- paste0(clinical_text,
                        "<p>⚠️ <strong>Limited power</strong> - findings should be validated in larger cohorts</p>\n")
                }
                
                clinical_text <- paste0(clinical_text, "</div>\n")
            }
            
            clinical_text <- paste0(clinical_text, "</div>")
            
            # Update clinical interpretation in results
            # Keep Instructions static; put interpretation into clinicalSummaryText
            private$.generateClinicalSummaryText(prepared_data, mutation_summary, clinical_text, effectiveOptions)
        },
        
        .getGeneSignificance = function(gene) {
            # Return clinical significance for common cancer genes
            significance_map <- list(
                "TP53" = "Guardian of genome, tumor suppressor frequently mutated in cancer",
                "KRAS" = "Oncogene driving cell proliferation, therapeutic target in multiple cancers",
                "PIK3CA" = "PI3K pathway activation, targetable with PI3K inhibitors",
                "EGFR" = "Growth factor receptor, FDA-approved targeted therapies available",
                "BRAF" = "MAPK pathway driver, targeted therapy available (vemurafenib, dabrafenib)",
                "ALK" = "Fusion oncogene, targetable with ALK inhibitors (crizotinib, alectinib)",
                "ROS1" = "Fusion oncogene, crizotinib and other ROS1 inhibitors available",
                "NTRK1" = "Neurotrophic receptor kinase, larotrectinib/entrectinib approved",
                "NTRK2" = "Neurotrophic receptor kinase, larotrectinib/entrectinib approved",
                "NTRK3" = "Neurotrophic receptor kinase, larotrectinib/entrectinib approved",
                "ERBB2" = "HER2 amplification/mutation, targeted therapy available",
                "MET" = "Hepatocyte growth factor receptor, MET inhibitors in development",
                "BRCA1" = "DNA repair gene, PARP inhibitor sensitivity",
                "BRCA2" = "DNA repair gene, PARP inhibitor sensitivity",
                "PTEN" = "Tumor suppressor, PI3K pathway regulation",
                "APC" = "Tumor suppressor, Wnt pathway regulation",
                "VHL" = "Tumor suppressor, angiogenesis regulation",
                "RB1" = "Cell cycle regulation, retinoblastoma protein",
                "CDKN2A" = "Cell cycle inhibitor, p16 tumor suppressor"
            )
            
            return(significance_map[[gene]] %||% "Potential cancer-associated gene, clinical significance under investigation")
        },
        
        .generateClinicalSummaryText = function(prepared_data, mutation_summary, clinical_text, effectiveOptions) {
            # Generate concise, copy-ready clinical summary for reports/publications
            total_samples <- nrow(prepared_data$data)
            top_genes <- head(mutation_summary, 3)
            
            summary_text <- paste0(
                "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; margin: 15px 0;'>\n",
                "<h3 style='color: #2c3e50; margin-top: 0; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>",
                "📄 Clinical Summary for Reports</h3>\n",
                "<div style='background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #28a745;'>\n",
                "<p style='margin-bottom: 15px; font-weight: bold; color: #28a745;'>Copy-Ready Summary Text:</p>\n",
                "<div style='font-family: Georgia, serif; line-height: 1.6; padding: 10px; background-color: #f8f9fa; border-radius: 3px;'>\n"
            )
            
            # Generate publication-ready text
            if (nrow(top_genes) > 0) {
                summary_text <- paste0(summary_text, 
                    "<p><strong>Genomic Analysis Summary:</strong> Analysis of ", total_samples, 
                    " samples revealed mutation frequencies in key cancer-associated genes. ")
                
                # Top 3 genes summary
                gene_summaries <- c()
                for (i in 1:min(3, nrow(top_genes))) {
                    gene <- top_genes$gene[i]
                    freq_pct <- top_genes$frequency_pct[i]
                    count <- top_genes$mutated_samples[i]
                    
                    gene_summaries <- c(gene_summaries, 
                        paste0(gene, " was mutated in ", count, " samples (", freq_pct, "%)"))
                }
                
                summary_text <- paste0(summary_text, 
                    paste(gene_summaries, collapse = ", "), ". ")
            }
            
            # TMB summary if available
            if (effectiveOptions$showTMB) {
                tmb_data <- prepared_data$data %>%
                    dplyr::mutate(
                        total_mutations = rowSums(dplyr::select(., dplyr::all_of(prepared_data$selected_genes)) == 1, na.rm = TRUE)
                    )
                avg_tmb <- round(mean(tmb_data$total_mutations, na.rm = TRUE), 1)
                high_tmb_samples <- sum(tmb_data$total_mutations >= 10, na.rm = TRUE)
                high_tmb_pct <- round((high_tmb_samples/total_samples)*100, 1)
                
                summary_text <- paste0(summary_text,
                    "The average number of mutated genes per sample was ", avg_tmb, ", with ", 
                    high_tmb_samples, " samples (", high_tmb_pct, "%) showing ≥10 mutated genes (panel-dependent). ")
            }
            
            # Clinical significance
            actionable_genes <- c("EGFR", "KRAS", "BRAF", "ALK", "ROS1", "NTRK1", "NTRK2", "NTRK3", "PIK3CA", "ERBB2", "MET")
            present_actionable <- intersect(top_genes$gene[top_genes$frequency > 0], actionable_genes)
            
            if (length(present_actionable) > 0) {
                summary_text <- paste0(summary_text,
                    "Notably, actionable mutations were identified in ", paste(present_actionable, collapse = ", "), 
                    ", which may inform targeted therapy selection. ")
            }
            
            summary_text <- paste0(summary_text,
                "These findings contribute to understanding the genomic landscape and may inform precision medicine approaches.</p>\n")
            
            # Add methodology note
            summary_text <- paste0(summary_text,
                "</div>\n",
                "<div style='margin-top: 15px; padding: 10px; background-color: #e9ecef; border-radius: 3px;'>\n",
                "<p style='margin: 0; font-size: 0.9em; color: #6c757d;'><strong>Methods Note:</strong> ",
                "Genomic landscape visualization was performed using oncoplot analysis with ", 
                ifelse(effectiveOptions$plotType == "oncoplot", "mutation matrix visualization", 
                ifelse(effectiveOptions$plotType == "frequency", "frequency-based analysis", "co-occurrence analysis")), 
                ". Analysis included ", length(prepared_data$selected_genes), " genes across ", 
                total_samples, " samples.</p>\n",
                "</div>\n",
                "</div>\n",
                "</div>"
            )
            
            # Combine interpretation + summary for the clinicalSummaryText pane
            self$results$clinicalSummaryText$setContent(paste0(clinical_text, summary_text))
        }
    )
)
