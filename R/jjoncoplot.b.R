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
                    
                    # Count co-occurrences
                    both_mutated <- sum(data[[gene1]] == 1 & data[[gene2]] == 1, na.rm = TRUE)
                    only_gene1 <- sum(data[[gene1]] == 1 & data[[gene2]] == 0, na.rm = TRUE)
                    only_gene2 <- sum(data[[gene1]] == 0 & data[[gene2]] == 1, na.rm = TRUE)
                    neither <- sum(data[[gene1]] == 0 & data[[gene2]] == 0, na.rm = TRUE)
                    
                    # Fisher's exact test for association
                    contingency_table <- matrix(c(both_mutated, only_gene1, only_gene2, neither), nrow = 2)
                    fisher_test <- fisher.test(contingency_table)
                    
                    # Determine association type
                    association <- if (fisher_test$p.value < 0.05) {
                        if (fisher_test$estimate > 1) "Co-occurring" else "Mutually exclusive"
                    } else {
                        "Independent"
                    }
                    
                    # Add to results
                    cooccurrence_results <- rbind(cooccurrence_results, data.frame(
                        gene1 = gene1,
                        gene2 = gene2,
                        cooccurrence_count = both_mutated,
                        exclusivity_count = only_gene1 + only_gene2,
                        association_type = association,
                        p_value = fisher_test$p.value
                    ))
                }
            }
            
            return(cooccurrence_results)
        },

        .updateGuidanceMessage = function() {
            sampleVar <- self$options$sampleVar
            geneVars <- self$options$geneVars
            data <- self$data
            
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
            sampleVar <- self$options$sampleVar
            geneVars <- self$options$geneVars
            clinicalVars <- self$options$clinicalVars
            showMutationLoad <- self$options$showMutationLoad
            plotType <- self$options$plotType
            
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
            
            # Get plot options
            plotType <- self$options$plotType %||% "oncoplot"
            colorScheme <- self$options$colorScheme %||% "default"
            showMutationLoad <- self$options$showMutationLoad %||% TRUE
            showGeneFreq <- self$options$showGeneFreq %||% TRUE
            showTMB <- self$options$showTMB %||% FALSE
            log10TransformTMB <- self$options$log10TransformTMB %||% TRUE
            drawMarginalPlots <- self$options$drawMarginalPlots %||% FALSE
            showClinicalAnnotation <- self$options$showClinicalAnnotation %||% FALSE
            fontSize <- self$options$fontSize %||% 10
            showLegend <- self$options$showLegend %||% TRUE

            # Extract data components
            mutation_matrix <- prepared_data$mutation_matrix
            selected_genes <- prepared_data$selected_genes
            clinical_data <- prepared_data$clinical_data

            if (plotType == "oncoplot") {
                # Create classic oncoplot
                p <- mutation_matrix %>%
                    ggplot2::ggplot(ggplot2::aes(x = gene, y = !!rlang::sym(self$options$sampleVar), fill = mutation_type)) +
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
                if (colorScheme == "default") {
                    # Color-blind safe default (blue-orange)
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#f0f0f0", "Mutation" = "#1f77b4"))
                } else if (colorScheme == "clinical") {
                    # Clinical blue-green scale
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#e5f5f9", "Mutation" = "#2ca02c"))
                } else if (colorScheme == "viridis") {
                    # Viridis color-blind safe palette
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#f0f0f0", "Mutation" = "#440154"))
                } else if (colorScheme == "high_contrast") {
                    # High contrast for accessibility
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#ffffff", "Mutation" = "#000000"))
                } else if (colorScheme == "mutation_type") {
                    # Create dynamic color palette for mutation types (color-blind considerations)
                    unique_types <- unique(mutation_matrix$mutation_type)
                    mutation_colors <- c(
                        "Wild-type" = "#f7f7f7",
                        "Missense" = "#1f77b4",      # Blue (color-blind safe)
                        "Nonsense" = "#d62728",      # Red
                        "Frame_Shift" = "#ff7f0e",   # Orange (color-blind safe)
                        "Splice_Site" = "#2ca02c",   # Green
                        "In_Frame" = "#9467bd",      # Purple
                        "SNV" = "#17becf",           # Cyan
                        "CNV" = "#8c564b",           # Brown
                        "Fusion" = "#e377c2",        # Pink
                        "Mutation" = "#1f77b4"       # Blue fallback
                    )
                    # Filter colors to only those present in data
                    available_colors <- mutation_colors[names(mutation_colors) %in% unique_types]
                    p <- p + ggplot2::scale_fill_manual(values = available_colors)
                }
                
            } else if (plotType == "frequency") {
                # Create gene frequency plot with color scheme support
                freq_color <- switch(colorScheme,
                    "default" = "#1f77b4",
                    "clinical" = "#2ca02c", 
                    "viridis" = "#440154",
                    "high_contrast" = "#000000",
                    "mutation_type" = "#1f77b4",
                    "#1f77b4"  # fallback
                )
                
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
                # Create co-occurrence heatmap
                genes <- prepared_data$selected_genes
                data <- prepared_data$data

                # Ensure we have numeric data for correlation
                gene_data <- data[genes]
                gene_data <- lapply(gene_data, function(x) as.numeric(as.character(x)))
                gene_data <- as.data.frame(gene_data)
                names(gene_data) <- genes
                
                # Calculate correlation matrix (handle edge cases)
                if (ncol(gene_data) < 2) {
                    # If only one gene, create a simple single-cell matrix
                    cooccurrence_matrix <- matrix(1, nrow = 1, ncol = 1)
                    rownames(cooccurrence_matrix) <- genes[1]
                    colnames(cooccurrence_matrix) <- genes[1]
                } else {
                    # Calculate correlation matrix
                    cooccurrence_matrix <- stats::cor(gene_data, use = "pairwise.complete.obs", method = "pearson")
                    
                    # Handle NAs and ensure proper data types
                    cooccurrence_matrix[is.na(cooccurrence_matrix)] <- 0
                    diag(cooccurrence_matrix) <- 1  # Set diagonal to 1 for clarity
                }
                
                # Convert to long format for ggplot
                cooccurrence_df <- expand.grid(
                    Gene1 = factor(rownames(cooccurrence_matrix), levels = rownames(cooccurrence_matrix)),
                    Gene2 = factor(colnames(cooccurrence_matrix), levels = colnames(cooccurrence_matrix)),
                    stringsAsFactors = FALSE
                )
                cooccurrence_df$Correlation <- as.numeric(as.vector(cooccurrence_matrix))
                
                # Ensure Correlation is properly numeric and bounded
                cooccurrence_df$Correlation[is.na(cooccurrence_df$Correlation)] <- 0
                cooccurrence_df$Correlation[cooccurrence_df$Correlation > 1] <- 1
                cooccurrence_df$Correlation[cooccurrence_df$Correlation < -1] <- -1
                
                # Apply color scheme to co-occurrence plot
                color_scheme_gradient <- switch(colorScheme,
                    "default" = list(low = "#3182bd", mid = "#f0f0f0", high = "#d62728"),
                    "clinical" = list(low = "#2ca02c", mid = "#f0f0f0", high = "#d62728"), 
                    "viridis" = list(low = "#440154", mid = "#21908c", high = "#fde725"),
                    "high_contrast" = list(low = "#000000", mid = "#808080", high = "#ffffff"),
                    "mutation_type" = list(low = "#1f77b4", mid = "#f0f0f0", high = "#ff7f0e"),
                    list(low = "#3182bd", mid = "#f0f0f0", high = "#d62728")  # fallback
                )

                p <- cooccurrence_df %>%
                    ggplot2::ggplot(ggplot2::aes(x = Gene1, y = Gene2, fill = Correlation)) +
                    ggplot2::geom_tile(color = "white", size = 0.1) +
                    ggplot2::scale_fill_gradient2(
                        low = color_scheme_gradient$low, 
                        mid = color_scheme_gradient$mid, 
                        high = color_scheme_gradient$high, 
                        midpoint = 0,
                        limits = c(-1, 1),
                        na.value = "grey90",
                        name = "Correlation"
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
                        fill = "Correlation"
                    )

            } else {
                # Default to frequency plot with color scheme support
                freq_color <- switch(colorScheme,
                    "default" = "#1f77b4",
                    "clinical" = "#2ca02c", 
                    "viridis" = "#440154",
                    "high_contrast" = "#000000",
                    "mutation_type" = "#1f77b4",
                    "#1f77b4"  # fallback
                )
                
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
                marginal_color <- switch(colorScheme,
                    "default" = "#1f77b4",
                    "clinical" = "#2ca02c", 
                    "viridis" = "#440154",
                    "high_contrast" = "#000000",
                    "mutation_type" = "#1f77b4",
                    "#1f77b4"  # fallback
                )
                
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
                        dplyr::select(!!rlang::sym(self$options$sampleVar), total_mutations)
                    
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
                        ggplot2::ggplot(ggplot2::aes(x = reorder(!!rlang::sym(self$options$sampleVar), total_mutations), y = total_mutations)) +
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

            # Update guidance message instead of throwing errors
            private$.updateGuidanceMessage()

            # Silent validation - return FALSE without error messages
            if (is.null(sampleVar) || length(sampleVar) == 0) {
                return(FALSE)
            }

            if (is.null(geneVars) || length(geneVars) < 1) {
                return(FALSE)
            }

            if (is.null(data) || nrow(data) == 0) {
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
            if (length(missing_vars) > 0) {
                return(FALSE)
            }

            # Check for minimum data requirements
            if (nrow(data) < 2) {
                return(FALSE)
            }

            return(TRUE)
        },

        .prepareData = function() {
            data <- self$data
            sampleVar <- self$options$sampleVar
            geneVars <- self$options$geneVars
            clinicalVars <- self$options$clinicalVars
            mutationTypeVar <- self$options$mutationTypeVar
            topn <- self$options$topn %||% 10
            maxSamples <- self$options$maxSamples %||% 50
            genesToInclude <- self$options$genesToInclude
            genesToIgnore <- self$options$genesToIgnore

            # Convert to data frame and handle missing values
            df <- as.data.frame(data)

            # Convert sample variable to character
            df[[sampleVar]] <- as.character(df[[sampleVar]])

            # Process gene variables - convert to numeric (0/1)
            for (var in geneVars) {
                df[[var]] <- as.numeric(as.character(df[[var]]))
                df[[var]][is.na(df[[var]])] <- 0  # Treat missing as wild-type
                df[[var]] <- ifelse(df[[var]] > 0, 1, 0)  # Convert to 0/1
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
                if (length(selected_genes) == 0) {
                    # Silent handling - will be shown in guidance message
                    return(NULL)
                }
            } else {
                # Calculate mutation frequencies and select top N genes
                gene_frequencies_calc <- df %>%
                    dplyr::select(dplyr::all_of(available_genes)) %>%
                    dplyr::summarise_all(mean, na.rm = TRUE) %>%
                    tidyr::pivot_longer(everything(), names_to = "gene", values_to = "frequency") %>%
                    dplyr::arrange(desc(frequency)) %>%
                    dplyr::slice_head(n = topn)
                
                selected_genes <- gene_frequencies_calc$gene
            }
            
            # Calculate final gene frequencies for selected genes
            gene_frequencies <- df %>%
                dplyr::select(dplyr::all_of(selected_genes)) %>%
                dplyr::summarise_all(mean, na.rm = TRUE) %>%
                tidyr::pivot_longer(everything(), names_to = "gene", values_to = "frequency") %>%
                dplyr::arrange(desc(frequency))

            # Calculate sample mutation burden and select samples
            df$mutation_count <- rowSums(df[selected_genes], na.rm = TRUE)

            # Implement all sorting options including hierarchical (ggoncoplot style)
            if (self$options$sortBy == "hierarchical") {
                # ggoncoplot hierarchical sorting using base-2 scoring
                df$hierarchical_score <- private$.calculateHierarchicalScore(df, selected_genes, gene_frequencies)
                df <- df %>% dplyr::arrange(desc(hierarchical_score))
            } else if (self$options$sortBy == "mutation_count") {
                df <- df %>% dplyr::arrange(desc(mutation_count))
            } else if (self$options$sortBy == "sample_id") {
                df <- df %>% dplyr::arrange(!!rlang::sym(sampleVar))
            } else if (self$options$sortBy == "clinical" && !is.null(clinicalVars) && length(clinicalVars) > 0) {
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
                        mutation_type = ifelse(mutation == 1, 
                                             as.character(!!rlang::sym(mutationTypeVar)), 
                                             "Wild-type")
                    )
            } else {
                mutation_matrix <- mutation_matrix %>%
                    dplyr::mutate(
                        mutation_type = ifelse(mutation == 1, "Mutation", "Wild-type")
                    )
            }

            return(list(
                data = df,
                mutation_matrix = mutation_matrix,
                selected_genes = selected_genes,
                gene_frequencies = gene_frequencies,
                clinical_data = clinical_data,
                sampleVar = sampleVar
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
            prepared_data <- private$.prepareData()
            
            if (is.null(prepared_data)) {
                return()
            }

            # Prepare plot data and set state for rendering
            plotWidth <- self$options$plotWidth %||% 800
            plotHeight <- self$options$plotHeight %||% 600
            self$results$main$setSize(plotWidth, plotHeight)
            
            # Set plot data state (jamovi pattern: setState with data, not plot object)
            image <- self$results$main
            image$setState(prepared_data)

            # Update mutation summary table
            mutation_summary <- prepared_data$gene_frequencies %>%
                dplyr::mutate(
                    mutated_samples = round(frequency * nrow(prepared_data$data)),
                    total_samples = nrow(prepared_data$data),
                    mutation_type = "SNV"
                ) %>%
                dplyr::select(gene, mutated_samples, total_samples, mutation_frequency = frequency, mutation_type)

            # Set mutation summary data safely
            for (i in seq_len(nrow(mutation_summary))) {
                self$results$mutationSummary$addRow(rowKey = i, values = mutation_summary[i, ])
            }

            # Update sample summary if enabled with corrected logic
            if (self$options$showMutationLoad) {
                sample_summary <- prepared_data$data %>%
                    dplyr::rowwise() %>%
                    dplyr::mutate(
                        genes_mutated = sum(c_across(prepared_data$selected_genes) > 0, na.rm = TRUE)
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::select(sample = !!rlang::sym(self$options$sampleVar), mutation_count, genes_mutated) %>%
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

            # Update clinical summary with actual analysis
            if (!is.null(self$options$clinicalVars) && length(self$options$clinicalVars) > 0) {
                clinical_vars <- self$options$clinicalVars
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
            if (self$options$plotType == "cooccurrence") {
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
                    self$options$plotType,
                    self$options$colorScheme,
                    plotWidth,
                    plotHeight
                )
            )
            # Set plot info data safely
            for (i in seq_len(nrow(plot_info))) {
                self$results$plotInfo$addRow(rowKey = i, values = plot_info[i, ])
            }
            
            # Generate clinical interpretation
            private$.generateClinicalInterpretation(prepared_data)
        },
        
        .generateClinicalInterpretation = function(prepared_data) {
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
            if (self$options$showTMB) {
                tmb_data <- prepared_data$data %>%
                    dplyr::mutate(
                        total_mutations = rowSums(dplyr::select(., dplyr::all_of(prepared_data$selected_genes)), na.rm = TRUE)
                    )
                
                avg_tmb <- mean(tmb_data$total_mutations, na.rm = TRUE)
                high_tmb_samples <- sum(tmb_data$total_mutations >= 10, na.rm = TRUE)  # 10+ mutations threshold
                
                clinical_text <- paste0(clinical_text,
                    "<div style='background-color: #e8f5e8; border-left: 4px solid #27ae60; padding: 10px; margin: 10px 0;'>\n",
                    "<h4 style='color: #27ae60; margin-top: 0;'>🧬 Tumor Mutation Burden (TMB) Analysis</h4>\n",
                    "<p><strong>Average TMB:</strong> ", round(avg_tmb, 2), " mutations per sample</p>\n",
                    "<p><strong>High TMB samples (≥10 mutations):</strong> ", high_tmb_samples, "/", total_samples, 
                    " (", round((high_tmb_samples/total_samples)*100, 1), "%)</p>\n",
                    "<p style='font-style: italic; color: #666;'>💡 High TMB (>10-20 mutations) may predict immunotherapy response in many cancer types.</p>\n",
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
            self$results$instructions$setContent(clinical_text)
            
            # Generate copy-ready clinical summary
            private$.generateClinicalSummaryText(prepared_data, mutation_summary)
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
        
        .generateClinicalSummaryText = function(prepared_data, mutation_summary) {
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
            if (self$options$showTMB) {
                tmb_data <- prepared_data$data %>%
                    dplyr::mutate(
                        total_mutations = rowSums(dplyr::select(., dplyr::all_of(prepared_data$selected_genes)), na.rm = TRUE)
                    )
                avg_tmb <- round(mean(tmb_data$total_mutations, na.rm = TRUE), 1)
                high_tmb_samples <- sum(tmb_data$total_mutations >= 10, na.rm = TRUE)
                high_tmb_pct <- round((high_tmb_samples/total_samples)*100, 1)
                
                summary_text <- paste0(summary_text,
                    "The average tumor mutation burden was ", avg_tmb, " mutations per sample, with ", 
                    high_tmb_samples, " samples (", high_tmb_pct, "%) showing high mutation burden (≥10 mutations). ")
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
                ifelse(self$options$plotType == "oncoplot", "mutation matrix visualization", 
                ifelse(self$options$plotType == "frequency", "frequency-based analysis", "co-occurrence analysis")), 
                ". Analysis included ", length(prepared_data$selected_genes), " genes across ", 
                total_samples, " samples.</p>\n",
                "</div>\n",
                "</div>\n",
                "</div>"
            )
            
            # Set the clinical summary text in results
            self$results$clinicalSummaryText$setContent(summary_text)
        }
    )
)
