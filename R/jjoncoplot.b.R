#' @title Genomic Landscape Visualization
#' @description Creates oncoplots (mutation landscapes) to visualize genomic alterations across genes and samples with optional clinical annotations
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
# @import ggoncoplot
#' @import tidyr
#' @import scales
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

                # Apply color schemes
                if (colorScheme == "default") {
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#f0f0f0", "Mutation" = "#d62728"))
                } else if (colorScheme == "clinical") {
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#e5f5f9", "Mutation" = "#2ca02c"))
                } else if (colorScheme == "mutation_type") {
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#f7f7f7", "Mutation" = "#d95f02"))
                }
                
            } else if (plotType == "frequency") {
                # Create gene frequency plot
                p <- prepared_data$gene_frequencies %>%
                    ggplot2::ggplot(ggplot2::aes(x = reorder(gene, frequency), y = frequency)) +
                    ggplot2::geom_col(fill = "#1f77b4", alpha = 0.7) +
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

                cooccurrence_matrix <- matrix(0, nrow = length(genes), ncol = length(genes))
                rownames(cooccurrence_matrix) <- genes
                colnames(cooccurrence_matrix) <- genes

                for (i in 1:length(genes)) {
                    for (j in 1:length(genes)) {
                        if (i != j) {
                            cooccurrence_matrix[i, j] <- cor(data[[genes[i]]], data[[genes[j]]], use = "complete.obs")
                        }
                    }
                }

                cooccurrence_df <- as.data.frame(as.table(cooccurrence_matrix))
                names(cooccurrence_df) <- c("Gene1", "Gene2", "Correlation")

                p <- cooccurrence_df %>%
                    ggplot2::ggplot(ggplot2::aes(x = Gene1, y = Gene2, fill = Correlation)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                    ggplot2::labs(title = "Gene Co-occurrence Matrix")

            } else {
                # Default to frequency plot
                p <- prepared_data$gene_frequencies %>%
                    ggplot2::ggplot(ggplot2::aes(x = reorder(gene, frequency), y = frequency)) +
                    ggplot2::geom_col(fill = "#1f77b4", alpha = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        title = "Gene Mutation Frequencies",
                        x = "Gene",
                        y = "Mutation Frequency"
                    )
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

        .createPlot = function(prepared_data) {
            plotType <- self$options$plotType %||% "oncoplot"
            colorScheme <- self$options$colorScheme %||% "default"
            showMutationLoad <- self$options$showMutationLoad %||% TRUE
            showGeneFreq <- self$options$showGeneFreq %||% TRUE
            showClinicalAnnotation <- self$options$showClinicalAnnotation %||% FALSE
            fontSize <- self$options$fontSize %||% 10
            showLegend <- self$options$showLegend %||% TRUE

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

                # Apply all color schemes
                if (colorScheme == "default") {
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#f0f0f0", "Mutation" = "#d62728"))
                } else if (colorScheme == "clinical") {
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#e5f5f9", "Mutation" = "#2ca02c"))
                } else if (colorScheme == "mutation_type") {
                    # Use different colors for different mutation types if available
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#f7f7f7", "Mutation" = "#d95f02"))
                } else if (colorScheme == "custom") {
                    # Use custom colors if provided
                    customColors <- self$options$customColors
                    if (!is.null(customColors) && nchar(customColors) > 0) {
                        colors <- strsplit(customColors, ",")[[1]]
                        if (length(colors) >= 2) {
                            p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = colors[1], "Mutation" = colors[2]))
                        }
                    }
                }
                
                # Add clinical annotations if enabled and data available
                if (showClinicalAnnotation && !is.null(clinical_data) && ncol(clinical_data) > 1) {
                    # This would require more complex implementation with cowplot or patchwork
                    # For now, add a note that clinical annotations are available
                    p <- p + ggplot2::labs(subtitle = "Clinical data available - see Clinical Summary table")
                }

            } else if (plotType == "frequency") {
                # Create gene frequency plot
                p <- prepared_data$gene_frequencies %>%
                    ggplot2::ggplot(ggplot2::aes(x = reorder(gene, frequency), y = frequency)) +
                    ggplot2::geom_col(fill = "#1f77b4", alpha = 0.7) +
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

                cooccurrence_matrix <- matrix(0, nrow = length(genes), ncol = length(genes))
                rownames(cooccurrence_matrix) <- genes
                colnames(cooccurrence_matrix) <- genes

                for (i in 1:length(genes)) {
                    for (j in 1:length(genes)) {
                        if (i != j) {
                            cooccurrence_matrix[i, j] <- cor(data[[genes[i]]], data[[genes[j]]], use = "complete.obs")
                        }
                    }
                }

                cooccurrence_df <- as.data.frame(as.table(cooccurrence_matrix))
                names(cooccurrence_df) <- c("Gene1", "Gene2", "Correlation")

                p <- cooccurrence_df %>%
                    ggplot2::ggplot(ggplot2::aes(x = Gene1, y = Gene2, fill = Correlation)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = fontSize),
                        axis.text.y = ggplot2::element_text(size = fontSize),
                        axis.title = ggplot2::element_blank()
                    ) +
                    ggplot2::labs(
                        title = "Gene Co-occurrence Analysis",
                        fill = "Correlation"
                    )
            }

            return(p)
        },

        .init = function() {
            # Set up instructions without validation (validation happens in .run)
            instructions_html <- "
            <h3>Genomic Landscape Visualization</h3>
            <p>This analysis creates oncoplots to visualize mutation patterns across genes and samples.</p>
            <h4>Data Requirements:</h4>
            <ul>
            <li><strong>Sample ID Variable:</strong> Unique identifier for each sample</li>
            <li><strong>Gene Variables:</strong> Binary variables (0/1) indicating mutation status</li>
            <li><strong>Clinical Variables:</strong> Optional variables for clinical annotations</li>
            </ul>
            <h4>Plot Types:</h4>
            <ul>
            <li><strong>Classic Oncoplot:</strong> Matrix showing mutations across genes and samples</li>
            <li><strong>Gene Frequency Plot:</strong> Bar chart of mutation frequencies by gene</li>
            <li><strong>Co-occurrence Plot:</strong> Heatmap showing gene mutation correlations</li>
            </ul>
            <h4>Minimum Requirements:</h4>
            <ul>
            <li>✅ <strong>Sample ID Variable:</strong> Select a variable containing unique sample identifiers</li>
            <li>✅ <strong>At least one Gene Variable:</strong> Select binary mutation variables (0 = wild-type, 1 = mutated)</li>
            </ul>
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
        }
    )
)
