#' @title Genomic Landscape Visualization
#' @description Creates oncoplots (mutation landscapes) to visualize genomic alterations across genes and samples with optional clinical annotations
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import ggoncoplot
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
#' ggoncoplot(data, "SampleID", c("TP53", "KRAS"))

#' @export ggoncoplotClass

ggoncoplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ggoncoplotClass",
    inherit = ggoncoplotBase,
    private = list(
        
        .validateData = function() {
            sampleVar <- self$options$sampleVar
            geneVars <- self$options$geneVars
            
            if (is.null(sampleVar)) {
                jmvcore::reject("Sample ID variable is required", code="a")
                return(FALSE)
            }
            
            if (length(geneVars) < 1) {
                jmvcore::reject("At least one gene variable is required", code="b")
                return(FALSE)
            }
            
            # Check if variables exist in data
            data <- self$data
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
                jmvcore::reject(paste("Missing variables:", paste(missing_vars, collapse=", ")), code="c")
                return(FALSE)
            }
            
            return(TRUE)
        },
        
        .prepareData = function() {
            data <- self$data
            sampleVar <- self$options$sampleVar
            geneVars <- self$options$geneVars
            clinicalVars <- self$options$clinicalVars
            maxGenes <- self$options$maxGenes
            maxSamples <- self$options$maxSamples
            
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
            
            # Calculate mutation frequencies and select top genes
            gene_frequencies <- df %>%
                dplyr::select(all_of(geneVars)) %>%
                dplyr::summarise_all(mean, na.rm = TRUE) %>%
                tidyr::pivot_longer(everything(), names_to = "gene", values_to = "frequency") %>%
                dplyr::arrange(desc(frequency)) %>%
                dplyr::slice_head(n = maxGenes)
            
            selected_genes <- gene_frequencies$gene
            
            # Calculate sample mutation burden and select samples
            df$mutation_count <- rowSums(df[selected_genes], na.rm = TRUE)
            
            if (self$options$sortBy == "mutation_count") {
                df <- df %>% dplyr::arrange(desc(mutation_count))
            } else if (self$options$sortBy == "sample_id") {
                df <- df %>% dplyr::arrange(!!sym(sampleVar))
            }
            
            # Limit number of samples
            if (nrow(df) > maxSamples) {
                df <- df %>% dplyr::slice_head(n = maxSamples)
            }
            
            # Prepare clinical data if provided
            clinical_data <- NULL
            if (!is.null(clinicalVars) && length(clinicalVars) > 0) {
                clinical_data <- df %>%
                    dplyr::select(all_of(c(sampleVar, clinicalVars)))
            }
            
            # Prepare mutation matrix for ggoncoplot
            mutation_matrix <- df %>%
                dplyr::select(all_of(c(sampleVar, selected_genes))) %>%
                tidyr::pivot_longer(-!!sym(sampleVar), names_to = "gene", values_to = "mutation") %>%
                dplyr::mutate(
                    mutation_type = ifelse(mutation == 1, "Mutation", "Wild-type")
                )
            
            return(list(
                data = df,
                mutation_matrix = mutation_matrix,
                selected_genes = selected_genes,
                gene_frequencies = gene_frequencies,
                clinical_data = clinical_data
            ))
        },
        
        .createPlot = function(prepared_data) {
            plotType <- self$options$plotType
            colorScheme <- self$options$colorScheme
            showMutationLoad <- self$options$showMutationLoad
            showGeneFreq <- self$options$showGeneFreq
            showClinicalAnnotation <- self$options$showClinicalAnnotation
            fontSize <- self$options$fontSize
            showLegend <- self$options$showLegend
            
            mutation_matrix <- prepared_data$mutation_matrix
            selected_genes <- prepared_data$selected_genes
            clinical_data <- prepared_data$clinical_data
            
            if (plotType == "oncoplot") {
                # Create classic oncoplot
                p <- mutation_matrix %>%
                    ggplot2::ggplot(ggplot2::aes(x = gene, y = !!sym(self$options$sampleVar), fill = mutation_type)) +
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
                
                # Apply color scheme
                if (colorScheme == "default") {
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#f0f0f0", "Mutation" = "#d62728"))
                } else if (colorScheme == "clinical") {
                    p <- p + ggplot2::scale_fill_manual(values = c("Wild-type" = "#2166ac", "Mutation" = "#762a83"))
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
            if (private$.validateData()) {
                # Set up instructions
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
                "
                
                self$results$instructions$setContent(instructions_html)
            }
        },
        
        .run = function() {
            if (!private$.validateData()) {
                return()
            }
            
            # Prepare data
            prepared_data <- private$.prepareData()
            
            # Create and display plot
            plot <- private$.createPlot(prepared_data)
            self$results$main$setState(plot)
            
            # Update mutation summary table
            mutation_summary <- prepared_data$gene_frequencies %>%
                dplyr::mutate(
                    mutated_samples = round(frequency * nrow(prepared_data$data)),
                    total_samples = nrow(prepared_data$data),
                    mutation_type = "SNV"
                ) %>%
                dplyr::select(gene, mutated_samples, total_samples, mutation_frequency = frequency, mutation_type)
            
            self$results$mutationSummary$setData(mutation_summary)
            
            # Update sample summary if enabled
            if (self$options$showMutationLoad) {
                sample_summary <- prepared_data$data %>%
                    dplyr::select(sample = !!sym(self$options$sampleVar), mutation_count) %>%
                    dplyr::mutate(
                        total_mutations = mutation_count,
                        genes_mutated = mutation_count,
                        mutation_burden = mutation_count / length(prepared_data$selected_genes)
                    ) %>%
                    dplyr::select(sample, total_mutations, genes_mutated, mutation_burden)
                
                self$results$sampleSummary$setData(sample_summary)
            }
            
            # Update clinical summary if clinical variables provided
            if (!is.null(self$options$clinicalVars) && length(self$options$clinicalVars) > 0) {
                clinical_summary <- data.frame(
                    variable = self$options$clinicalVars,
                    categories = "Various",
                    missing = 0,
                    complete = nrow(prepared_data$data)
                )
                self$results$clinicalSummary$setData(clinical_summary)
            }
            
            # Update plot info
            plot_info <- data.frame(
                parameter = c("Total Samples", "Total Genes", "Plot Type", "Color Scheme"),
                value = c(
                    nrow(prepared_data$data),
                    length(prepared_data$selected_genes),
                    self$options$plotType,
                    self$options$colorScheme
                )
            )
            self$results$plotInfo$setData(plot_info)
        }
    )
)