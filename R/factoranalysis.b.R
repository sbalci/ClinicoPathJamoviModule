#' Factor Analysis Class
#' 
#' Comprehensive factor analysis implementation inspired by BlueSky's BSkyFactorAnalysis
#' with enhanced features for clinical and pathological research.
#' 
#' @import jmvcore
#' @import stats
#' @import psych
#' @import GPArotation
#' @importFrom grDevices rainbow
#' @importFrom graphics abline legend lines plot points text
#' @export

factoranalysisClass <- R6::R6Class(
    "factoranalysisClass",
    inherit = factoranalysisBase,
    private = list(
        .faResult = NULL,
        .data = NULL,
        .vars = NULL,
        .eigenvalues = NULL,
        .parallelResults = NULL,
        
        .init = function() {
            private$.data <- self$data
            private$.vars <- self$options$vars
            
            # Initialize instructions
            html <- self$results$instructions
            html$setContent(private$.getInstructions())
            
            # Setup dynamic columns for loading tables
            if (length(private$.vars) > 0) {
                private$.setupLoadingTables()
            }
        },
        
        .run = function() {
            # Check if we have data and variables
            if (is.null(private$.vars) || length(private$.vars) < 2) {
                self$results$instructions$setContent(
                    "<p>Please select at least 2 variables for factor analysis.</p>"
                )
                return()
            }
            
            # Prepare data
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            # Run sampling adequacy tests if requested
            if (self$options$adequacyTests) {
                private$.runAdequacyTests(data)
            }
            
            # Determine number of factors
            nFactors <- private$.determineFactors(data)
            if (nFactors == 0) {
                self$results$instructions$setContent(
                    "<p>No factors meet the extraction criteria. Please check your data or adjust settings.</p>"
                )
                return()
            }
            
            # Run factor analysis
            faResult <- private$.runFactorAnalysis(data, nFactors)
            if (is.null(faResult)) return()
            
            private$.faResult <- faResult
            
            # Populate results tables
            private$.populateEigenvalues(data)
            private$.populateCommunalities()
            private$.populateLoadings()
            private$.populateFactorCorrelations()
            private$.populateGoodnessOfFit()
            private$.populateFactorScoresStats()
            
            if (self$options$comprehensive_output) {
                private$.populateComprehensiveResults()
            }
            
            if (self$options$clinical_interpretation) {
                private$.populateClinicalInterpretation()
            }
            
            # Save factor scores if requested
            if (self$options$saveScores && self$options$scores != 'none') {
                private$.saveFactorScores()
            }
        },
        
        .prepareData = function() {
            data <- self$data
            vars <- self$options$vars
            
            # Select variables and remove missing values
            data <- data[vars]
            data <- na.omit(data)
            
            # Check for sufficient data
            if (nrow(data) < 3 * length(vars)) {
                self$results$instructions$setContent(
                    "<p>Insufficient data: Need at least 3 times as many observations as variables.</p>"
                )
                return(NULL)
            }
            
            # Check for multicollinearity
            cor_matrix <- cor(data, use = "complete.obs")
            if (any(abs(cor_matrix[lower.tri(cor_matrix)]) > 0.95)) {
                self$results$instructions$setContent(
                    "<p>Warning: High multicollinearity detected. Consider removing highly correlated variables.</p>"
                )
            }
            
            return(data)
        },
        
        .runAdequacyTests = function(data) {
            adequacyTable <- self$results$adequacyTable
            
            # Bartlett's Test of Sphericity
            if (self$options$bartlettTest) {
                tryCatch({
                    bartlett_result <- psych::cortest.bartlett(cor(data), n = nrow(data))
                    
                    row <- list(
                        test = "Bartlett's Test of Sphericity",
                        statistic = bartlett_result$chisq,
                        pValue = bartlett_result$p.value,
                        result = ifelse(bartlett_result$p.value < 0.05, "Significant", "Non-significant"),
                        interpretation = ifelse(bartlett_result$p.value < 0.05, 
                                              "Correlation matrix is not identity - FA appropriate",
                                              "Correlation matrix may be identity - FA questionable")
                    )
                    adequacyTable$addRow(rowKey = "bartlett", values = row)
                }, error = function(e) {
                    # Skip if error
                })
            }
            
            # Kaiser-Meyer-Olkin Test
            if (self$options$kmoTest) {
                tryCatch({
                    kmo_result <- psych::KMO(cor(data))
                    overall_kmo <- kmo_result$MSA
                    
                    interpretation <- if (overall_kmo >= 0.9) "Excellent"
                                     else if (overall_kmo >= 0.8) "Good"
                                     else if (overall_kmo >= 0.7) "Adequate"
                                     else if (overall_kmo >= 0.6) "Mediocre"
                                     else if (overall_kmo >= 0.5) "Poor"
                                     else "Unacceptable"
                    
                    row <- list(
                        test = "Kaiser-Meyer-Olkin Test",
                        statistic = overall_kmo,
                        pValue = NA,
                        result = interpretation,
                        interpretation = paste("Sampling adequacy is", tolower(interpretation), "for factor analysis")
                    )
                    adequacyTable$addRow(rowKey = "kmo", values = row)
                }, error = function(e) {
                    # Skip if error
                })
            }
        },
        
        .determineFactors = function(data) {
            method <- self$options$nFactorsMethod
            nFactors <- 1
            
            # Calculate eigenvalues for all methods
            private$.eigenvalues <- eigen(cor(data, use = "complete.obs"))$values
            
            switch(method,
                "kaiser" = {
                    nFactors <- sum(private$.eigenvalues >= 1)
                },
                "manual" = {
                    nFactors <- self$options$nFactors
                },
                "parallel" = {
                    nFactors <- private$.runParallelAnalysis(data)
                },
                "scree" = {
                    # Let user inspect scree plot - default to Kaiser rule
                    nFactors <- sum(private$.eigenvalues >= 1)
                }
            )
            
            return(max(1, min(nFactors, length(private$.vars) - 1)))
        },
        
        .runParallelAnalysis = function(data) {
            tryCatch({
                parallel_result <- psych::fa.parallel(data, 
                                                      n.iter = self$options$parallel_iterations,
                                                      fa = "fa",
                                                      fm = "ml")
                private$.parallelResults <- parallel_result
                
                # Populate parallel analysis table
                if (self$options$nFactorsMethod == "parallel") {
                    private$.populateParallelAnalysis()
                }
                
                return(parallel_result$nfact)
            }, error = function(e) {
                return(sum(private$.eigenvalues >= 1))  # Fallback to Kaiser rule
            })
        },
        
        .runFactorAnalysis = function(data, nFactors) {
            method <- self$options$method
            rotation <- self$options$rotation
            scores_method <- self$options$scores
            
            tryCatch({
                if (method == "ml") {
                    # Maximum Likelihood
                    result <- psych::fa(data, 
                                        nfactors = nFactors,
                                        rotate = rotation,
                                        scores = ifelse(scores_method == "none", FALSE, scores_method),
                                        fm = "ml",
                                        max.iter = self$options$maxIterations)
                } else if (method == "paf") {
                    # Principal Axis Factoring
                    result <- psych::fa(data, 
                                        nfactors = nFactors,
                                        rotate = rotation,
                                        scores = ifelse(scores_method == "none", FALSE, scores_method),
                                        fm = "pa",
                                        max.iter = self$options$maxIterations)
                } else if (method == "pc") {
                    # Principal Components
                    result <- psych::principal(data, 
                                               nfactors = nFactors,
                                               rotate = rotation,
                                               scores = TRUE)
                }
                
                return(result)
                
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste("<p>Error in factor analysis:", e$message, "</p>")
                )
                return(NULL)
            })
        },
        
        .populateEigenvalues = function(data) {
            if (!self$options$eigenvalues) return()
            
            eigenTable <- self$results$eigenvaluesTable
            eigenvals <- private$.eigenvalues
            
            total_variance <- sum(eigenvals)
            cumulative <- 0
            
            for (i in 1:length(eigenvals)) {
                variance_percent <- (eigenvals[i] / total_variance) * 100
                cumulative <- cumulative + variance_percent
                
                row <- list(
                    factor = i,
                    eigenvalue = eigenvals[i],
                    variance_percent = variance_percent,
                    cumulative_percent = cumulative
                )
                
                # Add rotated eigenvalues if available
                if (!is.null(private$.faResult) && self$options$rotation != "none") {
                    if (!is.null(private$.faResult$Vaccounted)) {
                        rotated_eigenvals <- private$.faResult$Vaccounted["SS loadings", ]
                        if (i <= length(rotated_eigenvals)) {
                            row$eigenvalue_rotated <- rotated_eigenvals[i]
                            row$variance_percent_rotated <- (rotated_eigenvals[i] / total_variance) * 100
                        }
                    }
                }
                
                eigenTable$addRow(rowKey = i, values = row)
            }
        },
        
        .populateParallelAnalysis = function() {
            if (is.null(private$.parallelResults)) return()
            
            parallelTable <- self$results$parallelAnalysis
            parallel <- private$.parallelResults
            
            for (i in 1:length(parallel$fa.values)) {
                row <- list(
                    factor = i,
                    actual_eigenvalue = parallel$fa.values[i],
                    random_eigenvalue = parallel$fa.sim[i],
                    retain_factor = ifelse(parallel$fa.values[i] > parallel$fa.sim[i], "Yes", "No")
                )
                parallelTable$addRow(rowKey = i, values = row)
            }
        },
        
        .populateCommunalities = function() {
            if (!self$options$communalities || is.null(private$.faResult)) return()
            
            communTable <- self$results$communalitiesTable
            vars <- private$.vars
            
            # Initial communalities (prior estimates)
            initial_comm <- diag(cor(self$data[vars], use = "complete.obs"))
            
            # Extracted communalities
            extracted_comm <- private$.faResult$communalities
            
            for (i in 1:length(vars)) {
                row <- list(
                    variable = vars[i],
                    initial = initial_comm[i],
                    extracted = extracted_comm[i]
                )
                communTable$addRow(rowKey = vars[i], values = row)
            }
        },
        
        .populateLoadings = function() {
            if (is.null(private$.faResult)) return()
            
            loadings <- private$.faResult$loadings
            cutoff <- ifelse(self$options$hideSmallLoadings, self$options$loadingsCutoff, 0)
            
            # Setup tables based on rotation
            if (self$options$rotation == "none") {
                loadingTable <- self$results$factorLoadings
            } else {
                if (self$options$showUnrotated) {
                    # Would need unrotated loadings - skip for now
                }
                if (self$options$showRotated) {
                    loadingTable <- self$results$rotatedLoadings
                }
            }
            
            if (exists("loadingTable") && !is.null(loadingTable)) {
                vars <- private$.vars
                nFactors <- ncol(loadings)
                
                for (i in 1:length(vars)) {
                    row <- list(variable = vars[i])
                    
                    for (j in 1:nFactors) {
                        factor_name <- paste0("factor", j)
                        loading_value <- loadings[i, j]
                        
                        # Apply cutoff if specified
                        if (abs(loading_value) < cutoff) {
                            loading_value <- ""
                        } else {
                            loading_value <- round(loading_value, 3)
                        }
                        
                        row[[factor_name]] <- loading_value
                    }
                    
                    loadingTable$addRow(rowKey = vars[i], values = row)
                }
            }
        },
        
        .populateFactorCorrelations = function() {
            if (!self$options$factorCorrelations || is.null(private$.faResult)) return()
            if (!self$options$rotation %in% c("oblimin", "promax")) return()
            
            # Factor correlations only available for oblique rotations
            if (!is.null(private$.faResult$Phi)) {
                corrTable <- self$results$factorCorrelationsTable
                phi_matrix <- private$.faResult$Phi
                nFactors <- nrow(phi_matrix)
                
                for (i in 1:nFactors) {
                    row <- list(factor = paste("Factor", i))
                    
                    for (j in 1:nFactors) {
                        factor_name <- paste0("factor", j)
                        row[[factor_name]] <- round(phi_matrix[i, j], 3)
                    }
                    
                    corrTable$addRow(rowKey = i, values = row)
                }
            }
        },
        
        .populateGoodnessOfFit = function() {
            if (self$options$method != "ml" || is.null(private$.faResult)) return()
            
            fitTable <- self$results$goodnessOfFit
            
            if (!is.null(private$.faResult$STATISTIC)) {
                row <- list(
                    statistic = private$.faResult$STATISTIC,
                    df = private$.faResult$dof,
                    pValue = private$.faResult$PVAL,
                    result = ifelse(private$.faResult$PVAL < 0.05, 
                                   "Poor fit (p < 0.05)", 
                                   "Acceptable fit (p ≥ 0.05)")
                )
                fitTable$addRow(rowKey = "chisq", values = row)
            }
        },
        
        .populateFactorScoresStats = function() {
            if (self$options$scores == "none" || is.null(private$.faResult)) return()
            if (is.null(private$.faResult$scores)) return()
            
            scoresTable <- self$results$factorScoresStats
            scores <- private$.faResult$scores
            
            for (i in 1:ncol(scores)) {
                factor_scores <- scores[, i]
                
                row <- list(
                    factor = paste("Factor", i),
                    mean = mean(factor_scores, na.rm = TRUE),
                    sd = sd(factor_scores, na.rm = TRUE),
                    min = min(factor_scores, na.rm = TRUE),
                    max = max(factor_scores, na.rm = TRUE)
                )
                
                scoresTable$addRow(rowKey = i, values = row)
            }
        },
        
        .populateComprehensiveResults = function() {
            if (!self$options$comprehensive_output || is.null(private$.faResult)) return()
            
            compTable <- self$results$comprehensiveResults
            
            # Add various summary statistics
            measures <- list(
                list("Total Variance Explained", paste(round(sum(private$.faResult$Vaccounted["Proportion Var", ]) * 100, 1), "%"), 
                     "Percentage of total variance explained by extracted factors"),
                list("Fit Statistic", ifelse(is.null(private$.faResult$fit), "N/A", round(private$.faResult$fit, 3)), 
                     "Overall model fit index"),
                list("Number of Observations", nrow(self$data[private$.vars]), 
                     "Sample size used in analysis"),
                list("Number of Variables", length(private$.vars), 
                     "Number of variables in factor analysis")
            )
            
            for (i in 1:length(measures)) {
                measure <- measures[[i]]
                row <- list(
                    measure = measure[[1]],
                    value = as.character(measure[[2]]),
                    interpretation = measure[[3]]
                )
                compTable$addRow(rowKey = i, values = row)
            }
        },
        
        .populateClinicalInterpretation = function() {
            html <- self$results$clinicalInterpretation
            
            interpretation <- "<h3>Clinical Application Guidance</h3>"
            
            if (!is.null(private$.faResult)) {
                nFactors <- ncol(private$.faResult$loadings)
                
                interpretation <- paste0(interpretation,
                    "<p><strong>Factor Analysis Results Summary:</strong></p>",
                    "<ul>",
                    "<li>Extracted ", nFactors, " factors from ", length(private$.vars), " variables</li>",
                    "<li>Rotation method: ", self$options$rotation, "</li>",
                    "<li>Extraction method: ", switch(self$options$method,
                                                    "ml" = "Maximum Likelihood",
                                                    "paf" = "Principal Axis Factoring",
                                                    "pc" = "Principal Components"), "</li>",
                    "</ul>"
                )
                
                interpretation <- paste0(interpretation,
                    "<p><strong>Clinical Applications:</strong></p>",
                    "<ul>",
                    "<li><strong>Scale Development:</strong> Use factor loadings to identify items that cluster together</li>",
                    "<li><strong>Dimensionality Reduction:</strong> Replace multiple correlated variables with fewer factor scores</li>",
                    "<li><strong>Construct Validation:</strong> Confirm theoretical structure of clinical measures</li>",
                    "<li><strong>Data Interpretation:</strong> Focus on factors with eigenvalues > 1 and strong loadings (> 0.3)</li>",
                    "</ul>"
                )
                
                interpretation <- paste0(interpretation,
                    "<p><strong>Interpretation Guidelines:</strong></p>",
                    "<ul>",
                    "<li>Loadings > 0.7: Excellent</li>",
                    "<li>Loadings 0.5-0.7: Good</li>",
                    "<li>Loadings 0.3-0.5: Acceptable</li>",
                    "<li>Loadings < 0.3: Poor (consider removal)</li>",
                    "</ul>"
                )
            }
            
            html$setContent(interpretation)
        },
        
        .saveFactorScores = function() {
            # This would save factor scores to the dataset
            # Implementation depends on jamovi's data modification capabilities
            # For now, just note that scores are available in the results
            
            if (!is.null(private$.faResult$scores)) {
                # Factor scores are calculated and available in private$.faResult$scores
                # In a full implementation, these would be added to the dataset
                # with names using self$options$scoresPrefix
            }
        },
        
        .setupLoadingTables = function() {
            # Dynamic setup of loading tables based on number of factors
            # This would be called after determining the number of factors
            # For now, we'll rely on the base table structure
        },
        
        .getInstructions = function() {
            instructions <- "<h2>Factor Analysis Instructions</h2>"
            instructions <- paste0(instructions,
                "<p>Factor analysis is used to identify underlying latent variables (factors) that explain patterns of correlations among observed variables.</p>",
                "<h3>Getting Started:</h3>",
                "<ol>",
                "<li>Select 2 or more numeric variables</li>",
                "<li>Choose extraction method (Maximum Likelihood recommended)</li>",
                "<li>Select method for determining number of factors</li>",
                "<li>Choose rotation method if desired</li>",
                "<li>Review sampling adequacy tests before interpreting results</li>",
                "</ol>",
                "<p><strong>Minimum Requirements:</strong> At least 3 observations per variable, preferably 10:1 ratio.</p>"
            )
            return(instructions)
        },
        
        # Plotting functions
        .plotScree = function(image, ggtheme, theme, ...) {
            if (is.null(private$.eigenvalues)) return()
            
            eigenvals <- private$.eigenvalues
            factors <- 1:length(eigenvals)
            
            plotData <- data.frame(
                Factor = factors,
                Eigenvalue = eigenvals
            )
            
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Factor, y = Eigenvalue)) +
                ggplot2::geom_line() +
                ggplot2::geom_point() +
                ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
                ggplot2::labs(
                    title = "Scree Plot",
                    x = "Factor",
                    y = "Eigenvalue",
                    subtitle = "Dashed line shows Kaiser criterion (eigenvalue = 1)"
                ) +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .plotLoadings = function(image, ggtheme, theme, ...) {
            if (is.null(private$.faResult)) return()
            
            loadings <- private$.faResult$loadings
            if (ncol(loadings) < 2) return()
            
            plotData <- data.frame(
                Variable = rownames(loadings),
                Factor1 = loadings[, 1],
                Factor2 = loadings[, 2]
            )
            
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Factor1, y = Factor2, label = Variable)) +
                ggplot2::geom_point() +
                ggplot2::geom_text(vjust = -0.5) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
                ggplot2::labs(
                    title = "Factor Loadings Plot",
                    x = "Factor 1",
                    y = "Factor 2"
                ) +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .plotBiplot = function(image, ggtheme, theme, ...) {
            # Biplot implementation would go here
            # Requires both loadings and scores
            TRUE
        },
        
        .plotParallel = function(image, ggtheme, theme, ...) {
            if (is.null(private$.parallelResults)) return()
            
            parallel <- private$.parallelResults
            factors <- 1:length(parallel$fa.values)
            
            plotData <- data.frame(
                Factor = rep(factors, 2),
                Eigenvalue = c(parallel$fa.values, parallel$fa.sim),
                Type = rep(c("Actual", "Random"), each = length(factors))
            )
            
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Factor, y = Eigenvalue, color = Type)) +
                ggplot2::geom_line() +
                ggplot2::geom_point() +
                ggplot2::labs(
                    title = "Parallel Analysis",
                    x = "Factor",
                    y = "Eigenvalue",
                    subtitle = "Factors above the random line should be retained"
                ) +
                ggtheme
            
            print(p)
            TRUE
        }
    )
)