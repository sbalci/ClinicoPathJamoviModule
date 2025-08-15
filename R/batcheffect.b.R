# This file is a generated template, your changes will not be overwritten

#' @import jmvcore
#' @import R6
#' @importFrom sva ComBat
#' @importFrom stats prcomp var cor dist hclust cutree
#' @importFrom utils capture.output
#' @importFrom VIF vif
#' @export

batcheffectClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "batcheffectClass",
    inherit = batcheffectBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$features) || 
                length(self$options$features) == 0) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>
                    </head>
                    <body>
                    <h3>Batch Effect Control & Quality Assessment</h3>
                    <p><b>Essential for High-Quality Data Analysis</b></p>
                    
                    <p>This module addresses the critical \"garbage in, garbage out\" problem in 
                    digital pathology and multi-institutional studies by providing comprehensive 
                    batch effect detection and correction capabilities.</p>
                    
                    <h4>Getting Started:</h4>
                    <ol>
                    <li><b>Feature Variables:</b> Select numeric variables to assess for batch effects</li>
                    <li><b>Batch Variable:</b> Specify the technical grouping variable (institution, date, etc.)</li>
                    <li><b>Biological Variable:</b> Optional biological grouping to preserve during correction</li>
                    <li><b>Analysis Options:</b> Enable PCA visualization and ComBat correction</li>
                    </ol>
                    
                    <h4>Key Features:</h4>
                    <ul>
                    <li><b>PCA Visualization:</b> Detect batch effects through principal component analysis</li>
                    <li><b>ComBat Correction:</b> Remove batch effects while preserving biological variation</li>
                    <li><b>Feature Quality Assessment:</b> Distribution analysis and outlier detection</li>
                    <li><b>Redundancy Analysis:</b> Identify highly correlated features</li>
                    <li><b>Quality Control Metrics:</b> Comprehensive QC for high-dimensional data</li>
                    </ul>
                    
                    <h4>Clinical Applications:</h4>
                    <ul>
                    <li>Multi-institutional biomarker studies</li>
                    <li>Digital pathology feature harmonization</li>
                    <li>Longitudinal study quality control</li>
                    <li>High-dimensional data preprocessing</li>
                    </ul>
                    </body>
                    </html>"
                )
                return()
            }
        },

        .run = function() {
            # Check for required packages
            required_packages <- c("sva", "limma")
            missing_packages <- c()
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }
            
            if (length(missing_packages) > 0) {
                error_msg <- paste0(
                    "<p style='color: red;'><b>Error:</b> Required packages not installed: ", 
                    paste(missing_packages, collapse = ", "), 
                    "<br>Please install with: install.packages(c('", 
                    paste(missing_packages, collapse = "', '"), "'))</p>"
                )
                self$results$instructions$setContent(error_msg)
                return()
            }
            
            # Get variables
            features <- self$options$features
            batch_var <- self$options$batch_var
            biological_var <- self$options$biological_var
            
            # Get data
            data <- self$data
            
            # Validate inputs
            if (length(features) == 0) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Error:</b> Please select at least one feature variable.</p>"
                )
                return()
            }
            
            if (is.null(batch_var)) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Error:</b> Please specify a batch variable.</p>"
                )
                return()
            }
            
            # Prepare data for analysis
            tryCatch({
                analysisData <- private$.prepareData(data, features, batch_var, biological_var)
                
                if (is.null(analysisData)) {
                    return()
                }
                
                # Perform analyses
                private$.populateSummaryTable(analysisData)
                
                if (self$options$perform_pca) {
                    private$.performBatchDetection(analysisData)
                }
                
                if (self$options$feature_quality) {
                    private$.performFeatureQuality(analysisData)
                }
                
                if (self$options$redundancy_analysis) {
                    private$.performRedundancyAnalysis(analysisData)
                }
                
                if (self$options$perform_combat) {
                    private$.performComBatCorrection(analysisData)
                }
                
                # Generate plots
                if (self$options$show_plots) {
                    private$.preparePlots(analysisData)
                }
                
                # Generate interpretation
                private$.generateInterpretation(analysisData)
                
            }, error = function(e) {
                error_msg <- paste0("<p style='color: red;'><b>Analysis Error:</b> ", e$message, "</p>")
                self$results$instructions$setContent(error_msg)
            })
        },
        
        .prepareData = function(data, features, batch_var, biological_var) {
            # Create analysis dataset
            vars_to_include <- c(features, batch_var)
            if (!is.null(biological_var)) {
                vars_to_include <- c(vars_to_include, biological_var)
            }
            
            # Extract relevant columns
            analysis_data <- data[, vars_to_include, drop = FALSE]
            
            # Remove rows with missing batch information
            if (sum(is.na(analysis_data[[batch_var]])) > 0) {
                analysis_data <- analysis_data[!is.na(analysis_data[[batch_var]]), ]
            }
            
            # Check minimum sample size
            if (nrow(analysis_data) < 10) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Error:</b> Insufficient data. Need at least 10 complete observations.</p>"
                )
                return(NULL)
            }
            
            # Check batch variable
            batch_counts <- table(analysis_data[[batch_var]])
            if (length(batch_counts) < 2) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Error:</b> Batch variable must have at least 2 groups.</p>"
                )
                return(NULL)
            }
            
            if (any(batch_counts < 3)) {
                self$results$instructions$setContent(
                    "<p style='color: orange;'><b>Warning:</b> Some batches have very few samples (<3). Results may be unreliable.</p>"
                )
            }
            
            # Ensure batch variable is factor
            analysis_data[[batch_var]] <- as.factor(analysis_data[[batch_var]])
            if (!is.null(biological_var)) {
                analysis_data[[biological_var]] <- as.factor(analysis_data[[biological_var]])
            }
            
            return(analysis_data)
        },
        
        .populateSummaryTable = function(data) {
            summary_table <- self$results$summary
            features <- self$options$features
            batch_var <- self$options$batch_var
            
            # Basic data summary
            n_samples <- nrow(data)
            n_features <- length(features)
            n_batches <- length(unique(data[[batch_var]]))
            
            summary_table$addRow(rowKey="samples", values=list(
                metric="Total Samples",
                value=as.character(n_samples),
                status="Info",
                recommendation="Sample size adequate"
            ))
            
            summary_table$addRow(rowKey="features", values=list(
                metric="Features Analyzed",
                value=as.character(n_features),
                status="Info",
                recommendation="Feature count appropriate"
            ))
            
            summary_table$addRow(rowKey="batches", values=list(
                metric="Number of Batches",
                value=as.character(n_batches),
                status="Info",
                recommendation="Multiple batches detected"
            ))
            
            # Missing data summary
            feature_data <- data[, features, drop = FALSE]
            missing_pct <- round(sum(is.na(feature_data)) / (nrow(feature_data) * ncol(feature_data)) * 100, 2)
            
            missing_status <- if (missing_pct < 5) "Good" else if (missing_pct < 20) "Fair" else "Poor"
            missing_rec <- if (missing_pct < 5) "Low missing data" else if (missing_pct < 20) "Consider imputation" else "High missing data - investigate"
            
            summary_table$addRow(rowKey="missing", values=list(
                metric="Missing Data %",
                value=paste0(missing_pct, "%"),
                status=missing_status,
                recommendation=missing_rec
            ))
            
            # Batch imbalance
            batch_sizes <- table(data[[batch_var]])
            batch_cv <- sd(batch_sizes) / mean(batch_sizes)
            
            balance_status <- if (batch_cv < 0.3) "Good" else if (batch_cv < 0.6) "Fair" else "Poor"
            balance_rec <- if (batch_cv < 0.3) "Well-balanced batches" else "Consider batch size in interpretation"
            
            summary_table$addRow(rowKey="balance", values=list(
                metric="Batch Balance (CV)",
                value=round(batch_cv, 3),
                status=balance_status,
                recommendation=balance_rec
            ))
        },
        
        .performBatchDetection = function(data) {
            detection_table <- self$results$batch_detection
            features <- self$options$features
            batch_var <- self$options$batch_var
            
            tryCatch({
                # Extract feature matrix
                feature_matrix <- as.matrix(data[, features, drop = FALSE])
                
                # Remove features with no variance or all missing
                var_check <- apply(feature_matrix, 2, function(x) var(x, na.rm = TRUE))
                valid_features <- !is.na(var_check) & var_check > 1e-10
                
                if (sum(valid_features) < 2) {
                    detection_table$addRow(rowKey="error", values=list(
                        test="Error",
                        statistic=NA,
                        p_value=NA,
                        effect_size=NA,
                        interpretation="Insufficient valid features for analysis"
                    ))
                    return()
                }
                
                feature_matrix <- feature_matrix[, valid_features, drop = FALSE]
                
                # Handle missing values
                if (any(is.na(feature_matrix))) {
                    # Remove samples with any missing values for PCA
                    complete_cases <- complete.cases(feature_matrix)
                    if (sum(complete_cases) < 10) {
                        detection_table$addRow(rowKey="error", values=list(
                            test="Error",
                            statistic=NA,
                            p_value=NA,
                            effect_size=NA,
                            interpretation="Too many missing values for PCA analysis"
                        ))
                        return()
                    }
                    feature_matrix <- feature_matrix[complete_cases, , drop = FALSE]
                    batch_vector <- data[[batch_var]][complete_cases]
                } else {
                    batch_vector <- data[[batch_var]]
                }
                
                # Perform PCA
                pca_result <- prcomp(feature_matrix, center = TRUE, scale. = TRUE)
                
                # Test PC1 vs batch effect
                pc1_scores <- pca_result$x[, 1]
                batch_anova <- aov(pc1_scores ~ batch_vector)
                anova_summary <- summary(batch_anova)
                
                f_stat <- anova_summary[[1]]$`F value`[1]
                p_value <- anova_summary[[1]]$`Pr(>F)`[1]
                eta_squared <- anova_summary[[1]]$`Sum Sq`[1] / sum(anova_summary[[1]]$`Sum Sq`)
                
                interpretation <- if (p_value < 0.001) {
                    "Strong batch effect detected"
                } else if (p_value < 0.05) {
                    "Moderate batch effect detected"
                } else {
                    "No significant batch effect"
                }
                
                detection_table$addRow(rowKey="pc1_batch", values=list(
                    test="PC1 vs Batch (ANOVA)",
                    statistic=round(f_stat, 3),
                    p_value=p_value,
                    effect_size=round(eta_squared, 3),
                    interpretation=interpretation
                ))
                
                # Variance explained by first 3 PCs
                var_explained <- summary(pca_result)$importance[2, 1:min(3, ncol(pca_result$x))]
                
                detection_table$addRow(rowKey="pc1_var", values=list(
                    test="PC1 Variance Explained",
                    statistic=round(var_explained[1] * 100, 1),
                    p_value=NA,
                    effect_size=NA,
                    interpretation=paste0(round(var_explained[1] * 100, 1), "% of total variance")
                ))
                
                if (length(var_explained) >= 2) {
                    detection_table$addRow(rowKey="pc2_var", values=list(
                        test="PC2 Variance Explained",
                        statistic=round(var_explained[2] * 100, 1),
                        p_value=NA,
                        effect_size=NA,
                        interpretation=paste0(round(var_explained[2] * 100, 1), "% of total variance")
                    ))
                }
                
                # Store PCA results for plotting
                private$.pca_result <- pca_result
                private$.batch_vector <- batch_vector
                
            }, error = function(e) {
                detection_table$addRow(rowKey="error", values=list(
                    test="Analysis Error",
                    statistic=NA,
                    p_value=NA,
                    effect_size=NA,
                    interpretation=paste("PCA analysis failed:", e$message)
                ))
            })
        },
        
        .performFeatureQuality = function(data) {
            quality_table <- self$results$feature_quality
            features <- self$options$features
            
            for (feature in features) {
                tryCatch({
                    feature_data <- data[[feature]]
                    
                    # Missing percentage
                    missing_pct <- round(sum(is.na(feature_data)) / length(feature_data) * 100, 2)
                    
                    # Variance
                    variance <- var(feature_data, na.rm = TRUE)
                    
                    # Outlier detection
                    outlier_count <- private$.detectOutliers(feature_data)
                    
                    # Distribution assessment
                    distribution <- private$.assessDistribution(feature_data)
                    
                    # Quality score (0-100)
                    quality_score <- private$.calculateQualityScore(feature_data, missing_pct, variance, outlier_count)
                    
                    # Recommendation
                    recommendation <- private$.getQualityRecommendation(quality_score, missing_pct, variance)
                    
                    quality_table$addRow(rowKey=feature, values=list(
                        feature=feature,
                        missing_pct=missing_pct,
                        variance=round(variance, 4),
                        outliers=outlier_count,
                        distribution=distribution,
                        quality_score=round(quality_score, 1),
                        recommendation=recommendation
                    ))
                    
                }, error = function(e) {
                    quality_table$addRow(rowKey=feature, values=list(
                        feature=feature,
                        missing_pct=NA,
                        variance=NA,
                        outliers=NA,
                        distribution="Error",
                        quality_score=NA,
                        recommendation=paste("Analysis failed:", e$message)
                    ))
                })
            }
        },
        
        .performRedundancyAnalysis = function(data) {
            redundancy_table <- self$results$redundancy
            features <- self$options$features
            correlation_threshold <- self$options$correlation_threshold
            
            if (length(features) < 2) {
                redundancy_table$addRow(rowKey="insufficient", values=list(
                    feature_pair="Insufficient Features",
                    correlation=NA,
                    vif=NA,
                    redundancy_type="Need ≥2 features",
                    recommendation="Add more features for redundancy analysis"
                ))
                return()
            }
            
            tryCatch({
                feature_matrix <- data[, features, drop = FALSE]
                
                # Remove rows with any missing values
                complete_cases <- complete.cases(feature_matrix)
                if (sum(complete_cases) < 10) {
                    redundancy_table$addRow(rowKey="insufficient_data", values=list(
                        feature_pair="Insufficient Complete Data",
                        correlation=NA,
                        vif=NA,
                        redundancy_type="Too many missing values",
                        recommendation="Handle missing data before redundancy analysis"
                    ))
                    return()
                }
                
                feature_matrix <- feature_matrix[complete_cases, , drop = FALSE]
                
                # Calculate correlation matrix
                cor_matrix <- cor(feature_matrix, use = "complete.obs")
                
                # Find highly correlated pairs
                high_cor_pairs <- c()
                for (i in 1:(ncol(cor_matrix)-1)) {
                    for (j in (i+1):ncol(cor_matrix)) {
                        if (abs(cor_matrix[i, j]) >= correlation_threshold) {
                            pair_name <- paste(colnames(cor_matrix)[i], "-", colnames(cor_matrix)[j])
                            correlation <- cor_matrix[i, j]
                            
                            redundancy_type <- if (abs(correlation) >= 0.95) {
                                "Highly Redundant"
                            } else if (abs(correlation) >= 0.8) {
                                "Moderately Redundant"
                            } else {
                                "Potentially Redundant"
                            }
                            
                            recommendation <- if (abs(correlation) >= 0.95) {
                                "Consider removing one feature"
                            } else if (abs(correlation) >= 0.8) {
                                "Monitor for multicollinearity"
                            } else {
                                "Acceptable correlation level"
                            }
                            
                            redundancy_table$addRow(rowKey=paste0("pair_", i, "_", j), values=list(
                                feature_pair=pair_name,
                                correlation=round(correlation, 3),
                                vif=NA,  # VIF calculation would require regression
                                redundancy_type=redundancy_type,
                                recommendation=recommendation
                            ))
                        }
                    }
                }
                
                if (nrow(redundancy_table$asDF) == 0) {
                    redundancy_table$addRow(rowKey="no_redundancy", values=list(
                        feature_pair="No High Correlations Found",
                        correlation=NA,
                        vif=NA,
                        redundancy_type="Low Redundancy",
                        recommendation="Feature set appears well-diversified"
                    ))
                }
                
            }, error = function(e) {
                redundancy_table$addRow(rowKey="error", values=list(
                    feature_pair="Analysis Error",
                    correlation=NA,
                    vif=NA,
                    redundancy_type="Error",
                    recommendation=paste("Redundancy analysis failed:", e$message)
                ))
            })
        },
        
        .performComBatCorrection = function(data) {
            if (!requireNamespace("sva", quietly = TRUE)) {
                self$results$combat_results$addRow(rowKey="error", values=list(
                    metric="Error",
                    before_correction="sva package required",
                    after_correction="",
                    improvement="",
                    interpretation="Install sva package for ComBat correction"
                ))
                return()
            }
            
            combat_table <- self$results$combat_results
            features <- self$options$features
            batch_var <- self$options$batch_var
            biological_var <- self$options$biological_var
            
            tryCatch({
                # Prepare data for ComBat
                feature_matrix <- as.matrix(data[, features, drop = FALSE])
                feature_matrix <- t(feature_matrix)  # ComBat expects features as rows
                
                batch <- data[[batch_var]]
                
                # Prepare biological covariates
                mod <- NULL
                if (!is.null(biological_var)) {
                    biological <- data[[biological_var]]
                    mod <- model.matrix(~ biological)
                }
                
                # Handle missing values - ComBat requires complete data
                if (any(is.na(feature_matrix))) {
                    complete_samples <- apply(feature_matrix, 2, function(x) !any(is.na(x)))
                    if (sum(complete_samples) < 10) {
                        combat_table$addRow(rowKey="error", values=list(
                            metric="Error",
                            before_correction="Too many missing values",
                            after_correction="",
                            improvement="",
                            interpretation="ComBat requires complete data"
                        ))
                        return()
                    }
                    
                    feature_matrix <- feature_matrix[, complete_samples, drop = FALSE]
                    batch <- batch[complete_samples]
                    if (!is.null(mod)) {
                        mod <- mod[complete_samples, , drop = FALSE]
                    }
                }
                
                # Calculate metrics before correction
                batch_effect_before <- private$.calculateBatchEffect(feature_matrix, batch)
                
                # Apply ComBat
                corrected_matrix <- sva::ComBat(
                    dat = feature_matrix,
                    batch = batch,
                    mod = mod,
                    par.prior = self$options$combat_parametric,
                    prior.plots = FALSE
                )
                
                # Calculate metrics after correction
                batch_effect_after <- private$.calculateBatchEffect(corrected_matrix, batch)
                
                # Calculate improvement
                improvement <- ((batch_effect_before - batch_effect_after) / batch_effect_before) * 100
                
                interpretation <- if (improvement > 50) {
                    "Excellent batch effect reduction"
                } else if (improvement > 25) {
                    "Good batch effect reduction"
                } else if (improvement > 0) {
                    "Modest batch effect reduction"
                } else {
                    "No improvement or batch effect increased"
                }
                
                combat_table$addRow(rowKey="batch_effect", values=list(
                    metric="Batch Effect (F-statistic)",
                    before_correction=round(batch_effect_before, 3),
                    after_correction=round(batch_effect_after, 3),
                    improvement=round(improvement, 1),
                    interpretation=interpretation
                ))
                
                # Store corrected data for optional saving
                private$.corrected_data <- t(corrected_matrix)
                
            }, error = function(e) {
                combat_table$addRow(rowKey="error", values=list(
                    metric="ComBat Error",
                    before_correction="",
                    after_correction="",
                    improvement="",
                    interpretation=paste("ComBat correction failed:", e$message)
                ))
            })
        },
        
        .calculateBatchEffect = function(feature_matrix, batch) {
            # Calculate overall batch effect using average F-statistic across features
            f_stats <- c()
            
            for (i in 1:nrow(feature_matrix)) {
                feature_values <- feature_matrix[i, ]
                if (var(feature_values, na.rm = TRUE) > 1e-10) {
                    tryCatch({
                        anova_result <- aov(feature_values ~ batch)
                        f_stat <- summary(anova_result)[[1]]$`F value`[1]
                        if (!is.na(f_stat)) {
                            f_stats <- c(f_stats, f_stat)
                        }
                    }, error = function(e) {
                        # Skip features that cause errors
                    })
                }
            }
            
            if (length(f_stats) > 0) {
                return(mean(f_stats, na.rm = TRUE))
            } else {
                return(NA)
            }
        },
        
        .detectOutliers = function(x) {
            method <- self$options$outlier_method
            threshold <- self$options$outlier_threshold
            
            x_clean <- x[!is.na(x)]
            if (length(x_clean) < 3) return(0)
            
            outliers <- c()
            
            if (method == "iqr") {
                Q1 <- quantile(x_clean, 0.25)
                Q3 <- quantile(x_clean, 0.75)
                IQR <- Q3 - Q1
                outliers <- which(x_clean < (Q1 - threshold * IQR) | x_clean > (Q3 + threshold * IQR))
            } else if (method == "zscore") {
                z_scores <- abs(scale(x_clean))
                outliers <- which(z_scores > threshold)
            } else if (method == "robust") {
                median_x <- median(x_clean)
                mad_x <- mad(x_clean)
                if (mad_x > 0) {
                    robust_z <- abs((x_clean - median_x) / mad_x)
                    outliers <- which(robust_z > threshold)
                }
            }
            
            return(length(outliers))
        },
        
        .assessDistribution = function(x) {
            x_clean <- x[!is.na(x)]
            if (length(x_clean) < 3) return("Insufficient data")
            
            # Simple distribution assessment
            skewness <- private$.calculateSkewness(x_clean)
            
            if (abs(skewness) < 0.5) {
                return("Normal-like")
            } else if (abs(skewness) < 1) {
                return("Moderately skewed")
            } else {
                return("Highly skewed")
            }
        },
        
        .calculateSkewness = function(x) {
            n <- length(x)
            mean_x <- mean(x)
            sd_x <- sd(x)
            
            if (sd_x == 0) return(0)
            
            skew <- sum((x - mean_x)^3 / sd_x^3) / n
            return(skew)
        },
        
        .calculateQualityScore = function(x, missing_pct, variance, outlier_count) {
            score <- 100
            
            # Penalize for missing data
            score <- score - (missing_pct * 2)  # -2 points per 1% missing
            
            # Penalize for low variance
            if (is.na(variance) || variance < self$options$variance_threshold) {
                score <- score - 30
            }
            
            # Penalize for high outlier count
            outlier_pct <- (outlier_count / length(x[!is.na(x)])) * 100
            score <- score - (outlier_pct * 1.5)  # -1.5 points per 1% outliers
            
            return(max(0, min(100, score)))
        },
        
        .getQualityRecommendation = function(quality_score, missing_pct, variance) {
            if (quality_score >= 80) {
                return("High quality feature")
            } else if (quality_score >= 60) {
                return("Acceptable quality")
            } else if (missing_pct > self$options$missing_threshold) {
                return("High missing data - consider imputation or removal")
            } else if (is.na(variance) || variance < self$options$variance_threshold) {
                return("Low variance - consider removal")
            } else {
                return("Quality issues detected - investigate")
            }
        },
        
        .preparePlots = function(data) {
            # Set states for plots
            if (self$options$perform_pca && exists(".pca_result", envir = private)) {
                pca_plot <- self$results$pca_plot
                pca_plot$setState(list(
                    pca_result = private$.pca_result,
                    batch_vector = private$.batch_vector,
                    biological_var = self$options$biological_var
                ))
            }
            
            if (self$options$feature_quality) {
                quality_plot <- self$results$quality_plot
                quality_plot$setState(list(
                    data = data,
                    features = self$options$features,
                    outlier_method = self$options$outlier_method
                ))
            }
            
            if (self$options$redundancy_analysis) {
                correlation_plot <- self$results$correlation_plot
                correlation_plot$setState(list(
                    data = data,
                    features = self$options$features
                ))
            }
        },
        
        .pca_plot = function(image, ggtheme, ...) {
            if (is.null(image$state)) return()
            
            state <- image$state
            pca_result <- state$pca_result
            batch_vector <- state$batch_vector
            
            # Create PCA plot
            pca_data <- data.frame(
                PC1 = pca_result$x[, 1],
                PC2 = pca_result$x[, 2],
                Batch = batch_vector
            )
            
            p <- ggplot2::ggplot(pca_data, ggplot2::aes(x = PC1, y = PC2, color = Batch)) +
                ggplot2::geom_point(size = 3, alpha = 0.7) +
                ggplot2::labs(
                    title = "PCA - Batch Effect Visualization",
                    x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "% variance)"),
                    y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "% variance)")
                ) +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .quality_plot = function(image, ggtheme, ...) {
            if (is.null(image$state)) return()
            
            state <- image$state
            data <- state$data
            features <- state$features
            
            # Create a simple quality overview plot
            if (length(features) > 0) {
                # Missing data heatmap-style plot
                missing_data <- data[, features, drop = FALSE]
                missing_matrix <- is.na(missing_data)
                
                # Convert to long format for ggplot
                plot_data <- expand.grid(
                    Sample = 1:nrow(missing_matrix),
                    Feature = colnames(missing_matrix)
                )
                plot_data$Missing <- as.vector(missing_matrix)
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Sample, y = Feature, fill = Missing)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "lightblue")) +
                    ggplot2::labs(title = "Feature Quality Overview - Missing Data Pattern") +
                    ggtheme
                
                print(p)
            }
            TRUE
        },
        
        .correlation_plot = function(image, ggtheme, ...) {
            if (is.null(image$state)) return()
            
            state <- image$state
            data <- state$data
            features <- state$features
            
            if (length(features) >= 2) {
                feature_data <- data[, features, drop = FALSE]
                complete_data <- feature_data[complete.cases(feature_data), ]
                
                if (nrow(complete_data) > 0) {
                    cor_matrix <- cor(complete_data)
                    
                    # Convert correlation matrix to long format
                    cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
                    cor_df$Correlation <- as.vector(cor_matrix)
                    
                    p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
                        ggplot2::geom_tile() +
                        ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
                        ggplot2::labs(title = "Feature Correlation Matrix") +
                        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                        ggtheme
                    
                    print(p)
                }
            }
            TRUE
        },
        
        .generateInterpretation = function(data) {
            interpretation_text <- self$results$interpretation
            
            n_samples <- nrow(data)
            n_features <- length(self$options$features)
            n_batches <- length(unique(data[[self$options$batch_var]]))
            
            interpretation <- paste0(
                "<html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'></head><body>",
                "<h3>Batch Effect Control & Quality Assessment - Clinical Interpretation</h3>",
                
                "<h4>Analysis Summary</h4>",
                "<p><b>Dataset Characteristics:</b></p>",
                "<ul>",
                "<li>Samples analyzed: ", n_samples, "</li>",
                "<li>Features analyzed: ", n_features, "</li>",
                "<li>Batches detected: ", n_batches, "</li>",
                "</ul>",
                
                "<h4>Clinical Significance of Batch Effects</h4>",
                "<p><b>Why Batch Effect Control Matters:</b></p>",
                "<ul>",
                "<li><b>Data Integrity:</b> Ensures technical variation doesn't confound biological signals</li>",
                "<li><b>Reproducibility:</b> Critical for multi-institutional studies and clinical validation</li>",
                "<li><b>Statistical Power:</b> Reduces noise to improve detection of true biological effects</li>",
                "<li><b>Clinical Translation:</b> Essential for biomarker development and regulatory approval</li>",
                "</ul>",
                
                "<h4>Quality Control Recommendations</h4>",
                "<p><b>Data Quality Assessment:</b></p>",
                "<ul>",
                "<li><b>Missing Data:</b> Address high missing rates through imputation or feature removal</li>",
                "<li><b>Low Variance Features:</b> Consider removing features with minimal variation</li>",
                "<li><b>Outliers:</b> Investigate outliers for potential data quality issues</li>",
                "<li><b>Redundancy:</b> Remove highly correlated features to reduce multicollinearity</li>",
                "</ul>",
                
                "<h4>Batch Effect Correction</h4>",
                "<p><b>ComBat Correction Guidelines:</b></p>",
                "<ul>",
                "<li><b>When to Apply:</b> Significant batch effects detected via PCA or statistical tests</li>",
                "<li><b>Biological Preservation:</b> Always specify biological variables to preserve</li>",
                "<li><b>Validation:</b> Verify correction effectiveness and biological signal preservation</li>",
                "<li><b>Documentation:</b> Report correction methods in publications</li>",
                "</ul>",
                
                "<h4>Next Steps for Clinical Application</h4>",
                "<ul>",
                "<li>Apply quality filters based on recommendations</li>",
                "<li>Use corrected data for downstream analysis</li>",
                "<li>Validate findings in independent datasets</li>",
                "<li>Consider batch effects in study design for future work</li>",
                "</ul>",
                
                "</body></html>"
            )
            
            interpretation_text$setContent(interpretation)
        }
    )
)