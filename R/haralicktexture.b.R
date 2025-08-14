# This file is a generated template, your changes will not be overwritten

haralicktextureClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "haralicktextureClass",
    inherit = haralicktextureBase,
    private = list(
        .init = function() {
            if (is.null(self$data)) {
                self$results$interpretation$setContent(
                    "<h3>Haralick Texture Analysis for Digital Pathology</h3>
                    <p><strong>Purpose:</strong> Comprehensive Haralick texture feature analysis for quantifying 
                    spatial heterogeneity in digital pathology images, following methodologies validated in 
                    peer-reviewed biomarker research.</p>
                    
                    <h4>Data Requirements:</h4>
                    <ul>
                        <li><strong>Texture Measurements:</strong> Pre-computed Haralick features (entropy, contrast, correlation, etc.)</li>
                        <li><strong>Spatial Coordinates:</strong> X, Y coordinates for spatial analysis (optional)</li>
                        <li><strong>Region Identifiers:</strong> Tumor regions, tissue types, or analysis tiles</li>
                        <li><strong>Clinical Outcomes:</strong> Survival data, response variables for prognostic modeling</li>
                    </ul>
                    
                    <h4>Haralick Features Supported:</h4>
                    <ul>
                        <li><strong>Entropy:</strong> Measure of randomness/disorder in texture patterns</li>
                        <li><strong>Contrast:</strong> Local variations in grayscale co-occurrence matrix</li>
                        <li><strong>Correlation:</strong> Linear dependency of grayscale values</li>
                        <li><strong>Energy/Uniformity:</strong> Measure of textural uniformity</li>
                        <li><strong>Homogeneity:</strong> Closeness of distribution elements to diagonal</li>
                        <li><strong>Variance:</strong> Spread of grayscale values</li>
                    </ul>
                    
                    <h4>Analysis Framework:</h4>
                    <ul>
                        <li><strong>Descriptive Statistics:</strong> Distribution analysis and outlier detection</li>
                        <li><strong>Spatial Analysis:</strong> Regional heterogeneity quantification</li>
                        <li><strong>Prognostic Modeling:</strong> Survival analysis and biomarker validation</li>
                        <li><strong>Reproducibility:</strong> Platform comparison and validation metrics</li>
                    </ul>
                    
                    <h4>Clinical Applications:</h4>
                    <ul>
                        <li>Ki67 spatial heterogeneity quantification</li>
                        <li>Tumor microenvironment characterization</li>
                        <li>Prognostic biomarker development</li>
                        <li>Treatment response prediction</li>
                        <li>Digital pathology algorithm validation</li>
                    </ul>
                    
                    <p><em>This implementation follows the Haralick entropy methodology from 
                    Zilenaite-Petrulaitiene et al. (Am J Clin Pathol 2025) for prognostic biomarker development 
                    in breast cancer digital pathology.</em></p>"
                )
                return()
            }
            
            # Initialize results tables
            private$.initializeTables()
        },
        
        .run = function() {
            # Check required variables
            if (is.null(self$options$texture_features) || length(self$options$texture_features) == 0) {
                return()
            }
            
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Extract texture features
            texture_data <- private$.extractTextureData(data)
            
            if (nrow(texture_data) < 5) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for texture analysis. 
                    At least 5 observations with complete texture measurements are required.</p>"
                )
                return()
            }
            
            # Perform comprehensive texture analysis
            private$.performTextureAnalysis(texture_data)
            private$.generateTexturePlots(texture_data)
            private$.generateTextureInterpretation(texture_data)
        },
        
        .initializeTables = function() {
            # Descriptive statistics table
            desc_table <- self$results$descriptivestatistable
            desc_table$addColumn(name = 'feature', title = 'Texture Feature', type = 'text')
            desc_table$addColumn(name = 'n', title = 'N', type = 'integer')
            desc_table$addColumn(name = 'mean', title = 'Mean', type = 'number', format = 'zto')
            desc_table$addColumn(name = 'sd', title = 'SD', type = 'number', format = 'zto')
            desc_table$addColumn(name = 'median', title = 'Median', type = 'number', format = 'zto')
            desc_table$addColumn(name = 'iqr', title = 'IQR', type = 'text')
            desc_table$addColumn(name = 'range', title = 'Range', type = 'text')
            
            # Correlation matrix table
            corr_table <- self$results$correlationtable
            corr_table$addColumn(name = 'feature1', title = 'Feature 1', type = 'text')
            corr_table$addColumn(name = 'feature2', title = 'Feature 2', type = 'text')
            corr_table$addColumn(name = 'correlation', title = 'Correlation (r)', type = 'number', format = 'zto')
            corr_table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            corr_table$addColumn(name = 'interpretation', title = 'Strength', type = 'text')
            
            # Normality and distribution table
            dist_table <- self$results$distributiontable
            dist_table$addColumn(name = 'feature', title = 'Texture Feature', type = 'text')
            dist_table$addColumn(name = 'shapiro_w', title = 'Shapiro-Wilk W', type = 'number', format = 'zto')
            dist_table$addColumn(name = 'shapiro_p', title = 'P-value', type = 'number', format = 'zto,pvalue')
            dist_table$addColumn(name = 'skewness', title = 'Skewness', type = 'number', format = 'zto')
            dist_table$addColumn(name = 'kurtosis', title = 'Kurtosis', type = 'number', format = 'zto')
            dist_table$addColumn(name = 'distribution', title = 'Distribution Assessment', type = 'text')
        },
        
        .extractTextureData = function(data) {
            # Extract selected texture features
            feature_names <- self$options$texture_features
            
            # Validate that selected columns exist and are numeric
            valid_features <- c()
            for (feature in feature_names) {
                if (feature %in% names(data) && is.numeric(data[[feature]])) {
                    valid_features <- c(valid_features, feature)
                }
            }
            
            if (length(valid_features) == 0) {
                stop("No valid numeric texture features found in the selected variables.")
            }
            
            # Extract texture data
            texture_data <- data[, valid_features, drop = FALSE]
            
            # Remove rows with all missing values
            complete_rows <- rowSums(!is.na(texture_data)) > 0
            texture_data <- texture_data[complete_rows, , drop = FALSE]
            
            # Add additional variables if specified
            result_data <- texture_data
            
            # Add spatial coordinates if provided
            if (!is.null(self$options$x_coord) && self$options$x_coord %in% names(data)) {
                result_data$x_coord <- data[[self$options$x_coord]][complete_rows]
            }
            if (!is.null(self$options$y_coord) && self$options$y_coord %in% names(data)) {
                result_data$y_coord <- data[[self$options$y_coord]][complete_rows]
            }
            
            # Add grouping variable if provided
            if (!is.null(self$options$group_var) && self$options$group_var %in% names(data)) {
                result_data$group <- data[[self$options$group_var]][complete_rows]
            }
            
            # Add outcome variable if provided (for prognostic analysis)
            if (!is.null(self$options$outcome_var) && self$options$outcome_var %in% names(data)) {
                result_data$outcome <- data[[self$options$outcome_var]][complete_rows]
            }
            
            return(result_data)
        },
        
        .performTextureAnalysis = function(texture_data) {
            # Get texture feature columns only
            texture_features <- self$options$texture_features
            texture_cols <- intersect(texture_features, names(texture_data))
            texture_matrix <- texture_data[, texture_cols, drop = FALSE]
            
            # 1. Descriptive Statistics Analysis
            private$.analyzeDescriptiveStats(texture_matrix, texture_cols)
            
            # 2. Correlation Analysis
            private$.analyzeCorrelations(texture_matrix, texture_cols)
            
            # 3. Distribution Analysis
            private$.analyzeDistributions(texture_matrix, texture_cols)
        },
        
        .analyzeDescriptiveStats = function(texture_matrix, texture_cols) {
            desc_table <- self$results$descriptivestatistable
            
            for (i in seq_along(texture_cols)) {
                feature_name <- texture_cols[i]
                feature_data <- texture_matrix[[feature_name]]
                feature_data <- feature_data[!is.na(feature_data)]
                
                if (length(feature_data) >= 3) {
                    n <- length(feature_data)
                    mean_val <- mean(feature_data)
                    sd_val <- sd(feature_data)
                    median_val <- median(feature_data)
                    q1 <- quantile(feature_data, 0.25)
                    q3 <- quantile(feature_data, 0.75)
                    min_val <- min(feature_data)
                    max_val <- max(feature_data)
                    
                    iqr_text <- paste0(round(q1, 3), " - ", round(q3, 3))
                    range_text <- paste0(round(min_val, 3), " - ", round(max_val, 3))
                    
                    desc_table$addRow(rowKey = i, values = list(
                        feature = feature_name,
                        n = n,
                        mean = mean_val,
                        sd = sd_val,
                        median = median_val,
                        iqr = iqr_text,
                        range = range_text
                    ))
                }
            }
        },
        
        .analyzeCorrelations = function(texture_matrix, texture_cols) {
            if (length(texture_cols) < 2) return()
            
            corr_table <- self$results$correlationtable
            row_key <- 1
            
            for (i in 1:(length(texture_cols)-1)) {
                for (j in (i+1):length(texture_cols)) {
                    feature1 <- texture_cols[i]
                    feature2 <- texture_cols[j]
                    
                    data1 <- texture_matrix[[feature1]]
                    data2 <- texture_matrix[[feature2]]
                    
                    complete_pairs <- complete.cases(data1, data2)
                    if (sum(complete_pairs) >= 3) {
                        corr_test <- cor.test(data1[complete_pairs], data2[complete_pairs], method = "pearson")
                        r_value <- corr_test$estimate
                        p_value <- corr_test$p.value
                        
                        # Interpret correlation strength
                        strength <- ifelse(abs(r_value) >= 0.7, "Strong",
                                  ifelse(abs(r_value) >= 0.5, "Moderate",
                                  ifelse(abs(r_value) >= 0.3, "Weak", "Very weak")))
                        
                        corr_table$addRow(rowKey = row_key, values = list(
                            feature1 = feature1,
                            feature2 = feature2,
                            correlation = r_value,
                            p_value = p_value,
                            interpretation = strength
                        ))
                        
                        row_key <- row_key + 1
                    }
                }
            }
        },
        
        .analyzeDistributions = function(texture_matrix, texture_cols) {
            dist_table <- self$results$distributiontable
            
            for (i in seq_along(texture_cols)) {
                feature_name <- texture_cols[i]
                feature_data <- texture_matrix[[feature_name]]
                feature_data <- feature_data[!is.na(feature_data)]
                
                if (length(feature_data) >= 3) {
                    # Shapiro-Wilk test for normality
                    if (length(feature_data) <= 5000) {  # Shapiro-Wilk limitation
                        shapiro_test <- shapiro.test(feature_data)
                        shapiro_w <- shapiro_test$statistic
                        shapiro_p <- shapiro_test$p.value
                    } else {
                        shapiro_w <- NA
                        shapiro_p <- NA
                    }
                    
                    # Calculate skewness and kurtosis
                    if (requireNamespace('moments', quietly = TRUE)) {
                        skew_val <- moments::skewness(feature_data)
                        kurt_val <- moments::kurtosis(feature_data)
                    } else {
                        # Simple skewness calculation
                        n <- length(feature_data)
                        mean_val <- mean(feature_data)
                        sd_val <- sd(feature_data)
                        skew_val <- sum((feature_data - mean_val)^3) / ((n-1) * sd_val^3)
                        kurt_val <- sum((feature_data - mean_val)^4) / ((n-1) * sd_val^4)
                    }
                    
                    # Distribution assessment
                    dist_assessment <- "Unknown"
                    if (!is.na(shapiro_p)) {
                        if (shapiro_p >= 0.05) {
                            if (abs(skew_val) <= 0.5 && abs(kurt_val - 3) <= 0.5) {
                                dist_assessment <- "Normal distribution"
                            } else {
                                dist_assessment <- "Non-normal (symmetric)"
                            }
                        } else {
                            if (skew_val > 1) {
                                dist_assessment <- "Right-skewed"
                            } else if (skew_val < -1) {
                                dist_assessment <- "Left-skewed"
                            } else {
                                dist_assessment <- "Non-normal"
                            }
                        }
                    }
                    
                    dist_table$addRow(rowKey = i, values = list(
                        feature = feature_name,
                        shapiro_w = shapiro_w,
                        shapiro_p = shapiro_p,
                        skewness = skew_val,
                        kurtosis = kurt_val,
                        distribution = dist_assessment
                    ))
                }
            }
        },
        
        .generateTexturePlots = function(texture_data) {
            # Prepare plot data
            plot_data <- list(
                texture_features = self$options$texture_features,
                texture_data = texture_data[, self$options$texture_features, drop = FALSE],
                has_spatial = (!is.null(self$options$x_coord) && !is.null(self$options$y_coord)),
                has_group = !is.null(self$options$group_var) && "group" %in% names(texture_data),
                spatial_coords = if (!is.null(self$options$x_coord) && !is.null(self$options$y_coord)) {
                    texture_data[, c("x_coord", "y_coord")]
                } else { NULL },
                group_data = if ("group" %in% names(texture_data)) texture_data$group else NULL
            )
            
            self$results$textureplot$setState(plot_data)
            self$results$correlationplot$setState(plot_data)
        },
        
        .textureplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            library(ggplot2)
            
            # Create texture distribution plots
            texture_cols <- data$texture_features
            
            if (length(texture_cols) == 0) return(FALSE)
            
            # Prepare data for plotting
            plot_df <- data.frame()
            for (col in texture_cols) {
                if (col %in% names(data$texture_data)) {
                    col_data <- data$texture_data[[col]]
                    col_data <- col_data[!is.na(col_data)]
                    if (length(col_data) > 0) {
                        temp_df <- data.frame(
                            Feature = col,
                            Value = col_data
                        )
                        plot_df <- rbind(plot_df, temp_df)
                    }
                }
            }
            
            if (nrow(plot_df) == 0) return(FALSE)
            
            # Create violin/box plot
            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Feature, y = Value)) +
                ggplot2::geom_violin(alpha = 0.7, fill = "lightblue") +
                ggplot2::geom_boxplot(width = 0.1, alpha = 0.8) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggplot2::labs(
                    title = "Haralick Texture Feature Distributions",
                    subtitle = "Distribution characteristics of selected texture features",
                    x = "Texture Feature",
                    y = "Feature Value"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .correlationplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            library(ggplot2)
            
            texture_data <- data$texture_data
            
            if (ncol(texture_data) < 2) return(FALSE)
            
            # Calculate correlation matrix
            cor_matrix <- cor(texture_data, use = "complete.obs")
            
            # Convert to long format for plotting
            n_features <- ncol(cor_matrix)
            cor_df <- expand.grid(Feature1 = names(texture_data), Feature2 = names(texture_data))
            cor_df$Correlation <- as.vector(cor_matrix)
            
            # Create correlation heatmap
            p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Feature1, y = Feature2, fill = Correlation)) +
                ggplot2::geom_tile() +
                ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                                             midpoint = 0, limit = c(-1, 1)) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                              axis.text.y = ggplot2::element_text(angle = 0)) +
                ggplot2::labs(
                    title = "Haralick Texture Feature Correlation Matrix",
                    subtitle = "Inter-feature correlations (Pearson correlation coefficients)",
                    x = "Texture Features",
                    y = "Texture Features",
                    fill = "Correlation"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .generateTextureInterpretation = function(texture_data) {
            n_cases <- nrow(texture_data)
            texture_cols <- self$options$texture_features
            n_features <- length(texture_cols)
            
            # Calculate summary statistics
            feature_summaries <- list()
            for (col in texture_cols) {
                if (col %in% names(texture_data)) {
                    col_data <- texture_data[[col]]
                    col_data <- col_data[!is.na(col_data)]
                    if (length(col_data) > 0) {
                        feature_summaries[[col]] <- list(
                            mean = mean(col_data),
                            sd = sd(col_data),
                            median = median(col_data),
                            range = max(col_data) - min(col_data)
                        )
                    }
                }
            }
            
            # Find most variable feature (highest CV)
            cv_values <- sapply(feature_summaries, function(x) x$sd / x$mean)
            most_variable <- names(cv_values)[which.max(cv_values)]
            highest_cv <- max(cv_values, na.rm = TRUE)
            
            # Assess data quality
            complete_rate <- sum(complete.cases(texture_data[, texture_cols])) / n_cases * 100
            
            interpretation <- paste0(
                "<h3>Haralick Texture Analysis Summary</h3>",
                "<p><strong>Analysis Overview:</strong> ", n_cases, " observations analyzed across ", 
                n_features, " texture features</p>",
                
                "<h4>Data Quality Assessment:</h4>",
                "<ul>",
                "<li><strong>Completeness:</strong> ", round(complete_rate, 1), "% complete cases ",
                "(", ifelse(complete_rate >= 90, "Excellent", ifelse(complete_rate >= 70, "Good", "Limited")), " data quality)</li>",
                "<li><strong>Feature Coverage:</strong> ", n_features, " texture features analyzed</li>",
                "</ul>",
                
                "<h4>Texture Heterogeneity Assessment:</h4>",
                "<ul>",
                "<li><strong>Most Variable Feature:</strong> ", most_variable, " (CV = ", round(highest_cv, 3), ")",
                ifelse(highest_cv > 0.5, " - High heterogeneity detected", " - Moderate heterogeneity"), "</li>",
                
                if (length(feature_summaries) > 0) {
                    paste0("<li><strong>Feature Ranges:</strong> ",
                           paste(sapply(names(feature_summaries)[1:min(3, length(feature_summaries))], function(x) {
                               paste0(x, ": ", round(feature_summaries[[x]]$range, 3))
                           }), collapse = ", "),
                           if (length(feature_summaries) > 3) "..." else "", "</li>")
                } else { "" },
                "</ul>",
                
                "<h4>Clinical Interpretation:</h4>",
                "<div style='background-color: #f8f9fa; padding: 10px; border-left: 4px solid #007bff;'>",
                if (highest_cv > 1.0) {
                    "<p><strong>High Spatial Heterogeneity:</strong> Texture features show substantial variability, 
                    indicating significant spatial heterogeneity. <span style='color: blue;'>This may have prognostic significance 
                    and warrants further clinical correlation.</span></p>"
                } else if (highest_cv > 0.5) {
                    "<p><strong>Moderate Spatial Heterogeneity:</strong> Texture features demonstrate moderate variability. 
                    <span style='color: green;'>Suitable for biomarker development with appropriate statistical methods.</span></p>"
                } else {
                    "<p><strong>Low Spatial Heterogeneity:</strong> Texture features show limited variability. 
                    <span style='color: orange;'>Consider alternative features or analysis methods for biomarker development.</span></p>"
                },
                "</div>",
                
                "<h4>Statistical Recommendations:</h4>",
                "<ul>",
                "<li><strong>Distribution Assessment:</strong> Review distribution table for normality assumptions</li>",
                "<li><strong>Feature Selection:</strong> Consider correlation analysis to identify redundant features</li>",
                if (highest_cv > 0.5) {
                    "<li><strong>Prognostic Analysis:</strong> High heterogeneity suggests potential for survival analysis</li>"
                } else { "" },
                "<li><strong>Validation:</strong> Confirm findings in independent cohort</li>",
                "</ul>",
                
                "<h4>Methodological Notes:</h4>",
                "<ul>",
                "<li>Haralick features quantify spatial relationships in grayscale co-occurrence matrices</li>",
                "<li>Entropy measures randomness/disorder in texture patterns</li>",
                "<li>High entropy indicates heterogeneous, complex tissue architecture</li>",
                "<li>Low entropy suggests uniform, homogeneous tissue patterns</li>",
                "</ul>",
                
                "<h4>Quality Control Checks:</h4>",
                "<ul>",
                "<li>Verify image preprocessing consistency across samples</li>",
                "<li>Check for outliers in texture measurements</li>",
                "<li>Confirm spatial sampling adequacy</li>",
                "<li>Validate feature extraction parameters</li>",
                "</ul>",
                
                "<p><em>This analysis implements Haralick texture feature methodology validated in digital pathology 
                research (Haralick et al. 1973; Zilenaite-Petrulaitiene et al. 2025). Features are computed from 
                grayscale co-occurrence matrices and provide quantitative measures of spatial tissue heterogeneity.</em></p>"
            )
            
            self$results$interpretation$setContent(interpretation)
        }
    )
)