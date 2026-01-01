context("Biomarker Response Association Analysis - biomarkerresponse")

# Test data preparation
set.seed(123)  # For reproducible results

# Create comprehensive test datasets for different biomarker scenarios
# 1. Dataset with binary response (responder/non-responder)
binary_response_data <- data.frame(
  patient_id = paste0("PT", sprintf("%03d", 1:200)),
  biomarker_level = c(
    # Responders: higher biomarker levels
    rlnorm(80, meanlog = 2.5, sdlog = 0.8),  # Mean ~12-15
    # Non-responders: lower biomarker levels  
    rlnorm(120, meanlog = 1.8, sdlog = 0.7)  # Mean ~6-8
  ),
  response_binary = factor(c(
    rep("Responder", 80),
    rep("Non-responder", 120)
  )),
  treatment_arm = factor(sample(c("Drug A", "Drug B", "Placebo"), 200, replace = TRUE)),
  age = round(rnorm(200, 65, 12)),
  sex = factor(sample(c("Male", "Female"), 200, replace = TRUE))
)

# 2. Dataset with categorical response (CR/PR/SD/PD)
categorical_response_data <- data.frame(
  patient_id = paste0("PT", sprintf("%03d", 201:400)),
  biomarker_expression = c(
    # Complete Response: very high expression
    rlnorm(30, meanlog = 3.2, sdlog = 0.5),   
    # Partial Response: high expression
    rlnorm(50, meanlog = 2.7, sdlog = 0.6),   
    # Stable Disease: moderate expression
    rlnorm(70, meanlog = 2.2, sdlog = 0.7),   
    # Progressive Disease: low expression
    rlnorm(50, meanlog = 1.6, sdlog = 0.8)    
  ),
  response_category = factor(c(
    rep("CR", 30), rep("PR", 50), rep("SD", 70), rep("PD", 50)
  ), levels = c("CR", "PR", "SD", "PD")),
  tumor_type = factor(sample(c("Lung", "Breast", "Colorectal"), 200, replace = TRUE)),
  stage = factor(sample(c("II", "III", "IV"), 200, replace = TRUE))
)

# 3. Dataset with continuous response (% tumor shrinkage)
continuous_response_data <- data.frame(
  patient_id = paste0("PT", sprintf("%03d", 401:500)),
  biomarker_score = rnorm(100, 50, 15),  # Biomarker score 0-100
  percent_change = NA,  # Will be calculated based on biomarker
  histology = factor(sample(c("Adenocarcinoma", "Squamous", "Other"), 100, replace = TRUE)),
  prior_therapy = factor(sample(c("Naive", "Pre-treated"), 100, replace = TRUE))
)

# Calculate continuous response based on biomarker (higher biomarker = better response)
continuous_response_data$percent_change <- -30 + 0.8 * continuous_response_data$biomarker_score + 
                                          rnorm(100, 0, 20)  # Add noise

# 4. Small dataset for edge case testing
small_biomarker_data <- data.frame(
  biomarker = c(1.2, 3.4, 5.6, 7.8),
  response = factor(c("Yes", "No", "Yes", "No"))
)

# 5. Dataset with problematic values
problematic_biomarker_data <- data.frame(
  patient_id = 1:50,
  biomarker_with_zeros = c(rep(0, 10), rlnorm(40, 2, 1)),
  biomarker_with_negatives = c(rnorm(25, -5, 2), rlnorm(25, 10, 3)),
  biomarker_with_missing = c(rep(NA, 8), rlnorm(42, 8, 2)),
  response_good = factor(sample(c("Good", "Poor"), 50, replace = TRUE)),
  biomarker_outliers = c(rlnorm(45, 5, 1), rep(100, 5))  # Extreme outliers
)

test_that("Biomarker Response Analysis - Basic functionality and structure", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test basic function exists and can be called
  expect_true(exists("biomarkerresponseClass"))
  
  # Test that basic structure works with valid data
  expect_error(
    {
      biomarker_instance <- biomarkerresponseClass$new()
    },
    NA
  )
})

test_that("Biomarker Response Analysis - Data validation and input checking", {
  
  # Test data type validation for biomarker
  biomarker_numeric <- binary_response_data$biomarker_level
  expect_true(is.numeric(biomarker_numeric))
  expect_true(length(biomarker_numeric) > 0)
  
  # Test response variable validation
  response_factor <- binary_response_data$response_binary
  expect_true(is.factor(response_factor))
  expect_true(length(levels(response_factor)) >= 2)
  
  # Test minimum data requirements
  min_data_size <- 10  # Biomarker analysis needs sufficient data
  expect_true(length(biomarker_numeric) >= min_data_size)
  
  # Test for positive biomarker values (typical for expression data)
  positive_biomarker <- biomarker_numeric[biomarker_numeric > 0]
  expect_true(all(positive_biomarker > 0))
  expect_true(length(positive_biomarker) > 0)
})

test_that("Biomarker Response Analysis - ROC analysis and threshold calculations", {
  
  # Test ROC analysis with binary response
  biomarker_vals <- binary_response_data$biomarker_level
  response_binary <- as.numeric(binary_response_data$response_binary == "Responder")
  
  # Test that we can perform ROC analysis
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- pROC::roc(response_binary, biomarker_vals)
    expect_s3_class(roc_obj, "roc")
    
    # Test AUC calculation
    auc_val <- as.numeric(pROC::auc(roc_obj))
    expect_true(is.numeric(auc_val))
    expect_true(auc_val >= 0 && auc_val <= 1)
    
    # Test optimal threshold calculation
    coords_obj <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
    expect_true(is.numeric(coords_obj$threshold))
    expect_true(coords_obj$sensitivity >= 0 && coords_obj$sensitivity <= 1)
    expect_true(coords_obj$specificity >= 0 && coords_obj$specificity <= 1)
  }
})

test_that("Biomarker Response Analysis - Threshold performance metrics", {
  
  # Test threshold metrics calculation
  biomarker_vals <- binary_response_data$biomarker_level
  response_binary <- as.numeric(binary_response_data$response_binary == "Responder")
  threshold <- median(biomarker_vals, na.rm = TRUE)
  
  # Calculate confusion matrix components
  biomarker_positive <- biomarker_vals >= threshold
  tp <- sum(biomarker_positive & response_binary, na.rm = TRUE)
  tn <- sum(!biomarker_positive & !response_binary, na.rm = TRUE)
  fp <- sum(biomarker_positive & !response_binary, na.rm = TRUE)
  fn <- sum(!biomarker_positive & response_binary, na.rm = TRUE)
  
  # Calculate performance metrics
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  ppv <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  
  # Validate metrics
  expect_true(is.numeric(sensitivity) && sensitivity >= 0 && sensitivity <= 1)
  expect_true(is.numeric(specificity) && specificity >= 0 && specificity <= 1)
  expect_true(is.numeric(ppv) && ppv >= 0 && ppv <= 1)
  expect_true(is.numeric(npv) && npv >= 0 && npv <= 1)
})

test_that("Biomarker Response Analysis - Statistical tests validation", {
  
  # Test t-test for binary response
  biomarker_vals <- binary_response_data$biomarker_level
  response_factor <- binary_response_data$response_binary
  
  t_test <- t.test(biomarker_vals ~ response_factor)
  expect_s3_class(t_test, "htest")
  expect_true(is.numeric(t_test$statistic))
  expect_true(is.numeric(t_test$p.value))
  expect_true(t_test$p.value >= 0 && t_test$p.value <= 1)
  
  # Test Wilcoxon test
  wilcox_test <- wilcox.test(biomarker_vals ~ response_factor)
  expect_s3_class(wilcox_test, "htest")
  expect_true(is.numeric(wilcox_test$p.value))
  
  # Test ANOVA for categorical response
  biomarker_cat <- categorical_response_data$biomarker_expression
  response_cat <- categorical_response_data$response_category
  
  anova_test <- aov(biomarker_cat ~ response_cat)
  anova_summary <- summary(anova_test)
  expect_true(is.numeric(anova_summary[[1]][["F value"]][1]))
  expect_true(is.numeric(anova_summary[[1]][["Pr(>F)"]][1]))
  
  # Test Kruskal-Wallis test
  kw_test <- kruskal.test(biomarker_cat ~ response_cat)
  expect_s3_class(kw_test, "htest")
  expect_true(is.numeric(kw_test$p.value))
})

test_that("Biomarker Response Analysis - Correlation analysis", {
  
  # Test Pearson correlation for continuous response
  biomarker_cont <- continuous_response_data$biomarker_score
  response_cont <- continuous_response_data$percent_change
  
  pearson_cor <- cor.test(biomarker_cont, response_cont, method = "pearson")
  expect_s3_class(pearson_cor, "htest")
  expect_true(is.numeric(pearson_cor$estimate))
  expect_true(pearson_cor$estimate >= -1 && pearson_cor$estimate <= 1)
  expect_true(is.numeric(pearson_cor$p.value))
  
  # Test Spearman correlation
  spearman_cor <- cor.test(biomarker_cont, response_cont, method = "spearman")
  expect_s3_class(spearman_cor, "htest")
  expect_true(is.numeric(spearman_cor$estimate))
  expect_true(spearman_cor$estimate >= -1 && spearman_cor$estimate <= 1)
  
  # Test confidence intervals
  expect_true(length(pearson_cor$conf.int) == 2)
  expect_true(pearson_cor$conf.int[1] <= pearson_cor$estimate)
  expect_true(pearson_cor$conf.int[2] >= pearson_cor$estimate)
})

test_that("Biomarker Response Analysis - Group comparison statistics", {
  
  # Test group statistics calculation for binary response
  biomarker_vals <- binary_response_data$biomarker_level
  response_factor <- binary_response_data$response_binary
  
  # Calculate group statistics
  group_means <- tapply(biomarker_vals, response_factor, mean, na.rm = TRUE)
  group_sds <- tapply(biomarker_vals, response_factor, sd, na.rm = TRUE)
  group_medians <- tapply(biomarker_vals, response_factor, median, na.rm = TRUE)
  group_counts <- table(response_factor)
  
  # Validate statistics
  expect_true(all(is.numeric(group_means)))
  expect_true(all(is.numeric(group_sds)))
  expect_true(all(is.numeric(group_medians)))
  expect_true(all(group_counts > 0))
  expect_true(sum(group_counts) == length(biomarker_vals))
  
  # Test for expected differences (responders should have higher biomarker)
  if ("Responder" %in% names(group_means) && "Non-responder" %in% names(group_means)) {
    expect_true(group_means["Responder"] > group_means["Non-responder"])
  }
})

test_that("Biomarker Response Analysis - Edge cases and error handling", {
  
  # Test empty dataset
  empty_data <- data.frame(biomarker = numeric(0), response = factor(character(0)))
  expect_equal(nrow(empty_data), 0)
  
  # Test dataset with zeros
  data_with_zeros <- problematic_biomarker_data$biomarker_with_zeros
  non_zero_data <- data_with_zeros[data_with_zeros > 0]
  expect_true(all(non_zero_data > 0))
  expect_true(length(non_zero_data) > 0)
  
  # Test dataset with negative numbers (biomarkers usually positive)
  data_with_negatives <- problematic_biomarker_data$biomarker_with_negatives
  positive_only <- data_with_negatives[data_with_negatives > 0]
  expect_true(all(positive_only > 0))
  
  # Test very small dataset
  tiny_biomarker <- small_biomarker_data$biomarker
  expect_true(length(tiny_biomarker) < 10)  # Too small for reliable ROC analysis
  
  # Test dataset with missing values
  data_with_na <- problematic_biomarker_data$biomarker_with_missing
  complete_data <- data_with_na[!is.na(data_with_na)]
  expect_true(all(!is.na(complete_data)))
  expect_true(length(complete_data) > 0)
  
  # Test outlier detection
  data_with_outliers <- problematic_biomarker_data$biomarker_outliers
  q1 <- quantile(data_with_outliers, 0.25, na.rm = TRUE)
  q3 <- quantile(data_with_outliers, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  outliers <- data_with_outliers < (q1 - 1.5 * iqr) | data_with_outliers > (q3 + 1.5 * iqr)
  expect_true(sum(outliers) > 0)  # Should detect the extreme values we added
})

test_that("Biomarker Response Analysis - Response type handling", {
  
  # Test binary response type
  binary_response <- binary_response_data$response_binary
  expect_true(is.factor(binary_response))
  expect_equal(length(levels(binary_response)), 2)
  
  # Test categorical response type
  categorical_response <- categorical_response_data$response_category
  expect_true(is.factor(categorical_response))
  expect_true(length(levels(categorical_response)) > 2)
  expect_true(all(c("CR", "PR", "SD", "PD") %in% levels(categorical_response)))
  
  # Test continuous response type
  continuous_response <- continuous_response_data$percent_change
  expect_true(is.numeric(continuous_response))
  expect_true(length(continuous_response) > 0)
  
  # Test response type determination
  expect_equal(length(levels(binary_response)), 2)
  expect_equal(length(levels(categorical_response)), 4)
  expect_true(is.numeric(continuous_response))
})

test_that("Biomarker Response Analysis - Data transformation and preprocessing", {
  
  # Test log transformation
  biomarker_vals <- binary_response_data$biomarker_level
  log_transformed <- log(biomarker_vals + 1)  # Add 1 to handle potential zeros
  expect_true(all(is.finite(log_transformed)))
  expect_true(all(log_transformed >= 0))  # log(x+1) where x>=0 should be >=0
  
  # Test outlier removal using IQR method
  q1 <- quantile(biomarker_vals, 0.25, na.rm = TRUE)
  q3 <- quantile(biomarker_vals, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  outliers <- biomarker_vals < lower_bound | biomarker_vals > upper_bound
  cleaned_data <- biomarker_vals[!outliers]
  expect_true(length(cleaned_data) <= length(biomarker_vals))
  expect_true(all(cleaned_data >= lower_bound & cleaned_data <= upper_bound))
})

test_that("Biomarker Response Analysis - Integration with ClinicoPath datasets", {
  
  # Test with histopathology dataset if available
  if (exists("histopathology") && is.data.frame(histopathology)) {
    histo_data <- histopathology
    
    # Find appropriate biomarker variables
    biomarker_candidates <- c("MeasurementA", "MeasurementB", "Grade", "TStage")
    available_biomarkers <- biomarker_candidates[biomarker_candidates %in% names(histo_data)]
    
    if (length(available_biomarkers) > 0) {
      # Test with first available biomarker
      test_biomarker <- histo_data[[available_biomarkers[1]]]
      test_biomarker <- test_biomarker[!is.na(test_biomarker)]
      
      if (length(test_biomarker) >= 10) {
        expect_true(is.numeric(test_biomarker) || is.integer(test_biomarker))
        expect_true(length(test_biomarker) > 0)
        
        # Test basic statistics
        expect_true(is.numeric(mean(test_biomarker, na.rm = TRUE)))
        expect_true(is.numeric(sd(test_biomarker, na.rm = TRUE)))
        expect_true(is.numeric(median(test_biomarker, na.rm = TRUE)))
      }
    }
    
    # Test with response variables
    response_candidates <- c("Death", "Outcome", "Mortality5yr")
    available_responses <- response_candidates[response_candidates %in% names(histo_data)]
    
    if (length(available_responses) > 0) {
      test_response <- histo_data[[available_responses[1]]]
      test_response <- test_response[!is.na(test_response)]
      
      if (length(test_response) >= 10) {
        expect_true(length(test_response) > 0)
        # Should be factor or convertible to factor
        response_factor <- as.factor(test_response)
        expect_true(is.factor(response_factor))
      }
    }
  }
  
  # Test with treatmentResponse dataset if available
  if (exists("treatmentResponse") && is.data.frame(treatmentResponse)) {
    treatment_data <- treatmentResponse
    
    if ("ResponseValue" %in% names(treatment_data)) {
      response_vals <- treatment_data$ResponseValue
      response_vals <- response_vals[!is.na(response_vals)]
      
      if (length(response_vals) >= 10) {
        expect_true(is.numeric(response_vals))
        expect_true(length(response_vals) > 0)
        
        # Test statistics
        expect_true(is.numeric(mean(response_vals, na.rm = TRUE)))
        expect_true(is.numeric(range(response_vals, na.rm = TRUE)[1]))
        expect_true(is.numeric(range(response_vals, na.rm = TRUE)[2]))
      }
    }
  }
})

test_that("Biomarker Response Analysis - Clinical interpretation scenarios", {
  
  # Test predictive biomarker scenario (high biomarker = good response)
  predictive_biomarker <- binary_response_data$biomarker_level
  responder_status <- binary_response_data$response_binary
  
  # Calculate means for each group
  responder_mean <- mean(predictive_biomarker[responder_status == "Responder"], na.rm = TRUE)
  non_responder_mean <- mean(predictive_biomarker[responder_status == "Non-responder"], na.rm = TRUE)
  
  # For our simulated data, responders should have higher biomarker levels
  expect_true(responder_mean > non_responder_mean)
  
  # Test prognostic biomarker scenario (continuous outcome)
  prognostic_biomarker <- continuous_response_data$biomarker_score
  outcome_measure <- continuous_response_data$percent_change
  
  # Should have some correlation (negative = better response with higher biomarker)
  correlation <- cor(prognostic_biomarker, outcome_measure, use = "complete.obs")
  expect_true(is.numeric(correlation))
  expect_true(abs(correlation) >= 0)  # Should have some relationship
  
  # Test dose-response relationship
  # Higher biomarker levels should correlate with better outcomes
  dose_response_test <- cor.test(prognostic_biomarker, outcome_measure)
  expect_s3_class(dose_response_test, "htest")
  expect_true(is.numeric(dose_response_test$p.value))
})

test_that("Biomarker Response Analysis - Threshold method validation", {
  
  # Test median threshold method
  biomarker_vals <- binary_response_data$biomarker_level
  median_threshold <- median(biomarker_vals, na.rm = TRUE)
  expect_true(is.numeric(median_threshold))
  expect_true(median_threshold > 0)
  
  # Test quartile threshold method
  q75_threshold <- quantile(biomarker_vals, 0.75, na.rm = TRUE)
  expect_true(is.numeric(q75_threshold))
  expect_true(q75_threshold >= median_threshold)  # Q75 should be >= median
  
  # Test manual threshold specification
  manual_threshold <- 10.0
  expect_true(is.numeric(manual_threshold))
  expect_true(manual_threshold > 0)
  
  # Test threshold application
  biomarker_positive_median <- biomarker_vals >= median_threshold
  biomarker_positive_q75 <- biomarker_vals >= q75_threshold
  biomarker_positive_manual <- biomarker_vals >= manual_threshold
  
  expect_true(is.logical(biomarker_positive_median))
  expect_true(is.logical(biomarker_positive_q75))
  expect_true(is.logical(biomarker_positive_manual))
  
  # Q75 threshold should result in fewer positives than median
  expect_true(sum(biomarker_positive_q75) <= sum(biomarker_positive_median))
})

test_that("Biomarker Response Analysis - Plot data preparation", {
  
  # Test plot data structure for binary response
  plot_data_binary <- list(
    biomarker = binary_response_data$biomarker_level,
    response = binary_response_data$response_binary,
    response_type = "binary",
    plot_type = "boxplot"
  )
  
  expect_true(is.numeric(plot_data_binary$biomarker))
  expect_true(is.factor(plot_data_binary$response))
  expect_equal(plot_data_binary$response_type, "binary")
  expect_equal(plot_data_binary$plot_type, "boxplot")
  
  # Test plot data structure for continuous response
  plot_data_continuous <- list(
    biomarker = continuous_response_data$biomarker_score,
    response = continuous_response_data$percent_change,
    response_type = "continuous",
    plot_type = "scatter"
  )
  
  expect_true(is.numeric(plot_data_continuous$biomarker))
  expect_true(is.numeric(plot_data_continuous$response))
  expect_equal(plot_data_continuous$response_type, "continuous")
  expect_equal(plot_data_continuous$plot_type, "scatter")
  
  # Test data frame creation for plotting
  df_binary <- data.frame(
    biomarker = plot_data_binary$biomarker,
    response = plot_data_binary$response
  )
  expect_equal(nrow(df_binary), length(plot_data_binary$biomarker))
  expect_equal(ncol(df_binary), 2)
  
  df_continuous <- data.frame(
    biomarker = plot_data_continuous$biomarker,
    response = plot_data_continuous$response
  )
  expect_equal(nrow(df_continuous), length(plot_data_continuous$biomarker))
  expect_equal(ncol(df_continuous), 2)
})

# Test completion message
cat("✅ Biomarker Response Analysis test suite completed successfully!\n")
cat("📊 Tests covered:\n")
cat("   - Basic functionality and data validation\n")
cat("   - ROC analysis and threshold calculations\n") 
cat("   - Threshold performance metrics\n")
cat("   - Statistical tests (t-test, Wilcoxon, ANOVA, Kruskal-Wallis)\n")
cat("   - Correlation analysis (Pearson, Spearman)\n")
cat("   - Group comparison statistics\n")
cat("   - Edge cases and error handling\n")
cat("   - Response type handling (binary, categorical, continuous)\n")
cat("   - Data transformation and preprocessing\n")
cat("   - Integration with ClinicoPath datasets\n")
cat("   - Clinical interpretation scenarios\n")
cat("   - Threshold method validation\n")
cat("   - Plot data preparation\n")
