test_that("classification produces valid outputs with decision tree", {
    # Load test data
    data("BreastCancer", package = "ClinicoPath")

    # Run basic classification
    results <- ClinicoPath::classification(
        data = BreastCancer,
        dep = "Class",
        indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
        classifier = "singleDecisionTree",
        testing = "split",
        testSize = 0.3,
        reporting = c("confusionMatrix", "classifMetrices", "AUC"),
        reportClinicalMetrics = TRUE
    )

    # Test that results object exists
    testthat::expect_true(!is.null(results))

    # Test that confusion matrix exists
    conf_matrix <- results$confusion$matrix$asDF
    testthat::expect_true(nrow(conf_matrix) > 0)
    testthat::expect_true(all(c("class") %in% names(conf_matrix)))

    # Test that general metrics exist and are in valid range
    general_metrics <- results$classificationMetrics$general$asDF
    testthat::expect_true(nrow(general_metrics) > 0)

    # Check that accuracy is between 0 and 1
    accuracy_row <- general_metrics[general_metrics$metric == "classif.acc", ]
    if (nrow(accuracy_row) > 0) {
        testthat::expect_true(accuracy_row$value >= 0 && accuracy_row$value <= 1)
    }

    # Test that clinical metrics are calculated for binary classification
    clinical_metrics <- results$classificationMetrics$clinicalMetrics$asDF
    testthat::expect_true(nrow(clinical_metrics) > 0)

    # Check that sensitivity and specificity are in valid range [0, 1]
    sens_row <- clinical_metrics[clinical_metrics$metric == "Sensitivity", ]
    if (nrow(sens_row) > 0) {
        testthat::expect_true(sens_row$value >= 0 && sens_row$value <= 1,
                            info = "Sensitivity must be between 0 and 1")
    }

    spec_row <- clinical_metrics[clinical_metrics$metric == "Specificity", ]
    if (nrow(spec_row) > 0) {
        testthat::expect_true(spec_row$value >= 0 && spec_row$value <= 1,
                            info = "Specificity must be between 0 and 1")
    }
})

test_that("classification handles positive class specification correctly", {
    data("BreastCancer", package = "ClinicoPath")

    # Get unique classes
    classes <- unique(BreastCancer$Class)

    # Test with first class as positive
    results1 <- ClinicoPath::classification(
        data = BreastCancer,
        dep = "Class",
        indep = c("Cl.thickness", "Cell.size"),
        classifier = "singleDecisionTree",
        testing = "split",
        testSize = 0.3,
        reportClinicalMetrics = TRUE,
        positiveClass = as.character(classes[1])
    )

    # Test with second class as positive
    results2 <- ClinicoPath::classification(
        data = BreastCancer,
        dep = "Class",
        indep = c("Cl.thickness", "Cell.size"),
        classifier = "singleDecisionTree",
        testing = "split",
        testSize = 0.3,
        reportClinicalMetrics = TRUE,
        positiveClass = as.character(classes[2])
    )

    # Both should produce valid results
    testthat::expect_true(!is.null(results1))
    testthat::expect_true(!is.null(results2))

    # Clinical metrics should exist for both
    metrics1 <- results1$classificationMetrics$clinicalMetrics$asDF
    metrics2 <- results2$classificationMetrics$clinicalMetrics$asDF

    testthat::expect_true(nrow(metrics1) > 0)
    testthat::expect_true(nrow(metrics2) > 0)

    # Sensitivity and specificity should be swapped between the two runs
    # (what's sensitivity for class A is specificity for class B)
    sens1 <- metrics1[metrics1$metric == "Sensitivity", "value"]
    spec1 <- metrics1[metrics1$metric == "Specificity", "value"]
    sens2 <- metrics2[metrics2$metric == "Sensitivity", "value"]
    spec2 <- metrics2[metrics2$metric == "Specificity", "value"]

    if (length(sens1) > 0 && length(spec1) > 0 && length(sens2) > 0 && length(spec2) > 0) {
        # Allow small numerical tolerance
        testthat::expect_true(abs(sens1 - spec2) < 0.01,
                            info = "Sensitivity with class A positive should equal specificity with class B positive")
        testthat::expect_true(abs(spec1 - sens2) < 0.01,
                            info = "Specificity with class A positive should equal sensitivity with class B positive")
    }
})

test_that("classification ROC curve produces valid AUC", {
    data("BreastCancer", package = "ClinicoPath")

    results <- ClinicoPath::classification(
        data = BreastCancer,
        dep = "Class",
        indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
        classifier = "logisticRegression",
        testing = "split",
        testSize = 0.3,
        reporting = c("AUC")
    )

    # Check that AUC is reported in class metrics
    class_metrics <- results$classificationMetrics$class$asDF

    # Find AUC value
    auc_col <- grep("auc", names(class_metrics), ignore.case = TRUE, value = TRUE)
    if (length(auc_col) > 0) {
        auc_values <- class_metrics[[auc_col[1]]]
        auc_values <- auc_values[!is.na(auc_values)]

        if (length(auc_values) > 0) {
            # AUC should be between 0 and 1
            testthat::expect_true(all(auc_values >= 0 && auc_values <= 1),
                                info = "AUC must be between 0 and 1")

            # For a reasonable classifier on BreastCancer data, AUC should be > 0.5
            testthat::expect_true(any(auc_values > 0.5),
                                info = "AUC should be better than random (>0.5) on BreastCancer data")
        }
    }
})

test_that("classification handles SMOTE fallback correctly", {
    data("BreastCancer", package = "ClinicoPath")

    # Create imbalanced data
    imbalanced_data <- BreastCancer[c(1:50, 400:420), ]  # Very imbalanced

    # Test SMOTE option (should fall back to upsampling if smotefamily not installed)
    testthat::expect_warning(
        results <- ClinicoPath::classification(
            data = imbalanced_data,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            balancingMethod = "smote",
            testing = "split",
            testSize = 0.3
        ),
        regexp = "SMOTE|upsample|Falling back",
        info = "Should warn if SMOTE package is not available"
    )

    # Analysis should still complete
    testthat::expect_true(!is.null(results))
})

test_that("classification confusion matrix values are valid", {
    data("BreastCancer", package = "ClinicoPath")

    results <- ClinicoPath::classification(
        data = BreastCancer,
        dep = "Class",
        indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
        classifier = "singleDecisionTree",
        testing = "split",
        testSize = 0.3,
        reporting = c("confusionMatrix")
    )

    conf_matrix <- results$confusion$matrix$asDF

    # Confusion matrix should have at least 2 rows (for binary classification)
    testthat::expect_true(nrow(conf_matrix) >= 2)

    # All predicted values should be non-negative integers
    pred_cols <- grep("pred_", names(conf_matrix), value = TRUE)
    for (col in pred_cols) {
        values <- conf_matrix[[col]]
        testthat::expect_true(all(values >= 0),
                            info = paste("All values in", col, "should be non-negative"))
        testthat::expect_true(all(values == floor(values)),
                            info = paste("All values in", col, "should be integers"))
    }

    # Sum of confusion matrix should equal test set size
    total_predictions <- sum(unlist(conf_matrix[, pred_cols]))
    test_size <- round(nrow(BreastCancer) * 0.3)
    testthat::expect_true(abs(total_predictions - test_size) < 50,
                        info = "Total predictions should approximately equal test set size")
})

test_that("classification with confidence intervals produces valid CIs", {
    data("BreastCancer", package = "ClinicoPath")

    results <- ClinicoPath::classification(
        data = BreastCancer,
        dep = "Class",
        indep = c("Cl.thickness", "Cell.size"),
        classifier = "singleDecisionTree",
        testing = "split",
        testSize = 0.3,
        reportClinicalMetrics = TRUE,
        reportConfidenceIntervals = TRUE,
        bootstrapSamples = 100  # Low for testing speed
    )

    clinical_metrics <- results$classificationMetrics$clinicalMetrics$asDF

    # Check for CI columns
    ci_cols <- grep("ci_", names(clinical_metrics), value = TRUE)

    if (length(ci_cols) > 0) {
        # CI lower should be <= point estimate
        # CI upper should be >= point estimate
        for (metric in c("Sensitivity", "Specificity", "PPV", "NPV")) {
            metric_row <- clinical_metrics[clinical_metrics$metric == metric, ]

            if (nrow(metric_row) > 0) {
                value <- metric_row$value

                if ("ci_lower" %in% names(metric_row)) {
                    ci_lower <- metric_row$ci_lower
                    if (!is.na(ci_lower) && !is.na(value)) {
                        testthat::expect_true(ci_lower <= value,
                                            info = paste(metric, "CI lower should be <= point estimate"))
                    }
                }

                if ("ci_upper" %in% names(metric_row)) {
                    ci_upper <- metric_row$ci_upper
                    if (!is.na(ci_upper) && !is.na(value)) {
                        testthat::expect_true(ci_upper >= value,
                                            info = paste(metric, "CI upper should be >= point estimate"))
                    }
                }
            }
        }
    }
})

test_that("classification Matthews Correlation Coefficient is valid", {
    data("BreastCancer", package = "ClinicoPath")

    results <- ClinicoPath::classification(
        data = BreastCancer,
        dep = "Class",
        indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
        classifier = "logisticRegression",
        testing = "split",
        testSize = 0.3,
        reportMCC = TRUE
    )

    mcc_table <- results$classificationMetrics$mccTable$asDF

    if (nrow(mcc_table) > 0) {
        mcc_value <- mcc_table$mcc[1]

        # MCC should be between -1 and 1
        testthat::expect_true(mcc_value >= -1 && mcc_value <= 1,
                            info = "MCC must be between -1 and 1")

        # For a reasonable classifier, MCC should be > 0
        testthat::expect_true(mcc_value > 0,
                            info = "MCC should be positive for a reasonable classifier")

        # Check that interpretation exists
        testthat::expect_true("interpretation" %in% names(mcc_table))
        testthat::expect_true(nchar(mcc_table$interpretation[1]) > 0)
    }
})

test_that("classification cross-validation produces stable results", {
    data("BreastCancer", package = "ClinicoPath")

    # Use a smaller sample for faster testing
    small_data <- BreastCancer[1:200, ]

    results <- ClinicoPath::classification(
        data = small_data,
        dep = "Class",
        indep = c("Cl.thickness", "Cell.size"),
        classifier = "singleDecisionTree",
        testing = "crossValidation",
        noOfFolds = 5,
        reportClinicalMetrics = TRUE
    )

    # Check that results exist
    testthat::expect_true(!is.null(results))

    # Check general metrics
    general_metrics <- results$classificationMetrics$general$asDF
    testthat::expect_true(nrow(general_metrics) > 0)

    # Accuracy should be reasonable for cross-validation
    accuracy_row <- general_metrics[general_metrics$metric == "classif.acc", ]
    if (nrow(accuracy_row) > 0) {
        # For BreastCancer data with reasonable features, accuracy should be > 0.6
        testthat::expect_true(accuracy_row$value > 0.6,
                            info = "Cross-validation accuracy should be reasonable")
    }
})
