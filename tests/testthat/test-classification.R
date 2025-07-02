test_that("classification works", {
    
    # Load test data
    data("BreastCancer", package = "ClinicoPath")
    data("histopathology", package = "ClinicoPath")
    
    # Test 1: Basic functionality with decision tree
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
            classifier = "singleDecisionTree",
            testing = "split",
            testSize = 0.3,
            reporting = c("confusionMatrix", "classifMetrices"),
            complexity = 0.01,
            maxDepth = 10
        )
    })
    
    # Test 2: Random Forest classifier
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size", "Cell.shape", "Marg.adhesion"),
            classifier = "randomForest",
            testing = "crossValidation",
            noOfFolds = 5,
            noOfTrees = 50,
            reporting = c("confusionMatrix", "AUC")
        )
    })
    
    # Test 3: KNN classifier
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
            classifier = "knn",
            knnNeighbors = 5,
            knnDistance = "euclidean",
            testing = "split",
            testSize = 0.2
        )
    })
    
    # Test 4: Logistic Regression
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "logisticRegression",
            testing = "crossValidation",
            noOfFolds = 3
        )
    })
    
    # Test 5: Naive Bayes
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
            classifier = "naiveBayes",
            testing = "split",
            testSize = 0.25
        )
    })
    
    # Test 6: SVM classifier
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "svm",
            svmKernel = "radial",
            svmCost = 1.0,
            svmGamma = 1.0,
            testing = "split"
        )
    })
    
    # Test 7: Clinical metrics with confidence intervals
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
            classifier = "singleDecisionTree",
            testing = "split",
            reportClinicalMetrics = TRUE,
            reportConfidenceIntervals = TRUE,
            bootstrapSamples = 100
        )
    })
    
    # Test 8: Class imbalance handling - upsampling
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            balancingMethod = "upsample",
            testing = "split"
        )
    })
    
    # Test 9: Class imbalance handling - downsampling
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "randomForest",
            balancingMethod = "downsample",
            testing = "split"
        )
    })
    
    # Test 10: Different validation methods
    validation_methods <- c("holdout", "bootstrap", "cv")
    
    for (method in validation_methods) {
        testthat::expect_silent({
            results <- ClinicoPath::classification(
                data = BreastCancer,
                dep = "Class",
                indep = c("Cl.thickness", "Cell.size"),
                classifier = "singleDecisionTree",
                validateMethod = method,
                bootstrapSamples = 50  # Keep low for testing
            )
        })
    }
})

test_that("classification handles edge cases", {
    
    # Load test data
    data("BreastCancer", package = "ClinicoPath")
    
    # Test 1: Empty variable selection should not crash
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer
        )
    })
    
    # Test 2: Single predictor
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = "Cl.thickness",
            classifier = "singleDecisionTree",
            testing = "split"
        )
    })
    
    # Test 3: Small dataset
    small_data <- BreastCancer[1:20, ]
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = small_data,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            testing = "split",
            testSize = 0.5
        )
    })
    
    # Test 4: Different clinical cutoffs
    cutoffs <- c(0.3, 0.5, 0.7)
    
    for (cutoff in cutoffs) {
        testthat::expect_silent({
            results <- ClinicoPath::classification(
                data = BreastCancer,
                dep = "Class",
                indep = c("Cl.thickness", "Cell.size"),
                classifier = "singleDecisionTree",
                clinicalCutoff = cutoff,
                testing = "split"
            )
        })
    }
    
    # Test 5: Missing data handling
    test_data <- BreastCancer[1:50, ]
    test_data$Cl.thickness[1:5] <- NA
    test_data$Cell.size[6:10] <- NA
    
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = test_data,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            testing = "split"
        )
    })
})

test_that("classification parameter validation", {
    
    data("BreastCancer", package = "ClinicoPath")
    
    # Test different tree parameters
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            minSplit = 10,
            complexity = 0.001,
            maxCompete = 2,
            maxSurrogate = 3,
            maxDepth = 5,
            testing = "split"
        )
    })
    
    # Test different random forest parameters
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
            classifier = "randomForest",
            noOfTrees = 25,
            sampleFraction = 0.8,
            splitRule = "gini",
            testing = "split"
        )
    })
    
    # Test different KNN parameters
    knn_distances <- c("euclidean", "manhattan", "minkowski")
    
    for (distance in knn_distances) {
        testthat::expect_silent({
            results <- ClinicoPath::classification(
                data = BreastCancer,
                dep = "Class",
                indep = c("Cl.thickness", "Cell.size"),
                classifier = "knn",
                knnNeighbors = 3,
                knnDistance = distance,
                testing = "split"
            )
        })
    }
    
    # Test different SVM kernels
    svm_kernels <- c("linear", "radial", "polynomial")
    
    for (kernel in svm_kernels) {
        testthat::expect_silent({
            results <- ClinicoPath::classification(
                data = BreastCancer,
                dep = "Class",
                indep = c("Cl.thickness", "Cell.size"),
                classifier = "svm",
                svmKernel = kernel,
                svmCost = 0.5,
                svmGamma = 0.5,
                testing = "split"
            )
        })
    }
})

test_that("classification reporting options", {
    
    data("BreastCancer", package = "ClinicoPath")
    
    # Test all reporting combinations
    reporting_options <- list(
        c("confusionMatrix"),
        c("classifMetrices"),
        c("AUC"),
        c("confusionMatrix", "classifMetrices"),
        c("confusionMatrix", "AUC"),
        c("classifMetrices", "AUC"),
        c("confusionMatrix", "classifMetrices", "AUC")
    )
    
    for (options in reporting_options) {
        testthat::expect_silent({
            results <- ClinicoPath::classification(
                data = BreastCancer,
                dep = "Class",
                indep = c("Cl.thickness", "Cell.size"),
                classifier = "singleDecisionTree",
                reporting = options,
                testing = "split"
            )
        })
    }
    
    # Test clinical metrics reporting
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            reportClinicalMetrics = TRUE,
            reportConfidenceIntervals = FALSE,
            testing = "split"
        )
    })
    
    # Test with confidence intervals
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            reportClinicalMetrics = TRUE,
            reportConfidenceIntervals = TRUE,
            bootstrapSamples = 50,
            testing = "split"
        )
    })
})

test_that("classification plotting functionality", {
    
    data("BreastCancer", package = "ClinicoPath")
    
    # Test decision tree plotting
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            plotDecisionTree = TRUE,
            testing = "split"
        )
    })
    
    # Test predicted frequencies
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            predictedFreq = TRUE,
            testing = "split"
        )
    })
    
    # Test random forest model printing
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "randomForest",
            printRandForest = TRUE,
            testing = "split"
        )
    })
    
    # Test random forest predicted frequencies
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "randomForest",
            predictedFreqRF = TRUE,
            testing = "split"
        )
    })
})

test_that("classification with different test sizes", {
    
    data("BreastCancer", package = "ClinicoPath")
    
    # Test different test sizes
    test_sizes <- c(0.1, 0.2, 0.3, 0.4, 0.5)
    
    for (size in test_sizes) {
        testthat::expect_silent({
            results <- ClinicoPath::classification(
                data = BreastCancer,
                dep = "Class",
                indep = c("Cl.thickness", "Cell.size"),
                classifier = "singleDecisionTree",
                testing = "split",
                testSize = size
            )
        })
    }
    
    # Test different cross-validation fold numbers
    fold_numbers <- c(3, 5, 10)
    
    for (folds in fold_numbers) {
        testthat::expect_silent({
            results <- ClinicoPath::classification(
                data = BreastCancer,
                dep = "Class",
                indep = c("Cl.thickness", "Cell.size"),
                classifier = "singleDecisionTree",
                testing = "crossValidation",
                noOfFolds = folds
            )
        })
    }
})