test_that("advancedtree module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("advancedtreeClass"))
  expect_true(is.function(advancedtree))
})

test_that("advancedtree handles basic input validation", {
  # Test with missing required variables
  expect_error(
    advancedtree(data = histopathology, vars = NULL, facs = NULL, target = "Outcome"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    advancedtree(data = histopathology, vars = c("Age"), facs = NULL, target = NULL),
    NA  # Should not error during initialization, only during run
  )
})

test_that("advancedtree works with valid basic inputs", {
  # Test basic functionality with CART
  result <- advancedtree(
    data = histopathology,
    vars = c("Age", "Grade"),
    facs = c("Sex"),
    target = "Outcome",
    targetLevel = "1",
    algorithm = "rpart"
  )
  
  expect_s3_class(result, "advancedtreeClass")
  expect_true("Age" %in% names(histopathology))
  expect_true("Grade" %in% names(histopathology))
  expect_true("Sex" %in% names(histopathology))
  expect_true("Outcome" %in% names(histopathology))
})

test_that("advancedtree algorithms work correctly", {
  # Test different algorithms
  algorithms <- c("rpart", "ctree", "randomforest", "xgboost", "extratrees", "ensemble")
  
  for (algorithm in algorithms) {
    expect_error({
      result <- advancedtree(
        data = histopathology,
        vars = c("Age", "Grade"),
        facs = c("Sex", "Group"),
        target = "Mortality5yr",
        targetLevel = "Dead",
        algorithm = algorithm
      )
    }, NA, info = paste("algorithm:", algorithm))
  }
})

test_that("advancedtree validation methods work correctly", {
  # Test different validation approaches
  validation_methods <- c("cv", "bootstrap", "holdout", "temporal")
  
  for (validation in validation_methods) {
    expect_error({
      result <- advancedtree(
        data = histopathology,
        vars = c("Age", "Grade", "TStage"),
        facs = c("Sex", "LVI"),
        target = "Outcome",
        targetLevel = "1",
        algorithm = "rpart",
        validation = validation
      )
    }, NA, info = paste("validation:", validation))
  }
})

test_that("advancedtree hyperparameters work correctly", {
  # Test hyperparameter settings
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage"),
      facs = c("Sex", "Group"),
      target = "Mortality5yr",
      targetLevel = "Dead",
      algorithm = "randomforest",
      max_depth = 5,
      min_samples_split = 20,
      min_samples_leaf = 10,
      n_estimators = 50,
      learning_rate = 0.1
    )
  }, NA)
})

test_that("advancedtree feature selection works correctly", {
  # Test automated feature selection
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage", "OverallTime"),
      facs = c("Sex", "Group", "LVI", "PNI"),
      target = "Outcome",
      targetLevel = "1",
      algorithm = "randomforest",
      feature_selection = TRUE,
      importance_method = "gini"
    )
  }, NA)
  
  # Test different importance methods
  importance_methods <- c("gini", "permutation", "shap")
  
  for (method in importance_methods) {
    expect_error({
      result <- advancedtree(
        data = histopathology,
        vars = c("Age", "Grade"),
        facs = c("Sex", "Group"),
        target = "Outcome",
        targetLevel = "1",
        algorithm = "randomforest",
        importance_method = method
      )
    }, NA, info = paste("importance_method:", method))
  }
})

test_that("advancedtree class imbalance handling works correctly", {
  # Test class imbalance handling
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage"),
      facs = c("Sex", "LVI"),
      target = "Mortality5yr",
      targetLevel = "Dead",
      algorithm = "randomforest",
      handle_imbalance = TRUE,
      imbalance_method = "weights"
    )
  }, NA)
  
  # Test different imbalance methods
  imbalance_methods <- c("weights", "smote", "undersample", "cost_sensitive")
  
  for (method in imbalance_methods) {
    expect_error({
      result <- advancedtree(
        data = histopathology,
        vars = c("Age", "Grade"),
        facs = c("Sex", "Group"),
        target = "Outcome",
        targetLevel = "1",
        algorithm = "randomforest",
        handle_imbalance = TRUE,
        imbalance_method = method
      )
    }, NA, info = paste("imbalance_method:", method))
  }
})

test_that("advancedtree hyperparameter tuning works correctly", {
  # Test hyperparameter optimization
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade"),
      facs = c("Sex", "Group"),
      target = "Outcome",
      targetLevel = "1",
      algorithm = "xgboost",
      hyperparameter_tuning = TRUE,
      tuning_method = "random"
    )
  }, NA)
  
  # Test different tuning methods
  tuning_methods <- c("grid", "random", "bayesian")
  
  for (method in tuning_methods) {
    expect_error({
      result <- advancedtree(
        data = histopathology,
        vars = c("Age", "Grade"),
        facs = c("Sex"),
        target = "Outcome",
        targetLevel = "1",
        algorithm = "randomforest",
        hyperparameter_tuning = TRUE,
        tuning_method = method
      )
    }, NA, info = paste("tuning_method:", method))
  }
})

test_that("advancedtree visualization options work correctly", {
  # Test all visualization options
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage"),
      facs = c("Sex", "Group"),
      target = "Mortality5yr",
      targetLevel = "Dead",
      algorithm = "rpart",
      show_tree_plot = TRUE,
      show_importance_plot = TRUE,
      show_performance_metrics = TRUE,
      show_validation_curves = TRUE,
      show_roc_curve = TRUE,
      show_calibration_plot = TRUE,
      show_confusion_matrix = TRUE
    )
  }, NA)
})

test_that("advancedtree interpretability features work correctly", {
  # Test interpretability options
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage"),
      facs = c("Sex", "LVI"),
      target = "Outcome",
      targetLevel = "1",
      algorithm = "randomforest",
      interpretability = TRUE,
      shap_analysis = TRUE,
      partial_dependence = TRUE,
      interaction_analysis = TRUE
    )
  }, NA)
})

test_that("advancedtree clinical context options work correctly", {
  # Test different clinical contexts
  clinical_contexts <- c("diagnosis", "prognosis", "treatment", "risk", "biomarker", "screening")
  
  for (context in clinical_contexts) {
    expect_error({
      result <- advancedtree(
        data = histopathology,
        vars = c("Age", "Grade"),
        facs = c("Sex", "Group"),
        target = "Outcome",
        targetLevel = "1",
        algorithm = "rpart",
        clinical_context = context
      )
    }, NA, info = paste("clinical_context:", context))
  }
})

test_that("advancedtree cost-sensitive learning works correctly", {
  # Test cost-sensitive threshold optimization
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage"),
      facs = c("Sex", "LVI"),
      target = "Mortality5yr",
      targetLevel = "Dead",
      algorithm = "randomforest",
      cost_sensitive_thresholds = TRUE,
      fn_fp_ratio = 2.0
    )
  }, NA)
  
  # Test boundary values for cost ratio
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade"),
      facs = c("Sex"),
      target = "Outcome",
      targetLevel = "1",
      algorithm = "rpart",
      cost_sensitive_thresholds = TRUE,
      fn_fp_ratio = 0.1  # Minimum value
    )
  }, NA)
  
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade"),
      facs = c("Sex"),
      target = "Outcome",
      targetLevel = "1",
      algorithm = "rpart",
      cost_sensitive_thresholds = TRUE,
      fn_fp_ratio = 10.0  # Maximum value
    )
  }, NA)
})

test_that("advancedtree missing data handling works correctly", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Age[1:10] <- NA
  test_data$Grade[5:15] <- NA
  
  # Test different missing data strategies
  missing_strategies <- c("complete", "simple", "model", "tree")
  
  for (strategy in missing_strategies) {
    expect_error({
      result <- advancedtree(
        data = test_data,
        vars = c("Age", "Grade", "TStage"),
        facs = c("Sex", "Group"),
        target = "Outcome",
        targetLevel = "1",
        algorithm = "randomforest",
        missing_data_handling = strategy
      )
    }, NA, info = paste("missing_data_handling:", strategy))
  }
})

test_that("advancedtree bootstrap confidence works correctly", {
  # Test bootstrap confidence intervals
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade"),
      facs = c("Sex", "Group"),
      target = "Outcome",
      targetLevel = "1",
      algorithm = "rpart",
      bootstrap_confidence = TRUE,
      n_bootstrap = 100  # Reduced for testing speed
    )
  }, NA)
  
  # Test boundary values for bootstrap samples
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade"),
      facs = c("Sex"),
      target = "Outcome",
      targetLevel = "1",
      algorithm = "rpart",
      bootstrap_confidence = TRUE,
      n_bootstrap = 100  # Minimum practical value
    )
  }, NA)
})

test_that("advancedtree model export works correctly", {
  # Test model export functionality
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage"),
      facs = c("Sex", "LVI"),
      target = "Mortality5yr",
      targetLevel = "Dead",
      algorithm = "randomforest",
      export_model = TRUE
    )
  }, NA)
})

test_that("advancedtree handles different variable types", {
  # Test with numeric target variable (should convert to factor)
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade"),
      facs = c("Sex", "Group"),
      target = "Outcome",  # Numeric variable
      targetLevel = "1",
      algorithm = "rpart"
    )
  }, NA)
  
  # Test with character target variable
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade"),
      facs = c("Sex", "Group"),
      target = "Mortality5yr",  # Character variable
      targetLevel = "Dead",
      algorithm = "rpart"
    )
  }, NA)
})

test_that("advancedtree handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:30, ]
  
  expect_error({
    result <- advancedtree(
      data = small_data,
      vars = c("Age", "Grade"),
      facs = c("Sex", "Group"),
      target = "Outcome",
      targetLevel = "1",
      algorithm = "rpart",
      validation = "cv",
      cv_folds = 3  # Reduced folds for small data
    )
  }, NA)
})

test_that("advancedtree works with different continuous variables", {
  # Test with different continuous variables from histopathology
  continuous_vars <- c("Age", "Grade", "TStage", "OverallTime", "MeasurementA", "MeasurementB")
  
  for (var in continuous_vars) {
    if (var %in% names(histopathology) && !all(is.na(histopathology[[var]]))) {
      expect_error({
        result <- advancedtree(
          data = histopathology,
          vars = var,
          facs = c("Sex", "Group"),
          target = "Outcome",
          targetLevel = "1",
          algorithm = "rpart"
        )
      }, NA, info = paste("continuous_var:", var))
    }
  }
})

test_that("advancedtree works with different categorical variables", {
  # Test with different categorical variables
  categorical_vars <- c("Sex", "Group", "Grade_Level", "LVI", "PNI", "LymphNodeMetastasis")
  
  for (var in categorical_vars) {
    if (var %in% names(histopathology)) {
      expect_error({
        result <- advancedtree(
          data = histopathology,
          vars = c("Age", "Grade"),
          facs = var,
          target = "Outcome",
          targetLevel = "1",
          algorithm = "rpart"
        )
      }, NA, info = paste("categorical_var:", var))
    }
  }
})

test_that("advancedtree complex parameter combinations work", {
  # Test complex parameter combinations
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage", "OverallTime"),
      facs = c("Sex", "Group", "LVI", "PNI"),
      target = "Mortality5yr",
      targetLevel = "Dead",
      algorithm = "ensemble",
      validation = "cv",
      cv_folds = 5,
      max_depth = 6,
      min_samples_split = 20,
      min_samples_leaf = 10,
      n_estimators = 100,
      learning_rate = 0.1,
      feature_selection = TRUE,
      importance_method = "permutation",
      handle_imbalance = TRUE,
      imbalance_method = "weights",
      clinical_context = "prognosis",
      cost_sensitive_thresholds = TRUE,
      fn_fp_ratio = 2.0,
      missing_data_handling = "simple",
      show_tree_plot = FALSE,  # Ensemble doesn't show individual trees
      show_importance_plot = TRUE,
      show_performance_metrics = TRUE,
      show_roc_curve = TRUE,
      interpretability = TRUE
    )
  }, NA)
})

test_that("advancedtree dependency handling", {
  # Test function behavior when optional packages are available
  required_packages <- c("rpart", "randomForest", "party", "caret", "pROC")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      expect_error({
        result <- advancedtree(
          data = histopathology,
          vars = c("Age", "Grade"),
          facs = c("Sex", "Group"),
          target = "Outcome",
          targetLevel = "1",
          algorithm = "rpart"
        )
      }, NA, info = paste("package:", pkg))
    }
  }
})

test_that("advancedtree vs other tree functions compatibility", {
  # Test that advanced tree works with similar data as other tree functions
  expect_error({
    result <- advancedtree(
      data = histopathology,
      vars = c("Age", "Grade", "TStage"),  # Similar to predictor variables
      facs = c("Sex", "Group", "LVI"),     # Similar to categorical predictors
      target = "Outcome",                   # Similar to outcome variable
      targetLevel = "1",                    # Similar to positive level
      algorithm = "rpart"
    )
  }, NA)
  
  # Test that results object has expected structure
  result <- advancedtree(
    data = histopathology,
    vars = c("Age", "Grade"),
    facs = c("Sex", "Group"),
    target = "Outcome",
    targetLevel = "1",
    algorithm = "rpart"
  )
  
  expect_true(exists("model_summary", envir = result))
  expect_true(exists("performance_table", envir = result))
  expect_true(exists("tree_plot", envir = result))
})
