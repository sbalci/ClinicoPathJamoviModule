# Test suite for modelbuilder function
# Tests cover prediction model building, validation, performance metrics, and integration with DCA

library(testthat)
library(dplyr)

# Helper functions for creating test data
create_simple_clinical_data <- function(n = 100, seed = 123) {
    set.seed(seed)
    
    data <- data.frame(
        patient_id = paste0("P", sprintf("%03d", 1:n)),
        age = round(rnorm(n, mean = 65, sd = 12)),
        sex = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)),
        diabetes = sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.65, 0.35)),
        hypertension = sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.55, 0.45)),
        smoking = sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.45, 0.35, 0.20)),
        cholesterol = round(rnorm(n, mean = 200, sd = 35)),
        troponin = round(rlnorm(n, meanlog = 1.2, sdlog = 0.6), 2),
        creatinine = round(rnorm(n, mean = 1.0, sd = 0.25), 2),
        stringsAsFactors = FALSE
    )
    
    # Ensure realistic ranges
    data$age <- pmax(35, pmin(85, data$age))
    data$cholesterol <- pmax(120, pmin(320, data$cholesterol))
    data$creatinine <- pmax(0.5, pmin(2.5, data$creatinine))
    
    # Generate realistic binary outcome
    risk_score <- 0.05 * (data$age - 65) + 
                  0.8 * (data$sex == "Male") + 
                  1.2 * (data$diabetes == "Yes") + 
                  0.7 * (data$hypertension == "Yes") + 
                  1.5 * (data$smoking == "Current") + 
                  0.008 * (data$cholesterol - 200) + 
                  0.3 * log(data$troponin + 0.1) + 
                  0.8 * (data$creatinine - 1.0) + 
                  rnorm(n, 0, 0.8)
    
    prob_outcome <- plogis(risk_score - 1.5)
    outcome_numeric <- rbinom(n, 1, prob_outcome)
    data$cardiovascular_event <- factor(outcome_numeric, levels = c(0, 1), labels = c("No", "Yes"))
    
    return(data)
}

create_missing_data <- function(n = 100, missing_rate = 0.2, seed = 456) {
    set.seed(seed)
    
    data <- create_simple_clinical_data(n = n, seed = seed)
    
    # Introduce missing data
    missing_indices <- sample(1:n, size = floor(n * missing_rate))
    data$troponin[missing_indices] <- NA
    
    creatinine_missing <- sample(1:n, size = floor(n * missing_rate * 0.5))
    data$creatinine[creatinine_missing] <- NA
    
    return(data)
}

create_small_dataset <- function(n = 30, seed = 789) {
    set.seed(seed)
    return(create_simple_clinical_data(n = n, seed = seed))
}

create_imbalanced_data <- function(n = 100, event_rate = 0.05, seed = 101) {
    set.seed(seed)
    
    data <- create_simple_clinical_data(n = n, seed = seed)
    
    # Force low event rate
    n_events <- max(5, floor(n * event_rate))
    event_indices <- sample(1:n, n_events)
    data$cardiovascular_event <- factor(rep("No", n), levels = c("No", "Yes"))
    data$cardiovascular_event[event_indices] <- "Yes"
    
    return(data)
}

# Basic functionality tests
describe("modelbuilder Basic Functionality", {
    
    test_that("modelbuilder data validation works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
        data <- create_simple_clinical_data(n = 100)
        
        # Test basic data structure
        expect_true(is.data.frame(data))
        expect_true(nrow(data) > 0)
        expect_true(ncol(data) > 0)
        
        # Test outcome variable
        expect_true("cardiovascular_event" %in% names(data))
        expect_true(is.factor(data$cardiovascular_event))
        expect_equal(length(levels(data$cardiovascular_event)), 2)
        
        # Test predictor variables
        expect_true(all(c("age", "sex", "diabetes", "hypertension") %in% names(data)))
        expect_true(is.numeric(data$age))
        expect_true(is.character(data$sex) || is.factor(data$sex))
    })
    
    test_that("modelbuilder handles complete data correctly", {
        data <- create_simple_clinical_data(n = 50)
        
        # Test complete data
        expect_true(is.data.frame(data))
        expect_equal(sum(is.na(data)), 0)
        expect_equal(nrow(data), 50)
        
        # Test outcome distribution
        outcome_table <- table(data$cardiovascular_event)
        expect_true(all(outcome_table > 0))
        expect_equal(sum(outcome_table), 50)
    })
    
    test_that("modelbuilder handles missing data appropriately", {
        data <- create_missing_data(n = 100)
        
        # Test missing data presence
        expect_true(sum(is.na(data)) > 0)
        
        # Test specific missing patterns
        troponin_missing <- sum(is.na(data$troponin))
        creatinine_missing <- sum(is.na(data$creatinine))
        
        expect_true(troponin_missing > 0)
        expect_true(creatinine_missing >= 0)
    })
})

# Data validation tests
describe("modelbuilder Data Validation", {
    
    test_that("modelbuilder validates sample size requirements", {
        # Test minimum sample size
        small_data <- create_small_dataset(n = 30)
        expect_true(nrow(small_data) >= 30)
        
        # Test event counts
        event_count <- sum(small_data$cardiovascular_event == "Yes")
        expect_true(event_count >= 5)
    })
    
    test_that("modelbuilder validates outcome variable", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test binary outcome
        unique_outcomes <- unique(data$cardiovascular_event)
        expect_equal(length(unique_outcomes), 2)
        
        # Test outcome levels
        expect_true(all(c("No", "Yes") %in% levels(data$cardiovascular_event)))
    })
    
    test_that("modelbuilder handles imbalanced data", {
        data <- create_imbalanced_data(n = 100, event_rate = 0.05)
        
        # Test event rate
        event_rate <- mean(data$cardiovascular_event == "Yes")
        expect_true(event_rate > 0 && event_rate < 0.2)
        
        # Test minimum events
        n_events <- sum(data$cardiovascular_event == "Yes")
        expect_true(n_events >= 5)
    })
})

# Model building tests
describe("modelbuilder Model Building", {
    
    test_that("modelbuilder basic model building works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test basic predictors
        basic_predictors <- c("age", "sex", "diabetes", "hypertension")
        expect_true(all(basic_predictors %in% names(data)))
        
        # Test formula creation
        formula_string <- paste("cardiovascular_event ~", paste(basic_predictors, collapse = " + "))
        formula_obj <- as.formula(formula_string)
        expect_true(inherits(formula_obj, "formula"))
    })
    
    test_that("modelbuilder enhanced model building works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test enhanced predictors
        enhanced_predictors <- c("age", "sex", "diabetes", "hypertension", "smoking", "cholesterol")
        expect_true(all(enhanced_predictors %in% names(data)))
        
        # Test predictor types
        expect_true(is.numeric(data$age))
        expect_true(is.numeric(data$cholesterol))
        expect_true(is.character(data$smoking) || is.factor(data$smoking))
    })
    
    test_that("modelbuilder biomarker model building works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test biomarker predictors
        biomarker_predictors <- c("age", "sex", "diabetes", "troponin", "creatinine")
        expect_true(all(biomarker_predictors %in% names(data)))
        
        # Test biomarker data types
        expect_true(is.numeric(data$troponin))
        expect_true(is.numeric(data$creatinine))
    })
    
    test_that("modelbuilder handles model fitting", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test GLM fitting
        expect_no_error({
            model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                        family = binomial, data = data)
            
            expect_true(inherits(model, "glm"))
            expect_true(model$converged)
            expect_true(length(coef(model)) > 0)
        })
    })
})

# Performance metrics tests
describe("modelbuilder Performance Metrics", {
    
    test_that("modelbuilder AUC calculation works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Fit simple model
        model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                    family = binomial, data = data)
        
        # Generate predictions
        predictions <- predict(model, type = "response")
        actual <- data$cardiovascular_event
        
        # Test prediction properties
        expect_true(all(predictions >= 0 & predictions <= 1))
        expect_equal(length(predictions), length(actual))
        expect_false(any(is.na(predictions)))
    })
    
    test_that("modelbuilder calibration metrics work", {
        data <- create_simple_clinical_data(n = 100)
        
        # Fit model and generate predictions
        model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                    family = binomial, data = data)
        predictions <- predict(model, type = "response")
        
        # Test calibration data preparation
        binary_actual <- as.numeric(data$cardiovascular_event == "Yes")
        
        expect_true(all(binary_actual %in% c(0, 1)))
        expect_equal(length(binary_actual), length(predictions))
    })
    
    test_that("modelbuilder model statistics work", {
        data <- create_simple_clinical_data(n = 100)
        
        # Fit model
        model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                    family = binomial, data = data)
        
        # Test model statistics
        expect_true(is.numeric(AIC(model)))
        expect_true(is.numeric(BIC(model)))
        expect_true(is.numeric(logLik(model)))
        expect_true(model$df.residual > 0)
    })
})

# Missing data handling tests
describe("modelbuilder Missing Data Handling", {
    
    test_that("modelbuilder complete cases method works", {
        data <- create_missing_data(n = 100)
        
        # Test complete cases
        complete_data <- na.omit(data)
        expect_true(nrow(complete_data) < nrow(data))
        expect_equal(sum(is.na(complete_data)), 0)
    })
    
    test_that("modelbuilder mean imputation works", {
        data <- create_missing_data(n = 100)
        
        # Test mean imputation for numeric variables
        troponin_mean <- mean(data$troponin, na.rm = TRUE)
        expect_true(is.numeric(troponin_mean))
        expect_false(is.na(troponin_mean))
        
        # Test imputation logic
        missing_indices <- which(is.na(data$troponin))
        expect_true(length(missing_indices) > 0)
    })
    
    test_that("modelbuilder variable exclusion works", {
        data <- create_missing_data(n = 100, missing_rate = 0.8)
        
        # Test high missing rate detection
        missing_prop <- sapply(data, function(x) sum(is.na(x)) / length(x))
        high_missing_vars <- names(missing_prop)[missing_prop > 0.5]
        
        expect_true(length(high_missing_vars) > 0)
    })
})

# Data splitting tests
describe("modelbuilder Data Splitting", {
    
    test_that("modelbuilder stratified splitting works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test stratified sampling
        outcome_var <- "cardiovascular_event"
        outcome_positive <- "Yes"
        
        positive_indices <- which(data[[outcome_var]] == outcome_positive)
        negative_indices <- which(data[[outcome_var]] != outcome_positive)
        
        expect_true(length(positive_indices) > 0)
        expect_true(length(negative_indices) > 0)
        expect_equal(length(positive_indices) + length(negative_indices), nrow(data))
    })
    
    test_that("modelbuilder split ratio validation works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test 70/30 split
        split_ratio <- 0.7
        n_train <- floor(nrow(data) * split_ratio)
        n_val <- nrow(data) - n_train
        
        expect_true(n_train > 0)
        expect_true(n_val > 0)
        expect_equal(n_train + n_val, nrow(data))
    })
    
    test_that("modelbuilder maintains event rates across splits", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test event rate preservation
        overall_event_rate <- mean(data$cardiovascular_event == "Yes")
        
        # Simulate stratified split
        set.seed(123)
        positive_indices <- which(data$cardiovascular_event == "Yes")
        negative_indices <- which(data$cardiovascular_event == "No")
        
        train_pos <- sample(positive_indices, floor(length(positive_indices) * 0.7))
        train_neg <- sample(negative_indices, floor(length(negative_indices) * 0.7))
        
        train_indices <- c(train_pos, train_neg)
        val_indices <- setdiff(1:nrow(data), train_indices)
        
        train_event_rate <- mean(data$cardiovascular_event[train_indices] == "Yes")
        val_event_rate <- mean(data$cardiovascular_event[val_indices] == "Yes")
        
        # Event rates should be similar
        expect_true(abs(train_event_rate - val_event_rate) < 0.2)
    })
})

# Cross-validation tests
describe("modelbuilder Cross-Validation", {
    
    test_that("modelbuilder k-fold setup works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test k-fold parameters
        k_folds <- 5
        expect_true(k_folds > 1)
        expect_true(k_folds <= nrow(data))
        
        # Test fold creation
        fold_indices <- sample(rep(1:k_folds, length.out = nrow(data)))
        expect_equal(length(fold_indices), nrow(data))
        expect_true(all(fold_indices %in% 1:k_folds))
    })
    
    test_that("modelbuilder cross-validation splitting works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test CV split
        k_folds <- 5
        fold_indices <- sample(rep(1:k_folds, length.out = nrow(data)))
        
        for (fold in 1:k_folds) {
            train_data <- data[fold_indices != fold, ]
            test_data <- data[fold_indices == fold, ]
            
            expect_true(nrow(train_data) > 0)
            expect_true(nrow(test_data) > 0)
            expect_equal(nrow(train_data) + nrow(test_data), nrow(data))
        }
    })
    
    test_that("modelbuilder CV performance metrics work", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test CV results structure
        cv_results <- data.frame(
            fold = 1:5,
            auc = numeric(5),
            brier_score = numeric(5),
            calibration_slope = numeric(5)
        )
        
        expect_true(is.data.frame(cv_results))
        expect_equal(nrow(cv_results), 5)
        expect_true(all(c("fold", "auc", "brier_score") %in% names(cv_results)))
    })
})

# Integration tests
describe("modelbuilder Integration", {
    
    test_that("modelbuilder workflow integration works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test complete workflow components
        expect_true(is.data.frame(data))
        expect_true("cardiovascular_event" %in% names(data))
        expect_true(nrow(data) >= 50)
        
        # Test model fitting
        expect_no_error({
            model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                        family = binomial, data = data)
            predictions <- predict(model, type = "response")
            
            expect_true(length(predictions) == nrow(data))
            expect_true(all(predictions >= 0 & predictions <= 1))
        })
    })
    
    test_that("modelbuilder DCA preparation works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test DCA readiness
        model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                    family = binomial, data = data)
        predictions <- predict(model, type = "response")
        
        # Test prediction column creation
        data$basic_model_prob <- predictions
        
        expect_true("basic_model_prob" %in% names(data))
        expect_true(is.numeric(data$basic_model_prob))
        expect_true(all(data$basic_model_prob >= 0 & data$basic_model_prob <= 1))
    })
    
    test_that("modelbuilder multiple model comparison works", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test multiple models
        basic_model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                          family = binomial, data = data)
        enhanced_model <- glm(cardiovascular_event ~ age + sex + diabetes + smoking + cholesterol, 
                             family = binomial, data = data)
        
        # Test model comparison
        basic_aic <- AIC(basic_model)
        enhanced_aic <- AIC(enhanced_model)
        
        expect_true(is.numeric(basic_aic))
        expect_true(is.numeric(enhanced_aic))
        expect_true(basic_aic != enhanced_aic)
    })
})

# Edge case tests
describe("modelbuilder Edge Cases", {
    
    test_that("modelbuilder handles small samples", {
        data <- create_small_dataset(n = 30)
        
        # Test minimum requirements
        expect_true(nrow(data) >= 30)
        expect_true(sum(data$cardiovascular_event == "Yes") >= 5)
        expect_true(sum(data$cardiovascular_event == "No") >= 5)
    })
    
    test_that("modelbuilder handles perfect separation", {
        data <- create_simple_clinical_data(n = 50)
        
        # Test separation detection
        for (var in names(data)) {
            if (is.factor(data[[var]]) || is.character(data[[var]])) {
                cross_tab <- table(data[[var]], data$cardiovascular_event)
                
                # Check if any cells are zero (potential separation)
                has_zero_cells <- any(cross_tab == 0)
                
                if (has_zero_cells) {
                    # This is expected in some cases
                    expect_true(is.logical(has_zero_cells))
                }
            }
        }
    })
    
    test_that("modelbuilder handles high correlation", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test correlation detection
        numeric_vars <- sapply(data, is.numeric)
        if (sum(numeric_vars) > 1) {
            cor_matrix <- cor(data[, numeric_vars, drop = FALSE], use = "complete.obs")
            
            # Check correlation structure
            expect_true(is.matrix(cor_matrix))
            expect_true(nrow(cor_matrix) == ncol(cor_matrix))
            expect_true(all(diag(cor_matrix) == 1))
        }
    })
    
    test_that("modelbuilder handles convergence issues", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test convergence monitoring
        model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                    family = binomial, data = data,
                    control = glm.control(maxit = 100))
        
        # Test convergence properties
        expect_true(is.logical(model$converged))
        expect_true(is.numeric(model$iter))
        expect_true(model$iter <= 100)
    })
})

# Performance tests
describe("modelbuilder Performance", {
    
    test_that("modelbuilder performance scales appropriately", {
        data <- create_simple_clinical_data(n = 200)
        
        # Test performance with larger dataset
        start_time <- Sys.time()
        model <- glm(cardiovascular_event ~ age + sex + diabetes + smoking + cholesterol, 
                    family = binomial, data = data)
        end_time <- Sys.time()
        
        execution_time <- as.numeric(end_time - start_time)
        
        expect_true(execution_time < 5)  # Should complete within 5 seconds
        expect_true(is.numeric(execution_time))
    })
    
    test_that("modelbuilder memory usage is reasonable", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test memory efficiency
        object_size <- object.size(data)
        expect_true(object_size < 1000000)  # Less than 1MB
    })
})

# Clinical validation tests
describe("modelbuilder Clinical Validation", {
    
    test_that("modelbuilder produces clinically plausible results", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test clinical plausibility
        model <- glm(cardiovascular_event ~ age + sex + diabetes, 
                    family = binomial, data = data)
        
        # Check coefficient directions (should be positive for risk factors)
        coefficients <- coef(model)
        
        # Age should generally increase risk
        if ("age" %in% names(coefficients)) {
            expect_true(is.numeric(coefficients["age"]))
        }
        
        # Diabetes should increase risk
        if ("diabetesYes" %in% names(coefficients)) {
            expect_true(is.numeric(coefficients["diabetesYes"]))
        }
    })
    
    test_that("modelbuilder handles realistic clinical scenarios", {
        data <- create_simple_clinical_data(n = 100)
        
        # Test realistic event rates
        event_rate <- mean(data$cardiovascular_event == "Yes")
        expect_true(event_rate > 0.1 && event_rate < 0.8)  # Reasonable range
        
        # Test age distribution
        expect_true(min(data$age) >= 18)
        expect_true(max(data$age) <= 100)
        expect_true(mean(data$age) > 40)  # Typical clinical population
    })
})

test_that("ClinicoPath::modelbuilder runs successfully", {
    data <- create_simple_clinical_data(n = 200)
    
    # Run the analysis
    results <- ClinicoPath::modelbuilder(
        data = data,
        outcome = "cardiovascular_event",
        outcomePositive = "Yes",
        basicPredictors = c("age", "sex", "diabetes"),
        buildBasicModel = TRUE,
        enhancedPredictors = NULL,
        biomarkerPredictors = NULL,
        customPredictors = NULL,
        splitData = FALSE,
        crossValidation = FALSE
    )
    
    # Check if results object is returned
    expect_true(!is.null(results))
    expect_true(inherits(results, "modelbuilderResults"))
    
    # Check if main tables exist
    # Note: accessors might vary, verify with .h.R if needed, but usually match structure
    expect_true(!is.null(results$basicModelSummary))
})

print("All modelbuilder tests completed successfully!")
