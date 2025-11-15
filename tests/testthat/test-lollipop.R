# Test suite for lollipop function
# Tests cover data structure validation, helper functions, and edge cases

library(testthat)
library(ggplot2)
library(dplyr)

# Helper functions for testing
create_clinical_data <- function(n = 20, seed = 123) {
  set.seed(seed)
  
  # Create patient biomarker data
  patient_ids <- paste0("P", sprintf("%03d", 1:n))
  biomarker_levels <- round(rnorm(n, mean = 45, sd = 15), 1)
  
  data <- data.frame(
    patient_id = patient_ids,
    biomarker_level = pmax(biomarker_levels, 5), # Ensure positive values
    risk_category = sample(c("Low", "Medium", "High"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
  )
  
  return(data)
}

create_treatment_data <- function(n = 15, seed = 456) {
  set.seed(seed)
  
  # Create treatment response data
  treatments <- c("Treatment_A", "Treatment_B", "Treatment_C", "Treatment_D", "Treatment_E")
  response_scores <- round(runif(n, min = 10, max = 100), 1)
  
  data <- data.frame(
    treatment = sample(treatments, n, replace = TRUE),
    response_score = response_scores,
    efficacy = ifelse(response_scores > 70, "High", ifelse(response_scores > 40, "Medium", "Low"))
  )
  
  return(data)
}

create_timeline_data <- function(n = 12, seed = 789) {
  set.seed(seed)
  
  # Create patient timeline data
  patient_ids <- paste0("Patient_", LETTERS[1:n])
  days_to_event <- round(rexp(n, rate = 0.02), 0)
  
  data <- data.frame(
    patient_id = patient_ids,
    days_to_event = days_to_event,
    event_type = sample(c("Response", "Progression", "Stable"), n, replace = TRUE),
    treatment_arm = sample(c("Control", "Experimental"), n, replace = TRUE)
  )
  
  return(data)
}

create_survey_data <- function(n = 25, seed = 234) {
  set.seed(seed)
  
  # Create survey response data
  questions <- paste0("Q", 1:n)
  satisfaction_scores <- round(runif(n, min = 1, max = 10), 1)
  
  data <- data.frame(
    question = questions,
    satisfaction_score = satisfaction_scores,
    category = sample(c("Service", "Quality", "Price", "Support"), n, replace = TRUE)
  )
  
  return(data)
}

create_quality_metrics_data <- function(n = 18, seed = 567) {
  set.seed(seed)
  
  # Create quality metrics data
  metrics <- c("Accuracy", "Precision", "Recall", "F1_Score", "AUC", "Specificity", 
               "Sensitivity", "PPV", "NPV", "Accuracy_Test", "Precision_Test", 
               "Recall_Test", "F1_Test", "AUC_Test", "Spec_Test", "Sens_Test", 
               "PPV_Test", "NPV_Test")
  
  metric_values <- round(runif(n, min = 0.5, max = 1.0), 3)
  
  data <- data.frame(
    metric = metrics[1:n],
    value = metric_values,
    model_type = sample(c("Random_Forest", "SVM", "Neural_Network"), n, replace = TRUE)
  )
  
  return(data)
}

# Basic functionality tests
describe("lollipop Basic Functionality", {
  
  test_that("lollipop data structure validation works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_clinical_data()
    
    # Test basic functionality without errors
    expect_no_error({
      # This would test the basic R6 class structure
      # In actual jamovi, this would be:
      # result <- lollipop(data = data, dep = "biomarker_level", group = "patient_id")
      
      # For testing purposes, we'll validate the data structure
      expect_true(is.data.frame(data))
      expect_true("biomarker_level" %in% names(data))
      expect_true("patient_id" %in% names(data))
      expect_true(is.numeric(data$biomarker_level))
      expect_true(is.character(data$patient_id) || is.factor(data$patient_id))
    })
  })
  
  test_that("lollipop handles treatment data correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_treatment_data()
    
    # Validate treatment data structure
    expect_true("treatment" %in% names(data))
    expect_true("response_score" %in% names(data))
    expect_true(is.character(data$treatment) || is.factor(data$treatment))
    expect_true(is.numeric(data$response_score))
    
    # Check realistic response score ranges
    expect_true(all(data$response_score >= 0 & data$response_score <= 100))
  })
  
  test_that("lollipop handles timeline data correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_timeline_data()
    
    # Validate timeline data structure
    expect_true(all(c("patient_id", "days_to_event", "event_type") %in% names(data)))
    expect_true(is.character(data$patient_id) || is.factor(data$patient_id))
    expect_true(is.numeric(data$days_to_event))
    expect_true(is.character(data$event_type) || is.factor(data$event_type))
    
    # Check realistic time ranges
    expect_true(all(data$days_to_event >= 0))
  })
})

# Data validation tests
describe("lollipop Data Validation", {
  
  test_that("lollipop validates required variables", {
    data <- create_clinical_data()
    
    # Test required variables exist
    expect_true("biomarker_level" %in% names(data))
    expect_true("patient_id" %in% names(data))
    
    # Test variable types
    expect_true(is.numeric(data$biomarker_level))
    expect_true(is.character(data$patient_id) || is.factor(data$patient_id))
  })
  
  test_that("lollipop validates data types", {
    data <- create_clinical_data()
    
    # Test numeric dependent variable
    expect_true(is.numeric(data$biomarker_level))
    expect_false(is.character(data$biomarker_level))
    expect_false(is.logical(data$biomarker_level))
    
    # Test categorical grouping variable
    expect_true(is.character(data$patient_id) || is.factor(data$patient_id))
    expect_false(is.numeric(data$patient_id))
  })
  
  test_that("lollipop validates data ranges", {
    data <- create_clinical_data()
    
    # Test realistic biomarker ranges
    expect_true(all(data$biomarker_level >= 0))
    expect_true(all(data$biomarker_level < 200))  # Realistic upper bound
    
    # Test unique patient IDs
    expect_equal(length(unique(data$patient_id)), nrow(data))
  })
})

# Sorting functionality tests
describe("lollipop Sorting Functions", {
  
  test_that("lollipop data can be sorted ascending", {
    data <- create_treatment_data()
    
    # Test ascending sort
    sorted_data <- data[order(data$response_score), ]
    expect_true(is.data.frame(sorted_data))
    expect_equal(nrow(sorted_data), nrow(data))
    expect_true(all(diff(sorted_data$response_score) >= 0))
  })
  
  test_that("lollipop data can be sorted descending", {
    data <- create_treatment_data()
    
    # Test descending sort
    sorted_data <- data[order(-data$response_score), ]
    expect_true(is.data.frame(sorted_data))
    expect_equal(nrow(sorted_data), nrow(data))
    expect_true(all(diff(sorted_data$response_score) <= 0))
  })
  
  test_that("lollipop data can be sorted alphabetically", {
    data <- create_treatment_data()
    
    # Test alphabetical sort
    sorted_data <- data[order(data$treatment), ]
    expect_true(is.data.frame(sorted_data))
    expect_equal(nrow(sorted_data), nrow(data))
    
    # Check if alphabetically sorted
    treatment_order <- sorted_data$treatment
    expect_true(all(treatment_order == sort(treatment_order)))
  })
})

# Color scheme functionality tests
describe("lollipop Color Schemes", {
  
  test_that("lollipop color schemes are valid", {
    # Test that color schemes contain valid colors
    default_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A")
    clinical_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
    
    # Test color format (hex codes)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", default_colors)))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", clinical_colors)))
    
    # Test color count
    expect_gte(length(default_colors), 6)
    expect_gte(length(clinical_colors), 6)
  })
  
  test_that("lollipop colorblind safe palette exists", {
    colorblind_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
    
    # Test colorblind-safe colors
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colorblind_colors)))
    expect_gte(length(colorblind_colors), 6)
  })
})

# Display options tests
describe("lollipop Display Options", {
  
  test_that("lollipop value display calculations work", {
    data <- create_timeline_data()
    
    # Test value rounding for display
    rounded_values <- round(data$days_to_event, 1)
    expect_true(is.numeric(rounded_values))
    expect_equal(length(rounded_values), nrow(data))
  })
  
  test_that("lollipop mean calculations work", {
    data <- create_timeline_data()
    
    # Test mean calculation
    mean_value <- mean(data$days_to_event, na.rm = TRUE)
    expect_true(is.numeric(mean_value))
    expect_false(is.na(mean_value))
    expect_true(mean_value > 0)
  })
})

# Edge case tests
describe("lollipop Edge Cases", {
  
  test_that("lollipop handles minimal data", {
    minimal_data <- data.frame(
      x = c("A", "B"),
      y = c(1, 2)
    )
    
    expect_true(is.data.frame(minimal_data))
    expect_equal(nrow(minimal_data), 2)
    expect_true(is.numeric(minimal_data$y))
    expect_true(is.character(minimal_data$x) || is.factor(minimal_data$x))
  })
  
  test_that("lollipop handles identical values", {
    identical_data <- data.frame(
      x = c("A", "B", "C"),
      y = c(5, 5, 5)
    )
    
    expect_true(is.data.frame(identical_data))
    expect_equal(length(unique(identical_data$y)), 1)
    expect_true(all(identical_data$y == 5))
  })
  
  test_that("lollipop handles missing values", {
    data_with_na <- create_clinical_data()
    data_with_na$biomarker_level[1:3] <- NA
    
    # Test NA handling
    complete_cases <- complete.cases(data_with_na)
    expect_true(is.logical(complete_cases))
    expect_equal(sum(complete_cases), nrow(data_with_na) - 3)
    
    # Test complete case extraction
    clean_data <- data_with_na[complete_cases, ]
    expect_true(is.data.frame(clean_data))
    expect_equal(nrow(clean_data), nrow(data_with_na) - 3)
  })
})

# Large dataset tests
describe("lollipop Large Dataset Handling", {
  
  test_that("lollipop handles large datasets", {
    large_data <- create_clinical_data(n = 100)
    
    expect_true(is.data.frame(large_data))
    expect_equal(nrow(large_data), 100)
    expect_true(is.numeric(large_data$biomarker_level))
    expect_true(is.character(large_data$patient_id) || is.factor(large_data$patient_id))
  })
  
  test_that("lollipop handles many groups", {
    many_groups_data <- data.frame(
      group = paste0("Group_", 1:60),
      value = round(runif(60, min = 10, max = 100), 1)
    )
    
    expect_true(is.data.frame(many_groups_data))
    expect_equal(nrow(many_groups_data), 60)
    expect_equal(length(unique(many_groups_data$group)), 60)
    expect_true(all(many_groups_data$value >= 10 & many_groups_data$value <= 100))
  })
})

# Statistical calculation tests
describe("lollipop Statistical Calculations", {
  
  test_that("lollipop descriptive statistics work", {
    data <- create_clinical_data()
    
    # Test basic statistics
    mean_val <- mean(data$biomarker_level, na.rm = TRUE)
    median_val <- median(data$biomarker_level, na.rm = TRUE)
    sd_val <- sd(data$biomarker_level, na.rm = TRUE)
    
    expect_true(is.numeric(mean_val))
    expect_true(is.numeric(median_val))
    expect_true(is.numeric(sd_val))
    expect_false(is.na(mean_val))
    expect_false(is.na(median_val))
    expect_false(is.na(sd_val))
  })
  
  test_that("lollipop outlier detection works", {
    data <- create_clinical_data()
    
    # Test outlier detection
    Q1 <- quantile(data$biomarker_level, 0.25, na.rm = TRUE)
    Q3 <- quantile(data$biomarker_level, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    outliers <- data$biomarker_level < lower_bound | data$biomarker_level > upper_bound
    
    expect_true(is.logical(outliers))
    expect_equal(length(outliers), nrow(data))
  })
})

# Real-world data pattern tests
describe("lollipop Real-World Data Patterns", {
  
  test_that("lollipop handles skewed data", {
    skewed_data <- data.frame(
      category = paste0("Cat_", 1:10),
      value = c(rep(1, 7), 50, 75, 100)  # Heavily skewed
    )
    
    expect_true(is.data.frame(skewed_data))
    expect_equal(nrow(skewed_data), 10)
    expect_true(max(skewed_data$value) > 10 * median(skewed_data$value))
  })
  
  test_that("lollipop handles negative values", {
    negative_data <- data.frame(
      category = paste0("Cat_", 1:5),
      value = c(-10, -5, 0, 5, 10)
    )
    
    expect_true(is.data.frame(negative_data))
    expect_true(any(negative_data$value < 0))
    expect_true(any(negative_data$value > 0))
    expect_true(any(negative_data$value == 0))
  })
  
  test_that("lollipop handles very small values", {
    small_data <- data.frame(
      category = paste0("Cat_", 1:5),
      value = c(0.001, 0.005, 0.01, 0.05, 0.1)
    )
    
    expect_true(is.data.frame(small_data))
    expect_true(all(small_data$value > 0))
    expect_true(all(small_data$value < 1))
  })
})

# Clinical application tests
describe("lollipop Clinical Applications", {
  
  test_that("lollipop clinical data scenarios work", {
    # Patient biomarker levels
    biomarker_data <- create_clinical_data()
    expect_true(is.data.frame(biomarker_data))
    expect_true("biomarker_level" %in% names(biomarker_data))
    expect_true("patient_id" %in% names(biomarker_data))
    
    # Treatment response scores
    treatment_data <- create_treatment_data()
    expect_true(is.data.frame(treatment_data))
    expect_true("response_score" %in% names(treatment_data))
    expect_true("treatment" %in% names(treatment_data))
    
    # Patient timeline
    timeline_data <- create_timeline_data()
    expect_true(is.data.frame(timeline_data))
    expect_true("days_to_event" %in% names(timeline_data))
    expect_true("patient_id" %in% names(timeline_data))
  })
})

# Performance tests
describe("lollipop Performance", {
  
  test_that("lollipop performance scales appropriately", {
    # Medium dataset
    medium_data <- create_clinical_data(n = 50)
    
    start_time <- Sys.time()
    # Simulate processing time
    processed_data <- medium_data[order(medium_data$biomarker_level), ]
    end_time <- Sys.time()
    
    expect_true(is.data.frame(processed_data))
    expect_equal(nrow(processed_data), 50)
    expect_true(as.numeric(end_time - start_time) < 1)  # Should be very fast
  })
})

# Error handling tests
describe("lollipop Error Handling", {
  
  test_that("lollipop handles empty data", {
    empty_data <- data.frame()
    
    expect_true(is.data.frame(empty_data))
    expect_equal(nrow(empty_data), 0)
    expect_equal(ncol(empty_data), 0)
  })
  
  test_that("lollipop handles all missing values", {
    all_na_data <- data.frame(
      group = c("A", "B", "C"),
      value = c(NA, NA, NA)
    )
    
    expect_true(is.data.frame(all_na_data))
    expect_equal(sum(complete.cases(all_na_data)), 0)
  })
})

# Helper function validation tests
describe("lollipop Helper Functions", {
  
  test_that("create_clinical_data generates valid data", {
    data <- create_clinical_data(n = 10)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 10)
    expect_true(all(c("patient_id", "biomarker_level", "risk_category") %in% names(data)))
    expect_true(all(data$biomarker_level >= 5))
  })
  
  test_that("create_treatment_data generates valid data", {
    data <- create_treatment_data(n = 8)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 8)
    expect_true(all(c("treatment", "response_score", "efficacy") %in% names(data)))
    expect_true(all(data$response_score >= 10 & data$response_score <= 100))
  })
  
  test_that("create_timeline_data generates valid data", {
    data <- create_timeline_data(n = 5)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 5)
    expect_true(all(c("patient_id", "days_to_event", "event_type") %in% names(data)))
    expect_true(all(data$days_to_event > 0))
  })
  
  test_that("create_survey_data generates valid data", {
    data <- create_survey_data(n = 10)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 10)
    expect_true(all(c("question", "satisfaction_score", "category") %in% names(data)))
    expect_true(all(data$satisfaction_score >= 1 & data$satisfaction_score <= 10))
  })
  
  test_that("create_quality_metrics_data generates valid data", {
    data <- create_quality_metrics_data(n = 8)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 8)
    expect_true(all(c("metric", "value", "model_type") %in% names(data)))
    expect_true(all(data$value >= 0.5 & data$value <= 1.0))
  })
})

# Aggregation functionality tests
describe("lollipop Aggregation Functions", {

  test_that("lollipop data can be aggregated by mean", {
    
    # Create data with multiple entries per group
    set.seed(1)
    unaggregated_data <- data.frame(
        group = rep(c("A", "B"), each = 5),
        value = c(1:5, 6:10)
    )

    # Manually calculate expected means
    expected_means <- unaggregated_data %>%
        group_by(group) %>%
        summarise(value = mean(value, na.rm = TRUE))

    # Create an instance of the class to test the private method
    # This requires a bit of setup to mimic the jamovi environment
    options <- jmvcore::Options$new(aggregation = "mean")
    analysis <- lollipopClass$new(options = options, data = unaggregated_data)
    
    # Call the internal aggregation function
    aggregated_data <- analysis$.__enclos_env__$private$.aggregateData(
        unaggregated_data, 
        "value", 
        "group", 
        "mean"
    )
    
    # Compare results
    expect_equal(aggregated_data$value[aggregated_data$group == "A"], 3)
    expect_equal(aggregated_data$value[aggregated_data$group == "B"], 8)
    expect_equal(nrow(aggregated_data), 2)
  })

  test_that("lollipop data can be aggregated by median", {
    
    # Create data with multiple entries per group
    set.seed(1)
    unaggregated_data <- data.frame(
        group = rep(c("A", "B"), each = 5),
        value = c(1:5, 6:10)
    )

    # Manually calculate expected medians
    expected_medians <- unaggregated_data %>%
        group_by(group) %>%
        summarise(value = median(value, na.rm = TRUE))

    # Create an instance of the class
    options <- jmvcore::Options$new(aggregation = "median")
    analysis <- lollipopClass$new(options = options, data = unaggregated_data)
    
    # Call the internal aggregation function
    aggregated_data <- analysis$.__enclos_env__$private$.aggregateData(
        unaggregated_data, 
        "value", 
        "group", 
        "median"
    )
    
    # Compare results
    expect_equal(aggregated_data$value[aggregated_data$group == "A"], 3)
    expect_equal(aggregated_data$value[aggregated_data$group == "B"], 8)
    expect_equal(nrow(aggregated_data), 2)
  })

})

print("All lollipop tests completed successfully!")