# Test suite for raincloud function
# This suite performs rigorous checks on statistical outputs.

library(testthat)

# Helper function to get a clean analysis object for testing private methods
get_raincloud_analysis <- function(data, dep, group, options = list()) {
  # Set default options and merge with provided ones
  all_options <- c(
    options,
    list(
      dep_var = dep,
      group_var = group
    )
  )
  
  # Create jmvcore options object
  opts <- jmvcore::Options$new()
  for (name in names(all_options)) {
    # This is a bit of a hack to set private fields in the Options object
    private_field_name <- paste0("..", name)
    if (exists(private_field_name, envir = opts$.__enclos_env__$private)) {
        opts$.__enclos_env__$private[[private_field_name]] <- all_options[[name]]
    }
  }

  # Instantiate the class
  analysis <- raincloudClass$new(options = opts, data = data)
  return(analysis)
}


# 1. Test Summary Statistics Generation
describe("Raincloud Summary Statistics", {

  test_that(".generate_statistics calculates correct values", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("dplyr")
    
    # Create a simple, fixed dataset
    test_data <- data.frame(
      value = c(10, 20, 30, 40, 50),
      category = factor(rep("A", 5))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category")
    
    # Run the private method to get the HTML table
    stats_html <- analysis$.__enclos_env__$private$.generate_statistics(test_data, "value", "category")
    
    # Check that the output contains the correct, calculated values
    expect_true(grepl("<td><strong>A</strong></td>", stats_html))
    expect_true(grepl(">5</td>", stats_html)) # N
    expect_true(grepl(">30</td>", stats_html)) # Mean
    expect_true(grepl(">30</td>", stats_html)) # Median
    expect_true(grepl(round(sd(test_data$value), 3), stats_html)) # SD
    expect_true(grepl(round(IQR(test_data$value), 3), stats_html)) # IQR
  })
})


# 2. Test Outlier Detection
describe("Raincloud Outlier Detection", {

  test_that(".generate_outlier_analysis correctly finds outliers (IQR)", {
    skip_if_not_installed("jmvcore")
    
    # Create data with a clear outlier
    test_data <- data.frame(
      value = c(1, 2, 3, 4, 5, 100),
      category = factor(rep("A", 6))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category", 
                                     options = list(outlier_method = "iqr"))
    
    outlier_html <- analysis$.__enclos_env__$private$.generate_outlier_analysis(test_data, "value", "category")
    
    # Expect to find 1 outlier (the value 100)
    expect_true(grepl("<li><strong>A:</strong> 1 outliers detected</li>", outlier_html))
    expect_true(grepl("<strong>Total outliers across all groups:</strong> 1", outlier_html))
  })
})


# 3. Test Normality Testing
describe("Raincloud Normality Testing", {

  test_that(".generate_normality_tests works for normal data", {
    skip_if_not_installed("jmvcore")
    set.seed(123)
    test_data <- data.frame(
      value = rnorm(50, mean = 10, sd = 2),
      category = factor(rep("A", 50))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category")
    normality_html <- analysis$.__enclos_env__$private$.generate_normality_tests(test_data, "value", "category")
    
    # Expect p > 0.05 for normal data
    expect_true(grepl("<td>Normal</td>", normality_html))
  })

  test_that(".generate_normality_tests works for non-normal data", {
    skip_if_not_installed("jmvcore")
    test_data <- data.frame(
      value = c(rep(1, 20), rep(10, 20)), # Bimodal distribution
      category = factor(rep("A", 40))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category")
    normality_html <- analysis$.__enclos_env__$private$.generate_normality_tests(test_data, "value", "category")
    
    # Expect p < 0.05 for non-normal data
    expect_true(grepl("<td>Non-normal</td>", normality_html))
  })
})


# 4. Test Group Comparison Logic
describe("Raincloud Group Comparison", {

  test_that("auto method chooses t-test for two normal groups", {
    skip_if_not_installed("jmvcore")
    set.seed(1)
    test_data <- data.frame(
      value = c(rnorm(20, 10, 2), rnorm(20, 15, 2)),
      category = factor(rep(c("A", "B"), each = 20))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category", 
                                     options = list(comparison_method = "auto"))
    
    comparison_html <- analysis$.__enclos_env__$private$.generate_group_comparisons(test_data, "value", "category")
    
    # Check that "t-test" was chosen and the result is significant
    expect_true(grepl("<strong>Test Method:</strong></td><td.+>t-test</td>", comparison_html))
    expect_true(grepl("<strong>Result:</strong></td><td.+>Highly significant", comparison_html))
  })

  test_that("auto method chooses Wilcoxon for two non-normal groups", {
    skip_if_not_installed("jmvcore")
    set.seed(1)
    test_data <- data.frame(
      value = c(rexp(20, 1), rexp(20, 0.1)),
      category = factor(rep(c("A", "B"), each = 20))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category", 
                                     options = list(comparison_method = "auto"))
    
    comparison_html <- analysis$.__enclos_env__$private$.generate_group_comparisons(test_data, "value", "category")
    
    # Check that "Wilcoxon" was chosen
    expect_true(grepl("<strong>Test Method:</strong></td><td.+>Wilcoxon</td>", comparison_html))
  })

  test_that("auto method chooses ANOVA for three normal groups", {
    skip_if_not_installed("jmvcore")
    set.seed(1)
    test_data <- data.frame(
      value = c(rnorm(20, 10, 2), rnorm(20, 15, 2), rnorm(20, 20, 2)),
      category = factor(rep(c("A", "B", "C"), each = 20))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category", 
                                     options = list(comparison_method = "auto"))
    
    comparison_html <- analysis$.__enclos_env__$private$.generate_group_comparisons(test_data, "value", "category")
    
    # Check that "ANOVA" was chosen
    expect_true(grepl("<strong>Test Method:</strong></td><td.+>ANOVA</td>", comparison_html))
  })

  test_that("auto method chooses Kruskal-Wallis for three non-normal groups", {
    skip_if_not_installed("jmvcore")
    set.seed(1)
    test_data <- data.frame(
      value = c(rexp(20, 1), rexp(20, 0.5), rexp(20, 0.1)),
      category = factor(rep(c("A", "B", "C"), each = 20))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category", 
                                     options = list(comparison_method = "auto"))
    
    comparison_html <- analysis$.__enclos_env__$private$.generate_group_comparisons(test_data, "value", "category")
    
    # Check that "Kruskal-Wallis" was chosen
    expect_true(grepl("<strong>Test Method:</strong></td><td.+>Kruskal-Wallis</td>", comparison_html))
  })

  test_that("t-test calculation is correct", {
    skip_if_not_installed("jmvcore")
    
    # Simple, fixed data
    test_data <- data.frame(
      value = c(1, 2, 3, 10, 11, 12),
      category = factor(rep(c("A", "B"), each = 3))
    )
    
    # Manual t-test
    manual_test <- t.test(value ~ category, data = test_data)
    
    analysis <- get_raincloud_analysis(test_data, "value", "category", 
                                     options = list(comparison_method = "ttest"))
    
    comparison_html <- analysis$.__enclos_env__$private$.generate_group_comparisons(test_data, "value", "category")
    
    # Extract p-value from HTML and compare
    p_value_from_html_str <- regmatches(comparison_html, regexpr('P-value:</strong></td><td[^>]*>([0-9.]+)', comparison_html))
    p_value_from_html <- as.numeric(sub('<[^>]+>', '', sub('P-value:</strong></td><td[^>]*>', '', p_value_from_html_str)))

    expect_equal(p_value_from_html, round(manual_test$p.value, 4))
    expect_true(grepl(paste0("t = ", round(manual_test$statistic, 4)), comparison_html))
  })
})
