# Test suite for raincloud function
# This suite performs rigorous checks on statistical outputs.

library(testthat)

# Helper function to get a clean analysis object for testing private methods.
#
# This used to build a bare jmvcore::Options$new() and poke `..<name>` private
# fields into it. Those fields do not exist on a generic Options object, so NO
# option was ever actually set - every test below silently ran on whatever
# self$options fell back to - and `raincloudClass` was referenced unqualified,
# which is not visible under devtools::load_all(export_all = FALSE), so the whole
# file errored out before reaching a single assertion. Use the generated
# raincloudOptions constructor, which validates and applies the options for real.
get_raincloud_analysis <- function(data, dep, group, options = list()) {
  opts <- do.call(ClinicoPath:::raincloudOptions$new,
                  utils::modifyList(list(dep_var = dep, group_var = group), options))
  ClinicoPath:::raincloudClass$new(options = opts, data = data)
}


# 1. Test Summary Statistics Generation
describe("Raincloud Summary Statistics", {

  test_that(".generate_statistics calculates correct values", {
  skip_if_not_installed('jmvReadWrite')
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
    expect_true(grepl("<strong>Total potential outliers in assessed groups:</strong> 1", outlier_html))
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
    
    # Release review: p > 0.05 no longer reads "Normal" - Shapiro-Wilk can only
    # fail to reject, so the verdict is now an absence of evidence.
    expect_true(grepl("<td>No evidence against normality</td>", normality_html))
    expect_false(grepl("<td>Normal</td>", normality_html))
  })

  test_that(".generate_normality_tests works for non-normal data", {
    skip_if_not_installed("jmvcore")
    test_data <- data.frame(
      value = c(rep(1, 20), rep(10, 20)), # Bimodal distribution
      category = factor(rep("A", 40))
    )
    
    analysis <- get_raincloud_analysis(test_data, "value", "category")
    normality_html <- analysis$.__enclos_env__$private$.generate_normality_tests(test_data, "value", "category")
    
    # Release review: "Non-normal" is now "Departs from normality".
    expect_true(grepl("<td>Departs from normality</td>", normality_html))
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
    
    # Release review: t.test() is Welch's by default, so the label now says so,
    # and the Result row reports the p-value threshold instead of the
    # editorialising "Highly significant (***)".
    expect_true(grepl("<strong>Test Method:</strong></td><td.+>Welch's t-test</td>", comparison_html))
    expect_true(grepl("<strong>Result:</strong></td><td.+>p &lt; 0.001", comparison_html))
    expect_false(grepl("Highly significant", comparison_html))
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
    
    # Release review: label is now the full test name.
    expect_true(grepl("<strong>Test Method:</strong></td><td.+>Wilcoxon rank-sum test</td>", comparison_html))
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
    
    # Release review: AUTO now checks equal variances too, so it picks ordinary
    # one-way ANOVA only when Bartlett does not reject; here the three groups
    # share sd = 2, so ordinary ANOVA is still the right answer.
    expect_true(grepl("<strong>Test Method:</strong></td><td.+>one-way ANOVA</td>", comparison_html))
    expect_gt(bartlett.test(test_data$value, test_data$category)$p.value, 0.05)
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
    
    # Release review: label is now the full test name.
    expect_true(grepl("<strong>Test Method:</strong></td><td.+>Kruskal-Wallis test</td>", comparison_html))
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
    
    # Release review: p-values now print to 3 decimals (and as the entity
    # "&lt; 0.001" below 0.001) instead of sprintf("%.4f"), which rendered a
    # vanishing p as the impossible "0.0000".
    #
    # This data gives p ~ 0.0004, i.e. BELOW the 3-decimal floor, so there is no
    # number to parse out of the cell - asserting numeric equality against
    # formatC(p, digits = 3) == "0.000" contradicted the very contract the
    # release review introduced. Assert the contract instead.
    expect_lt(manual_test$p.value, 0.001)
    p_cell <- regmatches(comparison_html,
                         regexpr("P-value:</strong></td><td[^>]*>[^<]*", comparison_html))
    expect_true(grepl("&lt; 0.001", p_cell, fixed = TRUE))
    expect_false(grepl("0.000", p_cell, fixed = TRUE))
    expect_true(grepl(paste0("t = ", round(manual_test$statistic, 4)), comparison_html))
  })

  test_that("a p-value above 0.001 is printed as the t-test's own value to 3 decimals", {
    skip_if_not_installed("jmvcore")

    set.seed(6)
    test_data <- data.frame(
      value = c(rnorm(20, 0), rnorm(20, 0.7)),
      category = factor(rep(c("A", "B"), each = 20))
    )
    manual_test <- t.test(value ~ category, data = test_data)
    expect_gt(manual_test$p.value, 0.001)

    analysis <- get_raincloud_analysis(test_data, "value", "category",
                                     options = list(comparison_method = "ttest"))
    comparison_html <- analysis$.__enclos_env__$private$.generate_group_comparisons(test_data, "value", "category")

    p_cell <- regmatches(comparison_html,
                         regexpr("P-value:</strong></td><td[^>]*>[^<]*", comparison_html))
    p_value_from_html <- as.numeric(sub("P-value:</strong></td><td[^>]*>", "", p_cell))
    expect_equal(p_value_from_html,
                 as.numeric(formatC(manual_test$p.value, format = "f", digits = 3)))
  })
})
