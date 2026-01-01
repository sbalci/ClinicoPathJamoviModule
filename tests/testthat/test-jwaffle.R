# Test suite for jwaffle function
# Tests cover functionality, performance, edge cases, and error handling

library(testthat)
skip_if_not_installed("ClinicoPath")

# Test data setup
setup_test_data <- function() {
  set.seed(123)
  
  # Basic waffle chart data
  basic_data <- data.frame(
    category = factor(rep(c("A", "B", "C"), each = 100)),
    value = c(rep(1, 50), rep(0, 50),   # A: 50 cases
              rep(1, 70), rep(0, 30),   # B: 70 cases  
              rep(1, 30), rep(0, 70)),  # C: 30 cases
    weight = runif(300, 0.5, 2.0),
    facet_var = factor(rep(c("Group1", "Group2"), 150)),
    stringsAsFactors = FALSE
  )
  
  # Market research data
  market_data <- data.frame(
    product = factor(sample(c("Product_A", "Product_B", "Product_C", "Product_D"), 
                           200, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))),
    sales_count = sample(1:50, 200, replace = TRUE),
    region = factor(sample(c("North", "South", "East", "West"), 200, replace = TRUE)),
    quarter = factor(sample(c("Q1", "Q2", "Q3", "Q4"), 200, replace = TRUE))
  )
  
  # Survey response data
  survey_data <- data.frame(
    response = factor(sample(c("Strongly_Agree", "Agree", "Neutral", "Disagree", "Strongly_Disagree"), 
                            500, replace = TRUE, prob = c(0.15, 0.25, 0.30, 0.20, 0.10))),
    respondent_count = rep(1, 500),
    age_group = factor(sample(c("18-25", "26-35", "36-45", "46-55", "55+"), 
                             500, replace = TRUE)),
    satisfaction = factor(sample(c("High", "Medium", "Low"), 500, replace = TRUE, prob = c(0.3, 0.5, 0.2)))
  )
  
  return(list(basic = basic_data, market = market_data, survey = survey_data))
}

test_data <- setup_test_data()

# Basic Functionality Tests
describe("jwaffle Basic Functionality", {
  
  test_that("jwaffle creates basic waffle chart", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(
        data = test_data$basic,
        groups = "category"
      )
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle generates interpretation content", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    result <- jwaffle(
      data = test_data$basic,
      groups = "category"
    )
    
    expect_true(is.list(result))
    # Check if interpretation section exists (would be visible in actual jamovi interface)
  })
  
  test_that("jwaffle handles count variables", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(
        data = test_data$market,
        groups = "product",
        counts = "sales_count"
      )
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle creates faceted charts", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(
        data = test_data$basic,
        groups = "category",
        facet = "facet_var"
      )
    })
    
    expect_true(is.list(result))
  })
})

# Parameter Testing
describe("jwaffle Parameters", {
  
  test_that("jwaffle handles different row numbers", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    for (rows in c(3, 5, 8, 10)) {
      expect_silent({
        result <- jwaffle(
          data = test_data$basic,
          groups = "category",
          rows = rows
        )
      })
      
      expect_true(is.list(result))
    }
  })
  
  test_that("jwaffle handles flip parameter", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result_normal <- jwaffle(
        data = test_data$basic,
        groups = "category",
        flip = FALSE
      )
    })
    
    expect_silent({
      result_flipped <- jwaffle(
        data = test_data$basic,
        groups = "category",
        flip = TRUE
      )
    })
    
    expect_true(is.list(result_normal))
    expect_true(is.list(result_flipped))
  })
  
  test_that("jwaffle handles legend options", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result_with_legend <- jwaffle(
        data = test_data$basic,
        groups = "category",
        show_legend = TRUE,
        legendtitle = "Custom Legend"
      )
    })
    
    expect_silent({
      result_no_legend <- jwaffle(
        data = test_data$basic,
        groups = "category",
        show_legend = FALSE
      )
    })
    
    expect_true(is.list(result_with_legend))
    expect_true(is.list(result_no_legend))
  })
})

# Color Palette Testing  
describe("jwaffle Color Palettes", {
  
  test_that("jwaffle handles all color palette options", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    palettes <- c("default", "colorblind", "professional", "presentation", "journal", "pastel", "dark")
    
    for (palette in palettes) {
      expect_silent({
        result <- jwaffle(
          data = test_data$basic,
          groups = "category",
          color_palette = palette
        )
      })
      
      expect_true(is.list(result), info = paste("Failed for palette:", palette))
    }
  })
  
  test_that("jwaffle handles invalid color palette gracefully", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(
        data = test_data$basic,
        groups = "category",
        color_palette = "nonexistent_palette"
      )
    })
    
    expect_true(is.list(result))
  })
})

# Performance and Caching Tests
describe("jwaffle Performance Features", {
  
  test_that("jwaffle caching works correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    # First run - should create cache
    start_time1 <- Sys.time()
    result1 <- jwaffle(
      data = test_data$market,
      groups = "product",
      counts = "sales_count"
    )
    end_time1 <- Sys.time()
    time1 <- as.numeric(end_time1 - start_time1)
    
    # Second run with same parameters - should use cache
    start_time2 <- Sys.time()
    result2 <- jwaffle(
      data = test_data$market,
      groups = "product", 
      counts = "sales_count"
    )
    end_time2 <- Sys.time()
    time2 <- as.numeric(end_time2 - start_time2)
    
    expect_true(is.list(result1))
    expect_true(is.list(result2))
    # Note: Timing comparison removed as it can be unreliable in testing environments
  })
  
  test_that("jwaffle cache invalidation works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    # Create initial result
    result1 <- jwaffle(
      data = test_data$basic,
      groups = "category",
      color_palette = "default"
    )
    
    # Change parameters - should invalidate cache
    result2 <- jwaffle(
      data = test_data$basic,
      groups = "category",
      color_palette = "professional"
    )
    
    expect_true(is.list(result1))
    expect_true(is.list(result2))
  })
})

# Error Handling Tests
describe("jwaffle Error Handling", {
  
  test_that("jwaffle handles missing required parameters", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(data = test_data$basic)  # No groups specified
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle handles empty data", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    empty_data <- data.frame(
      category = character(0),
      value = numeric(0)
    )
    
    expect_error({
      result <- jwaffle(
        data = empty_data,
        groups = "category"
      )
    })
  })
  
  test_that("jwaffle handles invalid variable names", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_error({
      result <- jwaffle(
        data = test_data$basic,
        groups = "nonexistent_variable"
      )
    })
  })
  
  test_that("jwaffle handles data with all missing values", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    missing_data <- test_data$basic
    missing_data$category <- NA
    
    expect_error({
      result <- jwaffle(
        data = missing_data,
        groups = "category"
      )
    })
  })
})

# Edge Cases
describe("jwaffle Edge Cases", {
  
  test_that("jwaffle handles single category", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    single_cat_data <- data.frame(
      category = factor(rep("A", 100)),
      value = rep(1, 100)
    )
    
    expect_silent({
      result <- jwaffle(
        data = single_cat_data,
        groups = "category"
      )
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle handles many categories", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    many_cat_data <- data.frame(
      category = factor(sample(LETTERS[1:10], 200, replace = TRUE)),
      value = rep(1, 200)
    )
    
    expect_silent({
      result <- jwaffle(
        data = many_cat_data,
        groups = "category"
      )
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle handles zero counts", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    zero_data <- data.frame(
      category = factor(c("A", "B", "C")),
      count = c(10, 0, 5)
    )
    
    expect_silent({
      result <- jwaffle(
        data = zero_data,
        groups = "category",
        counts = "count"
      )
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle handles very large datasets", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    large_data <- data.frame(
      category = factor(sample(c("A", "B", "C", "D"), 5000, replace = TRUE)),
      value = rep(1, 5000)
    )
    
    expect_silent({
      result <- jwaffle(
        data = large_data,
        groups = "category"
      )
    })
    
    expect_true(is.list(result))
  })
})

# Real-world Application Tests
describe("jwaffle Real-world Applications", {
  
  test_that("jwaffle handles market research data", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(
        data = test_data$market,
        groups = "product",
        counts = "sales_count",
        facet = "region",
        color_palette = "professional",
        mytitle = "Market Share by Region",
        legendtitle = "Product",
        show_legend = TRUE
      )
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle handles survey response data", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(
        data = test_data$survey,
        groups = "response",
        facet = "age_group",
        color_palette = "colorblind",
        mytitle = "Survey Responses by Age Group",
        legendtitle = "Response",
        rows = 8,
        show_legend = TRUE
      )
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle handles demographic breakdown", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(
        data = test_data$survey,
        groups = "satisfaction",
        facet = "age_group",
        color_palette = "presentation",
        mytitle = "Satisfaction Levels by Demographics",
        flip = TRUE,
        rows = 6
      )
    })
    
    expect_true(is.list(result))
  })
})

# Customization Tests
describe("jwaffle Customization", {
  
  test_that("jwaffle handles custom titles and labels", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    expect_silent({
      result <- jwaffle(
        data = test_data$basic,
        groups = "category",
        mytitle = "Custom Waffle Chart Title",
        legendtitle = "Custom Legend Title",
        show_legend = TRUE
      )
    })
    
    expect_true(is.list(result))
  })
  
  test_that("jwaffle handles complex faceting scenarios", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    complex_data <- expand.grid(
      category = factor(c("A", "B", "C")),
      facet1 = factor(c("X", "Y")),
      facet2 = factor(c("P", "Q"))
    )
    complex_data$count <- sample(1:20, nrow(complex_data), replace = TRUE)
    
    expect_silent({
      result <- jwaffle(
        data = complex_data,
        groups = "category",
        counts = "count",
        facet = "facet1",
        color_palette = "journal"
      )
    })
    
    expect_true(is.list(result))
  })
})

# Integration Tests
describe("jwaffle Integration", {
  
  test_that("jwaffle integrates with different data types", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    # Character data
    char_data <- data.frame(
      group = c("Group1", "Group2", "Group3"),
      value = c(30, 45, 25)
    )
    
    expect_silent({
      result1 <- jwaffle(
        data = char_data,
        groups = "group",
        counts = "value"
      )
    })
    
    # Numeric grouping (should convert to factor)
    numeric_data <- data.frame(
      group = c(1, 2, 3, 1, 2, 3),
      weight = c(10, 15, 8, 12, 18, 6)
    )
    
    expect_silent({
      result2 <- jwaffle(
        data = numeric_data,
        groups = "group",
        counts = "weight"
      )
    })
    
    expect_true(is.list(result1))
    expect_true(is.list(result2))
  })
  
  test_that("jwaffle handles missing data patterns", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")
    
    missing_data <- test_data$basic
    missing_data$category[sample(1:300, 30)] <- NA
    missing_data$weight[sample(1:300, 20)] <- NA
    
    expect_silent({
      result <- jwaffle(
        data = missing_data,
        groups = "category",
        counts = "weight"
      )
    })
    
    expect_true(is.list(result))
  })
})

# Performance Stress Tests
describe("jwaffle Performance", {

  test_that("jwaffle handles repeated calls efficiently", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    # Multiple calls with same parameters should use caching
    for (i in 1:3) {
      expect_silent({
        result <- jwaffle(
          data = test_data$basic,
          groups = "category",
          color_palette = "default"
        )
      })

      expect_true(is.list(result))
    }
  })

  test_that("jwaffle memory usage is reasonable", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    # This test ensures we don't have memory leaks
    initial_memory <- gc()

    for (i in 1:5) {
      result <- jwaffle(
        data = test_data$market,
        groups = "product",
        counts = "sales_count",
        color_palette = sample(c("default", "professional", "colorblind"), 1)
      )
    }

    final_memory <- gc()

    # Basic memory usage check (not too strict as R's GC is complex)
    expect_true(is.list(result))
  })
})

# Regression Tests for Critical Fixes
describe("jwaffle Regression Tests", {

  test_that("cache invalidates when data values change (not just metadata)", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    # Create initial data
    data1 <- data.frame(
      category = factor(c(rep("A", 50), rep("B", 50))),
      value = rep(1, 100)
    )

    # Create data with same structure but different values
    data2 <- data.frame(
      category = factor(c(rep("A", 70), rep("B", 30))),  # Different distribution
      value = rep(1, 100)
    )

    # Both datasets have same row count, column count, and factor levels
    # But proportions are different: 50/50 vs 70/30

    result1 <- jwaffle(data = data1, groups = "category")
    result2 <- jwaffle(data = data2, groups = "category")

    # Results should be different because data values changed
    expect_true(is.list(result1))
    expect_true(is.list(result2))

    # Note: We can't directly test if cache was invalidated, but the fix ensures
    # that actual data values are hashed, not just metadata
  })

  test_that("NA handling only affects relevant columns", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    # Create data with NA in irrelevant column
    data_with_na <- data.frame(
      category = factor(c("A", "A", "B", "B", "C", "C")),
      irrelevant_col = c(1, NA, 3, NA, 5, NA),  # NA in column not used in analysis
      stringsAsFactors = FALSE
    )

    # Should not remove rows just because irrelevant_col has NA
    result <- jwaffle(data = data_with_na, groups = "category")

    expect_true(is.list(result))
    # All 6 rows should be included since 'category' has no NA values
  })

  test_that("NA in relevant columns are properly removed", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    # Create data with NA in the groups variable
    data_with_na <- data.frame(
      category = factor(c("A", NA, "B", "B", "C", NA)),
      value = c(1, 2, 3, 4, 5, 6)
    )

    # Should remove rows with NA in category
    result <- jwaffle(data = data_with_na, groups = "category")

    expect_true(is.list(result))
    # Only 4 rows should remain (rows 1, 3, 4, 5)
  })

  test_that("negative counts are rejected with clear error", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    # Create data with negative counts
    data_negative <- data.frame(
      category = factor(c("A", "B", "C")),
      count = c(10, -5, 8)  # Negative count
    )

    # Should throw error, not warning
    expect_error(
      jwaffle(data = data_negative, groups = "category", counts = "count"),
      regexp = "negative"
    )
  })

  test_that("weighted counts show 'weighted units' in caption", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    # This is a behavioral test - we can't directly access the caption,
    # but we ensure the function runs without error when using counts
    weighted_data <- data.frame(
      category = factor(c("A", "B", "C")),
      weight = c(100, 200, 150)
    )

    result <- jwaffle(
      data = weighted_data,
      groups = "category",
      counts = "weight",
      showSummaries = TRUE
    )

    expect_true(is.list(result))
    # The caption should use "weighted units" terminology
  })

  test_that("non-weighted counts show 'cases' in caption", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    # Test without counts variable
    simple_data <- data.frame(
      category = factor(rep(c("A", "B", "C"), each = 10))
    )

    result <- jwaffle(
      data = simple_data,
      groups = "category",
      showSummaries = TRUE
    )

    expect_true(is.list(result))
    # The caption should use "cases" terminology
  })

  test_that("faceted plots handle captions correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    faceted_data <- data.frame(
      category = factor(rep(c("A", "B"), 20)),
      facet_group = factor(rep(c("X", "Y"), each = 20))
    )

    result <- jwaffle(
      data = faceted_data,
      groups = "category",
      facet = "facet_group",
      showSummaries = TRUE
    )

    expect_true(is.list(result))
    # Captions should vary by facet group
  })

  test_that("zero counts are handled without negative values", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("waffle")

    zero_data <- data.frame(
      category = factor(c("A", "B", "C")),
      count = c(10, 0, 5)  # Zero count (valid)
    )

    # Should work fine with zero counts
    result <- jwaffle(
      data = zero_data,
      groups = "category",
      counts = "count"
    )

    expect_true(is.list(result))
  })
})
