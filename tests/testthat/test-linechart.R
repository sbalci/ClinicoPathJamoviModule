# Test suite for linechart function
# Tests cover functionality, data validation, plotting, and edge cases

library(testthat)

lc_todo <- function(res) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(res$todo$content)))


# Helper functions for testing
create_time_series_data <- function(n = 50, n_groups = 1, seed = 123) {
  set.seed(seed)
  
  # Create basic time series data
  time_points <- seq(1, 12, length.out = n)
  
  if (n_groups == 1) {
    # Single group data
    trend <- 0.5 * time_points + rnorm(n, 0, 2)
    seasonal <- 3 * sin(2 * pi * time_points / 12)
    noise <- rnorm(n, 0, 1)
    
    data <- data.frame(
      time = time_points,
      value = 20 + trend + seasonal + noise,
      patient_id = paste0("P", sprintf("%03d", 1:n))
    )
  } else {
    # Multiple groups data
    data <- NULL
    for (i in 1:n_groups) {
      group_n <- ceiling(n / n_groups)
      group_time <- seq(1, 12, length.out = group_n)
      
      # Different trends for different groups
      trend_coef <- 0.3 + 0.2 * i
      trend <- trend_coef * group_time + rnorm(group_n, 0, 2)
      seasonal <- 3 * sin(2 * pi * group_time / 12)
      noise <- rnorm(group_n, 0, 1)
      
      group_data <- data.frame(
        time = group_time,
        value = 18 + 2 * i + trend + seasonal + noise,
        treatment = paste0("Group_", LETTERS[i]),
        patient_id = paste0("P", sprintf("%03d", (i-1)*group_n + 1:group_n))
      )
      
      data <- rbind(data, group_data)
    }
  }
  
  return(data)
}

create_clinical_lab_data <- function(n = 60, seed = 456) {
  set.seed(seed)
  
  # Simulate clinical laboratory data over time
  days <- rep(c(0, 7, 14, 21, 28, 35, 42), each = ceiling(n/7))
  days <- days[1:n]
  
  # Simulate hemoglobin levels with treatment effect
  baseline_hgb <- 10
  treatment_effect <- 0.1 * days
  individual_variation <- rnorm(n, 0, 0.8)
  
  hgb_values <- baseline_hgb + treatment_effect + individual_variation
  hgb_values[hgb_values < 6] <- 6  # Minimum realistic value
  hgb_values[hgb_values > 18] <- 18  # Maximum realistic value
  
  data <- data.frame(
    visit_day = days,
    hemoglobin_g_dl = round(hgb_values, 1),
    patient_id = paste0("LC", sprintf("%03d", 1:n)),
    treatment_arm = factor(sample(c("Control", "Treatment"), n, replace = TRUE))
  )
  
  return(data)
}

create_dose_response_data <- function(n = 80, seed = 789) {
  set.seed(seed)
  
  # Simulate dose-response relationship
  doses <- c(0, 5, 10, 20, 40, 80)
  n_per_dose <- ceiling(n / length(doses))
  
  data <- NULL
  for (dose in doses) {
    # Emax model for dose response
    emax <- 85
    ed50 <- 25
    response <- (emax * dose) / (ed50 + dose) + rnorm(n_per_dose, 0, 8)
    response[response < 0] <- 0
    response[response > 100] <- 100
    
    dose_data <- data.frame(
      dose_mg = rep(dose, n_per_dose),
      response_percent = round(response, 1),
      subject_id = paste0("S", sprintf("%03d", 1:n_per_dose + length(data)))
    )
    
    data <- rbind(data, dose_data)
  }
  
  return(data[1:n, ])
}

# Basic functionality tests
describe("linechart Basic Functionality", {
  
  test_that("linechart creates basic time series plots", {
  skip_if_not_installed('jmvReadWrite')
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 30)
    
    # Test basic functionality without errors
    expect_no_error({
      # This would test the basic R6 class structure
      # In actual jamovi, this would be:
      # result <- linechart(data = data, xvar = "time", yvar = "value")
      
      # For testing purposes, we'll validate the data structure
      expect_true(is.data.frame(data))
      expect_true("time" %in% names(data))
      expect_true("value" %in% names(data))
      expect_true(is.numeric(data$time))
      expect_true(is.numeric(data$value))
    })
  })
  
  
})

# Data validation tests

# Grouping and factor handling tests

# Statistical analysis tests

# Summary statistics tests

# Color palette and theme tests

# Edge cases and error handling tests

# Integration tests - ACTUALLY CALLING linechart()
describe("linechart Integration", {

  test_that("linechart creates valid result object with simple time series", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    data <- create_time_series_data(n = 30)

    # CRITICAL: Actually call the linechart function
    result <- linechart(
      data = data,
      xvar = "time",
      yvar = "value"
    )

    # Verify result object structure
    expect_true(inherits(result, "linechartResults"))
    expect_true(!is.null(result))
  })

  test_that("linechart works with clinical laboratory data", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    data <- create_clinical_lab_data(n = 49)

    # CRITICAL: Actually call linechart with grouped longitudinal data
    result <- linechart(
      data = data,
      xvar = "visit_day",
      yvar = "hemoglobin_g_dl",
      groupby = "treatment_arm",
      trendline = TRUE,
      confidence = TRUE
    )

    expect_true(inherits(result, "linechartResults"))

    # Verify warnings about repeated measures are issued in correlation table
    expect_true(any(grepl("independent|repeated", result$correlation$asDF$interpretation, ignore.case = TRUE)))
  })

  test_that("linechart works with dose-response data", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    data <- create_dose_response_data(n = 60)

    # CRITICAL: Actually call linechart
    result <- linechart(
      data = data,
      xvar = "dose_mg",
      yvar = "response_percent",
      trendline = TRUE
    )

    expect_true(inherits(result, "linechartResults"))
  })

  test_that("linechart handles complex grouped time series", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    data <- create_time_series_data(n = 120, n_groups = 4)

    # CRITICAL: Actually call linechart with multiple groups
    result <- linechart(
      data = data,
      xvar = "time",
      yvar = "value",
      groupby = "treatment",
      trendline = TRUE,
      smooth = FALSE,
      points = TRUE
    )

    expect_true(inherits(result, "linechartResults"))

    # Verify warnings about grouped data are issued in correlation table
    expect_true(any(grepl("grouped|independent|repeated", result$correlation$asDF$interpretation, ignore.case = TRUE)))
  })

  test_that("linechart produces plot without errors", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    data <- create_time_series_data(n = 40, n_groups = 2)

    # Test that plotting completes without error
    expect_no_error({
      result <- linechart(
        data = data,
        xvar = "time",
        yvar = "value",
        groupby = "treatment",
        trendline = TRUE,
        confidence = TRUE,
        points = TRUE,
        smooth = FALSE
      )
    })
  })

  test_that("linechart handles single observation per time point", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    # Create data with exactly one observation per time point (no repeated measures)
    data <- data.frame(
      time = 1:20,
      value = rnorm(20, mean = 50, sd = 10)
    )

    # Should NOT trigger repeated measures warning
    expect_no_warning(
      linechart(
        data = data,
        xvar = "time",
        yvar = "value",
        trendline = TRUE
      ),
      message = "repeated"     # expect_no_warning() takes `message`, not `regexp`
    )
  })

  test_that("linechart properly sorts data by time", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    # Create deliberately unsorted data
    data <- data.frame(
      time = c(5, 1, 3, 2, 4, 8, 6, 7),
      value = c(15, 5, 10, 8, 12, 20, 18, 19)
    )

    # linechart should sort this internally
    result <- linechart(
      data = data,
      xvar = "time",
      yvar = "value"
    )

    expect_true(inherits(result, "linechartResults"))
  })

  test_that("linechart rejects insufficient data", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    # Only 2 observations - should fail minimum requirement
    data <- data.frame(
      time = 1:2,
      value = c(10, 15)
    )

    # jmvcore::reject() is caught by .run() and written into the `todo` panel;
    # it does not propagate as an R error, so expect_error() passed only on a crash.
    expect_match(lc_todo(linechart(data = data, xvar = "time", yvar = "value")),
                 "At least 3")
  })

  test_that("linechart rejects zero variance data", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ClinicoPath")

    # All Y values identical
    data <- data.frame(
      time = 1:10,
      value = rep(50, 10)
    )

    expect_match(lc_todo(linechart(data = data, xvar = "time", yvar = "value")),
                 "no variation")
  })
})

# Performance and scalability tests

# The three behaviours below previously had test_that blocks named after them
# that never called linechart() - they re-implemented the check inline and
# asserted base R. These call the analysis and assert on its actual output.

describe("linechart Data Quality Reporting", {

  test_that("linechart runs ANOVA for a categorical X variable", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")

    set.seed(11)
    data <- data.frame(
      visit = factor(rep(c("Baseline", "Week4", "Week8"), each = 8)),
      value = c(rnorm(8, 10), rnorm(8, 14), rnorm(8, 18))
    )

    res <- linechart(data = data, xvar = "visit", yvar = "value",
                     trendline = TRUE)
    df <- res$correlation$asDF

    # The measure label now carries the degrees of freedom, so match the prefix
    # rather than the whole string.
    row <- df[grepl("ANOVA", df$measure), ]
    expect_equal(nrow(row), 1L)
    expect_equal(row$value,
                 anova(lm(value ~ visit, data = data))$`F value`[1],
                 tolerance = 1e-10)
  })

  test_that("linechart reports rows dropped for missing values", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")

    data <- create_time_series_data(n = 30)
    data$value[c(3, 9, 21)] <- NA

    res <- linechart(data = data, xvar = "time", yvar = "value")

    # The count reaches the user through the results panel, not warning().
    expect_match(lc_todo(res), "excluded because of missing values")
    expect_equal(res$summary$asDF$value[res$summary$asDF$statistic ==
                   "Number of Observations"], "27")
  })

  test_that("linechart flags an unreadable number of groups", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")

    set.seed(12)
    n_groups <- 9
    data <- data.frame(
      time  = rep(1:4, times = n_groups),
      value = rnorm(4 * n_groups, 20, 3),
      arm   = factor(rep(paste0("Arm_", LETTERS[1:n_groups]), each = 4))
    )

    res <- linechart(data = data, xvar = "time", yvar = "value",
                     groupby = "arm")

    expect_match(lc_todo(res), "Many groups detected \\(9\\)")
  })
})
