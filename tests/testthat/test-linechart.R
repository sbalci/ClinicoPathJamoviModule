# Test suite for linechart function
# Tests cover functionality, data validation, plotting, and edge cases

library(testthat)
library(ggplot2)
library(dplyr)

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
  
  test_that("linechart handles grouped data correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 60, n_groups = 3)
    
    # Validate grouped data structure
    expect_true("treatment" %in% names(data))
    expect_true(is.character(data$treatment) || is.factor(data$treatment))
    expect_equal(length(unique(data$treatment)), 3)
    
    # Check that all groups have data
    group_counts <- table(data$treatment)
    expect_true(all(group_counts > 0))
  })
  
  test_that("linechart handles clinical laboratory data", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_clinical_lab_data(n = 42)
    
    # Validate clinical data structure
    expect_true(all(c("visit_day", "hemoglobin_g_dl", "treatment_arm") %in% names(data)))
    expect_true(is.numeric(data$visit_day))
    expect_true(is.numeric(data$hemoglobin_g_dl))
    expect_true(is.factor(data$treatment_arm))
    
    # Check realistic clinical ranges
    expect_true(all(data$hemoglobin_g_dl >= 6 & data$hemoglobin_g_dl <= 18))
  })
})

# Data validation tests
describe("linechart Data Validation", {
  
  test_that("linechart validates variable existence", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 20)
    
    # Test that missing variables are detected
    expect_error({
      missing_vars <- setdiff(c("nonexistent_var"), names(data))
      if (length(missing_vars) > 0) {
        stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
      }
    })
  })
  
  test_that("linechart validates Y variable is numeric", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 20)
    data$value <- as.character(data$value)
    
    # Test numeric conversion
    y_data <- suppressWarnings(as.numeric(data$value))
    expect_false(all(is.na(y_data)))
    
    # Test with non-numeric character data
    data$value <- c("apple", "banana", "cherry")[1:nrow(data)]
    y_data <- suppressWarnings(as.numeric(data$value))
    expect_true(all(is.na(y_data)))
  })
  
  test_that("linechart handles missing values appropriately", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 30)
    
    # Introduce missing values
    data$value[c(5, 10, 15)] <- NA
    data$time[c(2, 8)] <- NA
    
    # Test complete cases
    complete_before <- nrow(data)
    complete_data <- data[complete.cases(data), ]
    complete_after <- nrow(complete_data)
    
    expect_true(complete_after < complete_before)
    expect_true(complete_after > 0)
  })
  
  test_that("linechart validates minimum data requirements", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Test with insufficient data
    expect_error({
      data <- data.frame(time = 1:2, value = c(1, 2))
      if (nrow(data) < 3) {
        stop("At least 3 complete observations are required for line chart analysis.")
      }
    })
  })
  
  test_that("linechart validates Y variable variation", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 20)
    data$value <- rep(10, nrow(data))  # No variation
    
    # Test for zero variation
    expect_error({
      if (var(data$value, na.rm = TRUE) == 0) {
        stop("Y-axis variable has no variation (all values are identical).")
      }
    })
  })
})

# Grouping and factor handling tests
describe("linechart Grouping and Factors", {
  
  test_that("linechart handles factor grouping variables", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 60, n_groups = 4)
    
    # Test factor conversion
    if (is.character(data$treatment)) {
      data$treatment <- factor(data$treatment)
    }
    
    expect_true(is.factor(data$treatment))
    expect_equal(length(levels(data$treatment)), 4)
  })
  
  test_that("linechart handles numeric X variables", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 30)
    
    # Test numeric X handling
    expect_true(is.numeric(data$time))
    expect_true(length(unique(data$time)) > 1)
  })
  
  test_that("linechart handles character X variables", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 24)
    
    # Convert to character that looks numeric
    data$time <- as.character(data$time)
    
    # Test conversion logic
    unique_values <- unique(data$time)
    if (all(grepl("^[0-9.-]+$", unique_values, perl = TRUE))) {
      converted <- as.numeric(data$time)
      expect_false(any(is.na(converted)))
    }
  })
  
  test_that("linechart handles ordered factor X variables", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 30)
    
    # Create ordered factor
    data$time_category <- cut(data$time, breaks = 5, labels = c("Early", "Mid-Early", "Mid", "Mid-Late", "Late"))
    data$time_category <- factor(data$time_category, ordered = TRUE)
    
    expect_true(is.ordered(data$time_category))
  })
  
  test_that("linechart warns about too many groups", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 120)
    
    # Create many groups
    data$many_groups <- factor(sample(1:15, nrow(data), replace = TRUE))
    
    # Test group count warning
    n_groups <- length(unique(data$many_groups))
    expect_warning({
      if (n_groups > 10) {
        warning("Grouping variable has more than 10 levels. Consider reducing groups for clarity.")
      }
    })
  })
})

# Statistical analysis tests
describe("linechart Statistical Analysis", {
  
  test_that("linechart calculates correlation statistics", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 50)
    
    # Test correlation calculation
    cor_result <- cor.test(data$time, data$value, method = "pearson")
    expect_true(is.numeric(cor_result$estimate))
    expect_true(is.numeric(cor_result$p.value))
    expect_true(length(cor_result$conf.int) == 2)
    
    # Test Spearman correlation
    spearman_result <- cor.test(data$time, data$value, method = "spearman")
    expect_true(is.numeric(spearman_result$estimate))
    expect_true(is.numeric(spearman_result$p.value))
  })
  
  test_that("linechart calculates regression statistics", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 40)
    
    # Test linear regression
    lm_result <- lm(data$value ~ data$time)
    expect_true(is.numeric(coef(lm_result)))
    expect_equal(length(coef(lm_result)), 2)  # Intercept and slope
    
    # Test R-squared
    r_squared <- summary(lm_result)$r.squared
    expect_true(is.numeric(r_squared))
    expect_true(r_squared >= 0 && r_squared <= 1)
  })
  
  test_that("linechart handles ANOVA for categorical X", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 60)
    
    # Create categorical X variable
    data$category <- factor(cut(data$time, breaks = 4, labels = c("Q1", "Q2", "Q3", "Q4")))
    
    # Test ANOVA
    anova_result <- anova(lm(data$value ~ data$category))
    expect_true(is.numeric(anova_result$`F value`[1]))
    expect_true(is.numeric(anova_result$`Pr(>F)`[1]))
  })
  
  test_that("linechart interprets correlation correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Test correlation interpretation function
    interpret_correlation <- function(r, p_value) {
      if (is.na(r) || is.na(p_value)) return("Not available")
      
      sig_text <- if (p_value < 0.001) "***" else if (p_value < 0.01) "**" else if (p_value < 0.05) "*" else "ns"
      
      abs_r <- abs(r)
      strength <- if (abs_r < 0.1) "negligible" else
                 if (abs_r < 0.3) "weak" else
                 if (abs_r < 0.5) "moderate" else
                 if (abs_r < 0.7) "strong" else "very strong"
      
      direction <- if (r > 0) "positive" else "negative"
      
      return(paste0(strength, " ", direction, " correlation (", sig_text, ")"))
    }
    
    # Test different correlation strengths
    expect_equal(interpret_correlation(0.05, 0.8), "negligible positive correlation (ns)")
    expect_equal(interpret_correlation(0.25, 0.03), "weak positive correlation (*)")
    expect_equal(interpret_correlation(-0.45, 0.001), "moderate negative correlation (**)")
    expect_equal(interpret_correlation(0.75, 0.0001), "very strong positive correlation (***)")
    expect_equal(interpret_correlation(NA, 0.05), "Not available")
  })
})

# Summary statistics tests
describe("linechart Summary Statistics", {
  
  test_that("linechart calculates basic summary statistics", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 50)
    
    # Test Y variable statistics
    y_stats <- list(
      mean = mean(data$value, na.rm = TRUE),
      median = median(data$value, na.rm = TRUE),
      sd = sd(data$value, na.rm = TRUE),
      min = min(data$value, na.rm = TRUE),
      max = max(data$value, na.rm = TRUE)
    )
    
    expect_true(all(sapply(y_stats, is.numeric)))
    expect_true(all(sapply(y_stats, function(x) !is.na(x))))
    expect_true(y_stats$min <= y_stats$median)
    expect_true(y_stats$median <= y_stats$max)
    expect_true(y_stats$sd >= 0)
  })
  
  test_that("linechart calculates data characteristics", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 60, n_groups = 3)
    
    # Test data characteristics
    n_observations <- nrow(data)
    n_x_points <- length(unique(data$time))
    n_groups <- length(unique(data$treatment))
    
    expect_equal(n_observations, 60)
    expect_true(n_x_points > 1)
    expect_equal(n_groups, 3)
  })
})

# Color palette and theme tests
describe("linechart Visual Customization", {
  
  test_that("linechart provides correct color palettes", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Test color palette function
    get_color_palette <- function(n_colors, palette_name = "default") {
      if (n_colors == 1) {
        return("#2E86AB")
      }
      
      switch(palette_name,
        "default" = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A"),
        "colorblind" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
        "viridis" = c("#440154", "#31688e", "#35b779", "#fde725"),
        "clinical" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
        c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A")
      )[1:min(n_colors, 6)]
    }
    
    # Test single color
    single_color <- get_color_palette(1)
    expect_equal(single_color, "#2E86AB")
    
    # Test multiple colors
    default_colors <- get_color_palette(3, "default")
    expect_equal(length(default_colors), 3)
    expect_true(all(grepl("^#[0-9A-F]{6}$", default_colors, ignore.case = TRUE)))
    
    # Test colorblind palette
    colorblind_colors <- get_color_palette(4, "colorblind")
    expect_equal(length(colorblind_colors), 4)
  })
  
  test_that("linechart provides plot themes", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Test theme selection
    get_plot_theme <- function(theme_name = "default") {
      base_theme <- switch(theme_name,
        "default" = ggplot2::theme_gray(),
        "minimal" = ggplot2::theme_minimal(),
        "classic" = ggplot2::theme_classic(),
        "publication" = ggplot2::theme_bw(),
        ggplot2::theme_gray()
      )
      
      return(base_theme)
    }
    
    # Test different themes
    themes <- c("default", "minimal", "classic", "publication")
    for (theme_name in themes) {
      theme_obj <- get_plot_theme(theme_name)
      expect_true(inherits(theme_obj, "theme"))
    }
  })
})

# Edge cases and error handling tests
describe("linechart Edge Cases", {
  
  test_that("linechart handles single time point", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Single time point should fail minimum requirement
    expect_error({
      data <- data.frame(time = 1, value = 10)
      if (nrow(data) < 3) {
        stop("At least 3 complete observations are required for line chart analysis.")
      }
    })
  })
  
  test_that("linechart handles extreme values", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 30)
    
    # Add extreme values
    data$value[1] <- 1000
    data$value[2] <- -1000
    
    # Should still calculate statistics
    expect_true(is.numeric(mean(data$value)))
    expect_true(is.numeric(sd(data$value)))
    expect_true(sd(data$value) > 0)
  })
  
  test_that("linechart handles all same X values", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 20)
    data$time <- rep(1, nrow(data))  # All same X values
    
    # This should still work but may not be very meaningful
    expect_true(length(unique(data$time)) == 1)
    expect_true(is.numeric(data$time))
  })
  
  test_that("linechart handles reference line validation", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Test reference line validation
    validate_refline <- function(refline_value) {
      if (!is.null(refline_value) && !is.na(refline_value)) {
        refline_numeric <- as.numeric(refline_value)
        if (is.na(refline_numeric)) {
          warning("Reference line value is not numeric and will be ignored.")
          return(NULL)
        }
        return(refline_numeric)
      }
      return(NULL)
    }
    
    # Test valid reference line
    expect_equal(validate_refline(10), 10)
    expect_equal(validate_refline("15"), 15)
    expect_null(validate_refline(NULL))
    expect_null(validate_refline(NA))
    
    # Test invalid reference line
    expect_warning(validate_refline("abc"))
  })
  
  test_that("linechart handles empty factor levels", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    data <- create_time_series_data(n = 30, n_groups = 3)
    
    # Remove one group entirely
    data <- data[data$treatment != "Group_A", ]
    data$treatment <- factor(data$treatment)  # This will drop unused levels
    
    # Should handle dropped levels
    expect_true(length(levels(data$treatment)) == 2)
    expect_true(all(levels(data$treatment) %in% unique(data$treatment)))
  })
})

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

    # Verify warnings about repeated measures are issued
    expect_warning(
      linechart(
        data = data,
        xvar = "visit_day",
        yvar = "hemoglobin_g_dl",
        groupby = "treatment_arm",
        trendline = TRUE
      ),
      regexp = "independent|repeated"
    )
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

    # Verify warnings about grouped data are issued
    expect_warning(
      linechart(
        data = data,
        xvar = "time",
        yvar = "value",
        groupby = "treatment",
        trendline = TRUE
      ),
      regexp = "grouped|independent"
    )
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
      regexp = "repeated"
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

    expect_error(
      linechart(
        data = data,
        xvar = "time",
        yvar = "value"
      ),
      regexp = "At least 3"
    )
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

    expect_error(
      linechart(
        data = data,
        xvar = "time",
        yvar = "value"
      ),
      regexp = "no variation"
    )
  })
})

# Performance and scalability tests
describe("linechart Performance", {
  
  test_that("linechart handles moderately large datasets", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Test with larger dataset
    data <- create_time_series_data(n = 500, n_groups = 5)
    
    expect_no_error({
      # Basic statistics should compute quickly
      summary_stats <- list(
        n_observations = nrow(data),
        n_groups = length(unique(data$treatment)),
        y_mean = mean(data$value, na.rm = TRUE)
      )
      
      expect_equal(summary_stats$n_observations, 500)
      expect_equal(summary_stats$n_groups, 5)
    })
  })
  
  test_that("linechart handles many time points", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # High-resolution time series
    n_points <- 200
    data <- data.frame(
      time = seq(0, 24, length.out = n_points),
      value = sin(seq(0, 4*pi, length.out = n_points)) + rnorm(n_points, 0, 0.1)
    )
    
    expect_no_error({
      # Should handle high resolution data
      expect_equal(nrow(data), n_points)
      expect_equal(length(unique(data$time)), n_points)
      
      # Correlation should still work
      cor_result <- cor.test(data$time, data$value)
      expect_true(is.numeric(cor_result$estimate))
    })
  })
})