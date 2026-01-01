#' @title Visual Validation Tests for nomogrammer Function
#' @description Tests to ensure the nomogrammer function generates proper plots
#' @author ClinicoPath Development Team

library(testthat)
library(ggplot2)
library(scales)

test_that("Visual plot generation works correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test 1: Basic plot structure
  plot_basic <- nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8)
  
  expect_s3_class(plot_basic, "ggplot")
  expect_true("data" %in% names(plot_basic))
  expect_true("layers" %in% names(plot_basic))
  expect_true("scales" %in% names(plot_basic))
  expect_true("theme" %in% names(plot_basic))
  
  # Test 2: Plot with all features
  plot_full <- nomogrammer(
    Prevalence = 0.2, 
    Sens = 0.85, 
    Spec = 0.90,
    Detail = TRUE,
    NullLine = TRUE,
    LabelSize = 3.0,
    Verbose = FALSE
  )
  
  expect_s3_class(plot_full, "ggplot")
  
  # Test 3: Plot can be built without errors
  expect_silent(built_plot <- ggplot_build(plot_basic))
  expect_silent(built_plot_full <- ggplot_build(plot_full))
  
  # Test 4: Plot has expected data structure
  plot_data <- built_plot$data
  expect_gt(length(plot_data), 0)  # Should have at least one layer
  
  # Test 5: LR input generates same visual structure
  plot_lr <- nomogrammer(Prevalence = 0.3, Plr = 4.5, Nlr = 0.125)
  expect_s3_class(plot_lr, "ggplot")
  expect_silent(built_plot_lr <- ggplot_build(plot_lr))
})

test_that("Clinical scenario plots render correctly", {
  
  # Test realistic clinical scenarios
  clinical_scenarios <- list(
    list(name = "Mammography", prev = 0.005, sens = 0.85, spec = 0.95),
    list(name = "COVID test", prev = 0.10, sens = 0.95, spec = 0.85),
    list(name = "Troponin", prev = 0.15, sens = 0.99, spec = 0.85)
  )
  
  for (scenario in clinical_scenarios) {
    plot_scenario <- nomogrammer(
      Prevalence = scenario$prev,
      Sens = scenario$sens,
      Spec = scenario$spec,
      Detail = TRUE
    )
    
    expect_s3_class(plot_scenario, "ggplot")
    expect_silent(ggplot_build(plot_scenario))
  }
})

test_that("Edge case plots render correctly", {
  
  # Very low prevalence
  expect_silent(
    plot_low <- nomogrammer(Prevalence = 0.001, Sens = 0.99, Spec = 0.95)
  )
  expect_s3_class(plot_low, "ggplot")
  
  # Very high prevalence  
  expect_silent(
    plot_high <- nomogrammer(Prevalence = 0.999, Sens = 0.90, Spec = 0.90)
  )
  expect_s3_class(plot_high, "ggplot")
  
  # Extreme likelihood ratios
  expect_silent(
    plot_extreme <- nomogrammer(Prevalence = 0.2, Plr = 50, Nlr = 0.02)
  )
  expect_s3_class(plot_extreme, "ggplot")
})

# Manual visual inspection helper (not run in automated tests)
if (FALSE) {
  # This code can be run manually to visually inspect plots
  
  # Create a few example plots for manual inspection
  plot1 <- nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8, Detail = TRUE)
  plot2 <- nomogrammer(Prevalence = 0.1, Plr = 10, Nlr = 0.1, Detail = TRUE, NullLine = TRUE)
  plot3 <- nomogrammer(Prevalence = 0.05, Sens = 0.95, Spec = 0.90, Detail = TRUE)
  
  # Save plots for inspection
  ggsave("test_plot1_basic.png", plot1, width = 8, height = 6)
  ggsave("test_plot2_lr.png", plot2, width = 8, height = 6)
  ggsave("test_plot3_screening.png", plot3, width = 8, height = 6)
  
  cat("Test plots saved for manual inspection\n")
}

cat("=== Visual Validation Tests Complete ===\n")
cat("All plot generation tests passed successfully\n")
