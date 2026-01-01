test_that("advancedraincloud produces valid ggplot objects", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  skip_if_not_installed("ggrain")
  
  # Create a temporary directory for saving plots
  tmp_dir <- tempdir()
  
  # Test 1: Basic plot
  p1 <- advancedraincloud(
    data = histopathology,
    y_var = "Age",
    x_var = "Group"
  )
  
  # Force render to populate state
  capture.output(print(p1$plot))
  
  expect_true(ggplot2::is.ggplot(p1$plot$state))
  
  # Save to verify it renders
  expect_error({
    ggplot2::ggsave(
      filename = file.path(tmp_dir, "test_rain_basic.png"),
      plot = p1$plot$state,
      width = 8, height = 6
    )
  }, NA)
  
  # Test 2: Longitudinal plot
  # Create synthetic longitudinal data
  set.seed(123)
  n_subjects <- 20
  long_data <- data.frame(
    ID = rep(1:n_subjects, each = 2),
    Time = rep(c("Pre", "Post"), n_subjects),
    Score = rnorm(n_subjects * 2, mean = 50, sd = 10) + rep(rnorm(n_subjects, 0, 5), each = 2)
  )
  # Add effect
  long_data$Score[long_data$Time == "Post"] <- long_data$Score[long_data$Time == "Post"] + 5
  
  p2 <- advancedraincloud(
    data = long_data,
    y_var = "Score",
    x_var = "Time",
    id_var = "ID",
    show_longitudinal = TRUE,
    rain_side = "f1x1"
  )
  
  # Force render
  capture.output(print(p2$plot))
  
  expect_true(ggplot2::is.ggplot(p2$plot$state))
  
  expect_error({
    ggplot2::ggsave(
      filename = file.path(tmp_dir, "test_rain_longitudinal.png"),
      plot = p2$plot$state,
      width = 8, height = 6
    )
  }, NA)
  
  # Test 3: Complex options (Likert, Covariate, etc.)
  p3 <- advancedraincloud(
    data = histopathology,
    y_var = "Age",
    x_var = "Group",
    cov_var = "OverallTime", # Continuous covariate
    likert_mode = TRUE,
    rain_side = "r",
    color_palette = "viridis"
  )
  
  # Force render
  capture.output(print(p3$plot))
  
  expect_true(ggplot2::is.ggplot(p3$plot$state))
  
  expect_error({
    ggplot2::ggsave(
      filename = file.path(tmp_dir, "test_rain_complex.png"),
      plot = p3$plot$state,
      width = 8, height = 6
    )
  }, NA)
  
})
