
test_that("jjbetweenstats produces valid ggplot objects", {
  skip_if_not_installed("ggstatsplot")
  
  # Create a temporary directory for saving plots
  tmp_dir <- tempdir()
  
  # Test 1: Basic plot
  p1 <- jjbetweenstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    typestatistics = "parametric"
  )
  
  # Force render to populate state
  capture.output(print(p1$plot))
  
  expect_true(ggplot2::is.ggplot(p1$plot$state))
  
  # Save to verify it renders
  expect_error({
    ggplot2::ggsave(
      filename = file.path(tmp_dir, "test_jj_basic.png"),
      plot = p1$plot$state,
      width = 8, height = 6
    )
  }, NA)
  
  # Test 2: Non-parametric with pairwise comparisons
  p2 <- jjbetweenstats(
    data = mtcars,
    dep = "mpg",
    group = "cyl",
    typestatistics = "nonparametric",
    pairwisecomparisons = TRUE
  )
  
  # Force render
  capture.output(print(p2$plot))
  
  expect_true(ggplot2::is.ggplot(p2$plot$state))
  
  expect_error({
    ggplot2::ggsave(
      filename = file.path(tmp_dir, "test_jj_nonparam.png"),
      plot = p2$plot$state,
      width = 8, height = 6
    )
  }, NA)
  
  # Test 3: Robust statistics with centrality
  p3 <- jjbetweenstats(
    data = iris,
    dep = "Petal.Width",
    group = "Species",
    typestatistics = "robust",
    centralityplotting = TRUE,
    centralitytype = "robust"
  )
  
  # Force render
  capture.output(print(p3$plot))
  
  expect_true(ggplot2::is.ggplot(p3$plot$state))
  
  expect_error({
    ggplot2::ggsave(
      filename = file.path(tmp_dir, "test_jj_robust.png"),
      plot = p3$plot$state,
      width = 8, height = 6
    )
  }, NA)
  
})
