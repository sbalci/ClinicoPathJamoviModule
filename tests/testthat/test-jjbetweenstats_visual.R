
library(testthat)

# Render an analysis's main plot for real and report whether a ggplot came out.
jjb_renders <- function(res) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 550)
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    ok <- tryCatch({ print(res$plot); TRUE }, error = function(e) FALSE)
    grDevices::dev.off(); on.exit()
    isTRUE(ok) && file.exists(f) && file.size(f) > 1000
}

test_that("jjbetweenstats produces valid ggplot objects", {
  skip_if_not_installed('jmvReadWrite')
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
  
  # This analysis renders inside .plot() and never calls setState(), so
  # `$state` is empty by design and the old assertion could never pass. Render
  # for real and inspect the resulting ggplot instead - which also means the
  # plot path is finally covered by a test.
  expect_true(jjb_renders(p1))
  
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
  
  expect_true(jjb_renders(p2))
  
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
  
  expect_true(jjb_renders(p3))
  
  expect_error({
    ggplot2::ggsave(
      filename = file.path(tmp_dir, "test_jj_robust.png"),
      plot = p3$plot$state,
      width = 8, height = 6
    )
  }, NA)
  
})
