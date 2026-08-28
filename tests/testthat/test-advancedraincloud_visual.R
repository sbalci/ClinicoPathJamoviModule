# Rendering tests for advancedraincloud.
#
# These used to assert `result$plot$state` was a ggplot and then ggsave() it.
# That has not been the contract since the release review: `.run()` now calls
# image$setState(analysis_data), i.e. the state carries the DATA (the canonical
# jamovi pattern - it is what gets serialised into the .omv), and the ggplot is
# built inside `.plot()` on every render. ggsave()ing the state therefore fails
# with "no applicable method for 'grid.draw' applied to an object of class
# data.frame", which is the state doing exactly what it should.
#
# So render the way jamovi does: call the private `.plot()` with the results
# Image, on a null device. It returns TRUE, and any failure in ggrain, the
# palette, the annotations or the journal theme surfaces as a real error.

library(testthat)

# Build and render one plot; returns .plot()'s value (TRUE on success).
render_arc <- function(data, ...) {
  opts <- ClinicoPath:::advancedraincloudOptions$new(...)
  analysis <- ClinicoPath:::advancedraincloudClass$new(options = opts, data = data)
  analysis$run()

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  suppressWarnings(
    analysis$.__enclos_env__$private$.plot(
      analysis$results$plot,
      ggtheme = ggplot2::theme_bw(),
      theme = NULL
    )
  )
}

arc_state <- function(data, ...) {
  opts <- ClinicoPath:::advancedraincloudOptions$new(...)
  analysis <- ClinicoPath:::advancedraincloudClass$new(options = opts, data = data)
  analysis$run()
  analysis$results$plot$state
}

test_that("plot state carries the analysis data frame, not a ggplot", {
  skip_if_not_installed("ggrain")

  state <- arc_state(histopathology, y_var = "Age", x_var = "Group")

  expect_s3_class(state, "data.frame")
  expect_true(all(c("Age", "Group") %in% names(state)))
  expect_true(nrow(state) > 0)
  # complete cases only, and no non-finite values survive
  expect_true(all(is.finite(state$Age)))
})

test_that("a basic cross-sectional raincloud renders", {
  skip_if_not_installed("ggrain")

  expect_true(render_arc(histopathology, y_var = "Age", x_var = "Group"))
})

test_that("a longitudinal raincloud with connections renders", {
  skip_if_not_installed("ggrain")

  set.seed(123)
  n_subjects <- 20
  long_data <- data.frame(
    ID = rep(1:n_subjects, each = 2),
    Time = rep(c("Pre", "Post"), n_subjects),
    Score = rnorm(n_subjects * 2, mean = 50, sd = 10) + rep(rnorm(n_subjects, 0, 5), each = 2)
  )
  long_data$Score[long_data$Time == "Post"] <- long_data$Score[long_data$Time == "Post"] + 5

  expect_true(render_arc(
    long_data,
    y_var = "Score",
    x_var = "Time",
    id_var = "ID",
    show_longitudinal = TRUE,
    rain_side = "f1x1"
  ))
})

test_that("Likert mode with a covariate and the viridis palette renders", {
  skip_if_not_installed("ggrain")

  # viridisLite::viridis() has no `discrete` argument; passing one errored and
  # the palette silently fell back to ggplot defaults. Keep this arm covered.
  expect_true(render_arc(
    histopathology,
    y_var = "Age",
    x_var = "Group",
    cov_var = "OverallTime",
    likert_mode = TRUE,
    rain_side = "r",
    color_palette = "viridis"
  ))
})
