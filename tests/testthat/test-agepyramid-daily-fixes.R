# VAL-agepyramid-01: no stale results after either required variable is cleared.
# Backend transition test; native jamovi clearWith/save-reopen is separate evidence.
testthat::test_that("clearing a required variable clears all computed age-pyramid results", {
  d <- data.frame(age = c(0, 5, 10, 15), gender = factor(c("F", "M", "F", "M")))
  for (variable in c("age", "gender")) {
    options <- ClinicoPath:::agepyramidOptions$new(
      age = "age", gender = "gender", female = "F", male = "M", enableGGCharts = TRUE)
    a <- ClinicoPath:::agepyramidClass$new(options = options, data = d)
    a$run()
    before <- a$results$pyramidTable$asDF
    testthat::expect_gt(nrow(before), 0)
    selected <- options$option(variable)
    selected$value <- NULL
    a$.__enclos_env__$private$.run()
    testthat::expect_equal(a$results$pyramidTable$rowCount, 0L, info = variable)
    testthat::expect_null(a$results$pyramidTable$notes$pct, info = variable)
    testthat::expect_identical(a$results$dataInfo$content, "<div></div>", info = variable)
    testthat::expect_null(a$results$plot$state, info = variable)
    testthat::expect_null(a$results$plotGGCharts$state, info = variable)
    testthat::expect_true(a$results$welcome$visible, info = variable)
    fresh_args <- list(data = d, age = "age", gender = "gender", female = "F", male = "M")
    fresh_args[variable] <- list(NULL)
    fresh <- do.call(ClinicoPath::agepyramid, fresh_args)
    testthat::expect_equal(a$results$pyramidTable$asDF, fresh$pyramidTable$asDF)
    selected$value <- variable
    a$.__enclos_env__$private$.run()
    testthat::expect_equal(a$results$pyramidTable$asDF, before)
    testthat::expect_false(a$results$welcome$visible)
    testthat::expect_false(is.null(a$results$plot$state))
    testthat::expect_false(is.null(a$results$plotGGCharts$state))
  }
})
