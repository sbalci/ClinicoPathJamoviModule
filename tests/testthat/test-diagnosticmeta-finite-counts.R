dm_finite_data <- function() {
  data.frame(study = paste0("S", 1:6), tp = c(40, 35, 30, 45, 50, 22),
             fn = c(10, 15, 20, 5, 10, 8), fp = c(5, 10, 7, 12, 6, 9),
             tn = c(45, 40, 43, 38, 44, 41))
}

test_that("infinite diagnostic counts give actionable native analysis errors", {
  for (column in c("tp", "fn", "fp", "tn")) {
    for (value in c(Inf, -Inf)) {
      d <- dm_finite_data()
      d[[column]][2] <- value
      analysis <- diagnosticmetaClass$new(
        data = d, options = diagnosticmetaOptions$new(
          study = "study", true_positives = "tp", false_negatives = "fn",
          false_positives = "fp", true_negatives = "tn"
        )
      )
      expect_error(analysis$run(), "Infinite diagnostic counts", fixed = TRUE)
      expect_error(analysis$run(), column, fixed = TRUE)
      analysis$run(noThrow = TRUE)
      expect_true(analysis$errored)
      expect_identical(analysis$status, "error")
    }
  }
})

test_that("NaN diagnostic counts follow the disclosed missing-study path", {
  d <- dm_finite_data()
  d$tp[1] <- NaN
  result <- diagnosticmeta(d, study = "study", true_positives = "tp",
                           false_negatives = "fn", false_positives = "fp",
                           true_negatives = "tn")
  expect_match(result$notices$content, "1 of 6 studies were excluded", fixed = TRUE)
  expect_true(all(is.finite(result$bivariateresults$asDF$estimate)))
})
