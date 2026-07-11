
test_that("synopticcompleteness computes per-report and per-element completeness", {
  skip_if_not_installed("jmvcore")

  # 4 reports x 4 elements; known presence pattern (NA = absent)
  data <- data.frame(
    e1 = c("Reported", "Reported", "Reported", NA),
    e2 = c("Reported", "Reported", NA, NA),
    e3 = c("Reported", NA, NA, NA),
    e4 = c("Reported", "Reported", "Reported", "Reported"))
  # per report present counts: 4,3,2,1 -> completeness 100/75/50/25%
  expect_no_error({
    model <- synopticcompleteness(
      data = data, items = c("e1", "e2", "e3", "e4"),
      presenceRule = "nonmissing", completeThreshold = 100,
      showOverall = TRUE, showPerItem = TRUE, showByGroup = FALSE,
      showTrend = FALSE, showPlot = TRUE)
  })
  expect_true(inherits(model, "jmvcoreClass"))

  ov <- model$results$overallTable$asDF
  # mean completeness = mean(100,75,50,25) = 62.5%
  expect_true(grepl("62.5", ov$value[ov$metric == "Mean completeness"]))
  # exactly 1 report fully complete
  expect_true(grepl("^1 ", ov$value[grepl("complete", ov$metric)]))

  pi <- model$results$perItemTable$asDF
  # e4 present in all 4 (100%), e3 in 1 (25%)
  expect_equal(pi$present[pi$element == "e4"], 4)
  expect_equal(pi$present[pi$element == "e3"], 1)
})

test_that("synopticcompleteness detects an improving trend over time", {
  skip_if_not_installed("jmvcore")

  set.seed(2026); nr <- 240
  month <- sample(1:24, nr, TRUE)
  base_p <- pmin(0.99, 0.70 + 0.010 * month)
  mk <- function() ifelse(rbinom(nr, 1, base_p) == 1, "Reported", NA_character_)
  data <- data.frame(month = month,
                     a = mk(), b = mk(), c = mk(), d = mk(), e = mk())

  model <- synopticcompleteness(
    data = data, items = c("a", "b", "c", "d", "e"), timeVar = "month",
    presenceRule = "nonmissing", showTrend = TRUE, showPlot = TRUE)
  tr <- model$results$trendTable$asDF
  expect_true(any(grepl("per unit time", tr$statistic)))
  # direction should read Improving
  expect_true(any(grepl("Improving", tr$value)))
})
