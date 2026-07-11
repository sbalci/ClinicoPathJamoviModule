
test_that("ctdnadynamics classifies clearance and links MRD to survival", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("survival")

  set.seed(2026); n <- 120
  baseline <- round(runif(n, 0.5, 25), 2)
  cleared <- rbinom(n, 1, 0.55)
  followup <- ifelse(cleared == 1, round(pmax(0, rnorm(n, 0, 0.02)), 3),
                     round(baseline * runif(n, 0.3, 1.3), 3))
  mrd_pos <- as.integer(followup > 0.05)
  os <- round(rexp(n, 0.02 * exp(1.1 * mrd_pos)), 1)
  ev <- rbinom(n, 1, 0.7)
  data <- data.frame(baseline_vaf = baseline, followup_vaf = followup,
                     days = round(runif(n, 21, 42)),
                     os = os, event = ev)

  expect_no_error({
    model <- ctdnadynamics(
      data = data, baselineVaf = "baseline_vaf", followupVaf = "followup_vaf",
      detectionThreshold = 0.05, timeBetween = "days",
      survivalTime = "os", survivalStatus = "event", eventLevel = "1",
      showClassification = TRUE, showDynamics = TRUE, showSurvival = TRUE,
      showPlot = TRUE, showSummary = TRUE)
  })
  expect_true(inherits(model, "jmvcoreClass"))

  cls <- model$results$classificationTable$asDF
  expect_true(nrow(cls) == 3)                       # cleared / persistent / total
  # cleared fraction roughly half
  cleared_n <- cls$n[cls$category == "Cleared / MRD-negative"]
  expect_true(cleared_n > n * 0.3 && cleared_n < n * 0.75)

  sv <- model$results$survivalTable$asDF
  expect_true(any(grepl("Hazard ratio", sv$statistic)))
})

test_that("ctdnadynamics handles all-cleared edge case", {
  skip_if_not_installed("jmvcore")
  data <- data.frame(
    baseline_vaf = runif(30, 1, 10),
    followup_vaf = rep(0, 30))
  expect_no_error({
    model <- ctdnadynamics(
      data = data, baselineVaf = "baseline_vaf", followupVaf = "followup_vaf",
      detectionThreshold = 0.05, showSurvival = FALSE)
  })
  cls <- model$results$classificationTable$asDF
  expect_equal(cls$n[cls$category == "Cleared / MRD-negative"], 30)
})
