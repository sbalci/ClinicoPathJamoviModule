# Exploratory percentage thresholds, not a lesion-level RECIST assessment.
# Exercise the loaded module, never a local copy of its categorization logic.
wf_boundary_categories <- function(response) {
  analysis <- waterfallClass$new(
    data = data.frame(id = "P1", response = 0),
    options = waterfallOptions$new(patientID = "id", responseVar = "response")
  )
  analysis$.__enclos_env__$private$.categorizeRECIST(response)
}

wf_boundary_run <- function(response) {
  suppressWarnings(waterfall(
    data = data.frame(id = paste0("P", seq_along(response)), response = response),
    patientID = "id", responseVar = "response", showConfidenceIntervals = TRUE
  ))
}

wf_boundary_counts <- function(result) {
  table <- result$summaryTable$asDF
  stats::setNames(table$n, table$category)[c("CR", "PR", "SD", "PD")]
}

test_that("the real categorizer includes exactly +20 percent in PD", {
  response <- c(-100, -99, -60, -30, -29, -10, 0, 10, 19.99, 20, 21, 50, 100, NA)
  expected <- c("CR", "PR", "PR", "PR", "SD", "SD", "SD", "SD", "SD",
                "PD", "PD", "PD", "PD", "Unknown")
  expect_identical(as.character(wf_boundary_categories(response)), expected)
})

test_that("backend counts and rates use the inclusive PD boundary", {
  result <- wf_boundary_run(c(-100, -60, -30, -10, 0, 10, 20, 21, 50, 100))
  expect_equal(unname(wf_boundary_counts(result)), c(1, 2, 3, 4))
  rates <- result$enhancedClinicalMetrics$asDF
  expect_identical(rates$value[1:2], c("30.0%", "60.0%"))
})

test_that("out-of-range shrinkage is capped and disclosed by the analysis", {
  result <- wf_boundary_run(c(-150, -120, -100, -105, -110))
  expect_equal(unname(wf_boundary_counts(result)), c(5, 0, 0, 0))
  expect_match(result$notices$content, "IMPOSSIBLE SHRINKAGE CAPPED", fixed = TRUE)
  result <- wf_boundary_run(c(20, 30, 50, 100, 200))
  expect_equal(unname(wf_boundary_counts(result)), c(0, 0, 0, 5))
  expect_identical(as.character(wf_boundary_categories(c(-100, -30, 20))),
                   c("CR", "PR", "PD"))
})

test_that("displayed exact binomial intervals agree with binom.test", {
  for (responders in c(0, 12, 40)) {
    result <- wf_boundary_run(c(rep(-50, responders), rep(40, 40 - responders)))
    rates <- result$enhancedClinicalMetrics$asDF
    expected <- round(stats::binom.test(responders, 40)$conf.int * 100, 1)
    expect_equal(as.numeric(c(rates$ci_lower[1], rates$ci_upper[1])), as.numeric(expected))
  }
})

test_that("the backend assigns a -50 percent response to PR", {
  result <- wf_boundary_run(-50)
  expect_equal(unname(wf_boundary_counts(result)), c(0, 1, 0, 0))
})

test_that("the complete reference workflow reports the corrected counts and rates", {
  response <- c(-100, -110, -80, -60, -45, -35, -30,
                -29, -20, -10, 0, 5, 10, 15, 20, 21, 30, 50, 80, 120)
  result <- wf_boundary_run(response)
  expect_equal(unname(wf_boundary_counts(result)), c(2, 5, 7, 6))
  expect_identical(result$enhancedClinicalMetrics$asDF$value[1:2],
                   c("35.0%", "70.0%"))
})
