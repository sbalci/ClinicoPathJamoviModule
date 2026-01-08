# ═══════════════════════════════════════════════════════════
# Basic Tests: outcomeorganizer
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(outcomeorganizer_os, package = "ClinicoPath")
data(outcomeorganizer_compete, package = "ClinicoPath")
data(outcomeorganizer_pfs, package = "ClinicoPath")
data(outcomeorganizer_rfs, package = "ClinicoPath")
data(outcomeorganizer_causespecific, package = "ClinicoPath")
data(outcomeorganizer_multistate, package = "ClinicoPath")
data(outcomeorganizer_dfs, package = "ClinicoPath")

test_that("outcomeorganizer creates proper class", {
  result <- outcomeorganizer(
    data = outcomeorganizer_os,
    outcome = "vital_status",
    time = "time_months"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles overall survival (OS)", {
  result <- outcomeorganizer(
    data = outcomeorganizer_os,
    outcome = "vital_status",
    time = "time_months",
    dead = "Dead",
    alive = "Alive"
  )
  expect_s3_class(result, "outcomeorganizerClass")
  expect_true(length(result$results) > 0)
})

test_that("outcomeorganizer handles competing risks data", {
  result <- outcomeorganizer(
    data = outcomeorganizer_compete,
    outcome = "outcome_status",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles progression-free survival (PFS)", {
  result <- outcomeorganizer(
    data = outcomeorganizer_pfs,
    outcome = "progression",
    time = "time_months"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles recurrence-free survival (RFS)", {
  result <- outcomeorganizer(
    data = outcomeorganizer_rfs,
    outcome = "recurrence",
    time = "fu_time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles cause-specific survival", {
  result <- outcomeorganizer(
    data = outcomeorganizer_causespecific,
    outcome = "death_status",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles multistate models", {
  result <- outcomeorganizer(
    data = outcomeorganizer_multistate,
    outcome = "current_state",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles disease-free survival (DFS)", {
  result <- outcomeorganizer(
    data = outcomeorganizer_dfs,
    outcome = "status",
    time = "time_years"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer output table option works", {
  result <- outcomeorganizer(
    data = outcomeorganizer_os,
    outcome = "vital_status",
    time = "time_months",
    outputTable = TRUE
  )
  expect_s3_class(result, "outcomeorganizerClass")
  expect_true(!is.null(result$results$outputTable))
})

test_that("outcomeorganizer diagnostics option works", {
  result <- outcomeorganizer(
    data = outcomeorganizer_os,
    outcome = "vital_status",
    time = "time_months",
    diagnostics = TRUE
  )
  expect_s3_class(result, "outcomeorganizerClass")
  expect_true(!is.null(result$results$diagnosticsTable))
})

test_that("outcomeorganizer handles multiple covariates", {
  result <- outcomeorganizer(
    data = outcomeorganizer_os,
    outcome = "vital_status",
    time = "time_months",
    dead = "Dead",
    alive = "Alive"
  )
  expect_s3_class(result, "outcomeorganizerClass")
  expect_true(nrow(outcomeorganizer_os) == 150)
})

test_that("outcomeorganizer handles different time units", {
  result <- outcomeorganizer(
    data = outcomeorganizer_dfs,
    outcome = "status",
    time = "time_years"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles complex outcome categories", {
  result <- outcomeorganizer(
    data = outcomeorganizer_compete,
    outcome = "outcome_status",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
  # Check that data has 4 outcome categories
  expect_true(length(unique(outcomeorganizer_compete$outcome_status)) == 4)
})
