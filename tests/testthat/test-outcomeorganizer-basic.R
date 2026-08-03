# ═══════════════════════════════════════════════════════════
# Basic Tests: outcomeorganizer
# ═══════════════════════════════════════════════════════════
library(testthat)

# run_outcomeorganizer() (helper-outcomeorganizer.R) supplies the six Level
# options that the generated wrapper requires. Calling outcomeorganizer()
# directly without them throws "argument \"outcomeLevel\" is missing, with no
# default" before the analysis ever runs.

data(outcomeorganizer_os, package = "ClinicoPath")
data(outcomeorganizer_compete, package = "ClinicoPath")
data(outcomeorganizer_pfs, package = "ClinicoPath")
data(outcomeorganizer_rfs, package = "ClinicoPath")
data(outcomeorganizer_causespecific, package = "ClinicoPath")
data(outcomeorganizer_multistate, package = "ClinicoPath")
data(outcomeorganizer_dfs, package = "ClinicoPath")

test_that("outcomeorganizer creates proper class", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_os,
    outcome = "vital_status",
    outcomeLevel = "Dead",
    analysistype = "os"
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles overall survival (OS)", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_os,
    outcome = "vital_status",
    outcomeLevel = "Dead",
    analysistype = "os"
  )
  expect_s3_class(result, "outcomeorganizerResults")
  expect_true(!is.null(result$summary))
})

test_that("outcomeorganizer handles competing risks data", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_compete,
    outcome = "outcome_status",
    multievent = TRUE,
    dod = "Dead_Disease", dooc = "Dead_Other",
    awd = "Alive_Disease", awod = "Alive_NED",
    analysistype = "compete"
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles progression-free survival (PFS)", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_pfs,
    outcome = "vital_status",
    outcomeLevel = "Dead",
    recurrence = "progression",
    recurrenceLevel = "Yes",
    analysistype = "pfs"
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles recurrence-free survival (RFS)", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_rfs,
    outcome = "vital",
    outcomeLevel = "Dead",
    recurrence = "recurrence",
    recurrenceLevel = "Yes",
    analysistype = "rfs"
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles cause-specific survival", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_causespecific,
    outcome = "death_status",
    outcomeLevel = "Dead_Cancer",
    analysistype = "cause"
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles multistate models", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_multistate,
    outcome = "current_state",
    multievent = TRUE,
    awod = "Disease_Free", awd = "Local_Recurrence",
    dod = "Dead", dooc = "Metastatic",
    analysistype = "multistate"
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles disease-free survival (DFS)", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_dfs,
    outcome = "status",
    outcomeLevel = "Dead",
    analysistype = "dfs"
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer output table and diagnostics work", {
  result <- run_outcomeorganizer(
    data = outcomeorganizer_os,
    outcome = "vital_status",
    outcomeLevel = "Dead",
    outputTable = TRUE,
    diagnostics = TRUE,
    showNaturalSummary = TRUE,
    showGlossary = TRUE
  )
  expect_s3_class(result, "outcomeorganizerResults")
})
