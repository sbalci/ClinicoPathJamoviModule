library(testthat)

data(survival_test, package = "ClinicoPath")

survival_audit_args <- function(data = survival_test, ...) {
  defaults <- list(
    data = data,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "1",
    dod = "",
    dooc = "",
    awd = "",
    awod = "",
    explanatory = "treatment"
  )
  supplied <- list(...)
  defaults[names(supplied)] <- supplied
  defaults
}

strip_survival_html <- function(x) {
  trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", x)))
}

survival_spec_dir <- function() {
  candidates <- c(testthat::test_path("..", "..", "jamovi"), "jamovi")
  found <- Filter(
    function(path) file.exists(file.path(path, "survival.a.yaml")),
    candidates
  )
  if (length(found) == 0L)
    return(NULL)
  found[[1]]
}

test_that("pairwise test type changes the pairwise test", {
  logrank <- do.call(
    survival,
    survival_audit_args(explanatory = "stage", pw = TRUE,
                        survivalTestType = "logrank")
  )
  peto <- do.call(
    survival,
    survival_audit_args(explanatory = "stage", pw = TRUE,
                        survivalTestType = "fh_rho1")
  )

  expect_gt(logrank$pairwiseTable$rowCount, 0L)
  expect_equal(logrank$pairwiseTable$rowCount, peto$pairwiseTable$rowCount)
  expect_false(isTRUE(all.equal(
    logrank$pairwiseTable$asDF$value,
    peto$pairwiseTable$asDF$value
  )))
})

test_that("low event notices use the intended severity and explain suppression", {
  make_events <- function(n_events) {
    data <- survival_test
    data$outcome <- 0
    data$outcome[seq_len(n_events)] <- 1
    data
  }

  very_low <- do.call(
    survival,
    survival_audit_args(
      data = make_events(5),
      calibration_curves = TRUE,
      bootstrapValidation = TRUE
    )
  )
  expect_true(very_low$strongWarnings$visible)
  expect_match(strip_survival_html(very_low$strongWarnings$content), "Only 5 event")
  expect_equal(very_low$coxTable$rowCount, 0L)
  expect_match(
    very_low$calibrationTable$notes$lowevents$note,
    "fewer than 10 events",
    fixed = TRUE
  )
  expect_match(
    very_low$bootstrapValidationTable$notes$lowevents$note,
    "fewer than 10 events",
    fixed = TRUE
  )

  limited <- do.call(survival, survival_audit_args(data = make_events(15)))
  expect_true(limited$strongWarnings$visible)
  expect_match(strip_survival_html(limited$strongWarnings$content), "Only 15 events")

  moderate <- do.call(survival, survival_audit_args(data = make_events(30)))
  expect_true(moderate$warnings$visible)
  expect_match(strip_survival_html(moderate$warnings$content), "Limited event count: 30")
})

test_that("survival result invalidation covers repaired dependencies", {
  skip_if_not_installed("yaml")
  spec_dir <- survival_spec_dir()
  skip_if(is.null(spec_dir), "jamovi spec directory not available")

  options <- yaml::read_yaml(file.path(spec_dir, "survival.a.yaml"))
  results <- yaml::read_yaml(file.path(spec_dir, "survival.r.yaml"))
  declared <- vapply(options$options, function(option) option$name, character(1))
  result_names <- vapply(results$items, function(item) item$name, character(1))
  by_name <- stats::setNames(results$items, result_names)

  expect_false("errors" %in% result_names)
  expect_equal(by_name$weightedLogRankTable$rows, 3)
  expect_equal(by_name$bootstrapValidationTable$rows, 3)
  expect_equal(by_name$rcsTestTable$rows, 2)

  expect_true(all(c("padjustmethod", "survivalTestType") %in%
                    by_name$pairwiseTable$clearWith))
  expect_true(all(c("padjustmethod", "survivalTestType") %in%
                    by_name$pairwiseSummary$clearWith))
  expect_true("seed" %in% by_name$bootstrapValidationTable$clearWith)
  expect_true("seed" %in% by_name$bootstrapValidationExplanation$clearWith)
  expect_true("rcs_variable" %in% by_name$calibrationTable$clearWith)
  expect_true("rcs_variable" %in% by_name$calibrationGroupTable$clearWith)
  expect_true("rcs_variable" %in% by_name$calibrationPlot$clearWith)
  expect_true(all(c("spline_knots", "spline_scale") %in%
                    by_name$parametricModelSummary$clearWith))
  expect_true(all(c("spline_knots", "spline_scale") %in%
                    by_name$parametricSurvivalPlot$clearWith))

  for (item in results$items) {
    clear_with <- item$clearWith
    if (is.null(clear_with))
      next
    expect_false(anyDuplicated(clear_with) > 0L, info = item$name)
    expect_true(all(clear_with %in% declared), info = item$name)
  }
})

test_that("pairwise selector is presented with pairwise controls", {
  spec_dir <- survival_spec_dir()
  skip_if(is.null(spec_dir), "jamovi spec directory not available")
  ui <- paste(readLines(file.path(spec_dir, "survival.u.yaml"), warn = FALSE),
              collapse = "\n")

  expect_match(
    ui,
    "label: Pairwise Comparisons[\\s\\S]*name: survivalTestType[\\s\\S]*enable: \\(pw\\)",
    perl = TRUE
  )
  weighted_block <- sub(
    "^[\\s\\S]*label: Weighted Log-Rank Tests",
    "label: Weighted Log-Rank Tests",
    ui,
    perl = TRUE
  )
  weighted_block <- sub("\\n      - type: Label[\\s\\S]*$", "", weighted_block,
                        perl = TRUE)
  expect_false(grepl("name: survivalTestType", weighted_block, fixed = TRUE))
})

