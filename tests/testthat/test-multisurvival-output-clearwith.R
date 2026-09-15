# Output columns are written only while empty (isNotFilled), so every option that
# changes their values or their rows must be in clearWith, or the column exported
# to the user's dataset goes stale (/check-function-full multisurvival, 2026-09-15).

test_that("multisurvival Output columns clear on every option that changes them", {
  r <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "multisurvival.r.yaml"))
  out <- Filter(function(x) identical(x$type, "Output"), r$items)
  names(out) <- vapply(out, `[[`, "", "name")

  rows <- c("outcome", "outcomeLevel", "multievent", "analysistype", "dod", "dooc", "awd", "awod",
            "explanatory", "contexpl", "adjexplanatory", "use_stratify", "stratvar",
            "tint", "dxdate", "fudate", "timetypedata", "uselandmark", "landmark")

  expect_setequal(names(out), c("calculatedtime", "outcomeredefined", "addRiskScore", "addRiskGroup"))
  missing_calc <- setdiff(c(rows, "timetypeoutput"), out$calculatedtime$clearWith)
  expect_length(missing_calc, 0)
  for (nm in c("outcomeredefined", "addRiskScore", "addRiskGroup")) {
    expect_length(setdiff(rows, out[[nm]]$clearWith), 0)
  }
  # A competing-risk outcome is a labelled factor: a continuous Output drops the labels.
  expect_identical(out$outcomeredefined$measureType, "nominal")
  expect_identical(out$addRiskGroup$measureType, "nominal")
})
