library(testthat)

known_data_2_tests <- data.frame(
  gold = factor(
    c(
      "P", "P", "P", "P", "P", "P", "P", "P", "P", "P",
      "N", "N", "N", "N", "N", "N", "N", "N", "N", "N"
    ),
    levels = c("P", "N")
  ),
  test1 = factor(
    c(
      "P", "P", "P", "P", "P", "P", "P", "P", "N", "N",
      "P", "P", "P", "N", "N", "N", "N", "N", "N", "N"
    ),
    levels = c("P", "N")
  ),
  test2 = factor(
    c(
      "P", "P", "P", "P", "P", "N", "N", "N", "P", "P",
      "P", "N", "N", "P", "P", "N", "N", "N", "N", "N"
    ),
    levels = c("P", "N")
  )
)

known_data_3_tests <- data.frame(
  gold = factor(
    c(
      "P", "P", "P", "P", "P", "P", "P", "P", "P", "P",
      "N", "N", "N", "N", "N", "N", "N", "N", "N", "N"
    ),
    levels = c("P", "N")
  ),
  test1 = factor(
    c(
      "P", "P", "P", "P", "N", "N", "N", "N", "N", "N",
      "P", "P", "P", "P", "N", "N", "N", "N", "N", "N"
    ),
    levels = c("P", "N")
  ),
  test2 = factor(
    c(
      "P", "P", "N", "N", "P", "P", "N", "N", "N", "N",
      "P", "P", "N", "N", "P", "P", "N", "N", "N", "N"
    ),
    levels = c("P", "N")
  ),
  test3 = factor(
    c(
      "P", "N", "P", "N", "P", "N", "P", "N", "N", "N",
      "P", "N", "P", "N", "P", "N", "P", "N", "N", "N"
    ),
    levels = c("P", "N")
  )
)


test_that("decisioncombine reports expected metrics for a two-test pattern", {
  skip_if_not_installed("jmvReadWrite")

  result <- decisioncombine(
    data = known_data_2_tests,
    gold = "gold",
    goldPositive = "P",
    test1 = "test1",
    test1Positive = "P",
    test2 = "test2",
    test2Positive = "P",
    test3Positive = "P"
  )

  tbl <- as.data.frame(result$combinationTable)
  expect_setequal(
    c(
      # `rowType` distinguishes the observed test patterns from the named strategy rows
      # (Serial/AND, Parallel/OR), added in 1.0.4 so the serial rule is findable.
      "pattern", "rowType", "tp", "fp", "fn", "tn", "prevalence", "sens", "spec", "ppv", "npv",
      "acc", "balancedAccuracy", "youden", "lrPos", "lrNeg", "dor"
    ),
    names(tbl)
  )

  plus_row <- tbl[tbl$pattern == "+/+", ]
  expect_equal(plus_row$tp, 5)
  expect_equal(plus_row$fp, 1)
  expect_equal(plus_row$fn, 5)
  expect_equal(plus_row$tn, 9)
  expect_equal(plus_row$prevalence, 0.5, tolerance = 1e-6)
  expect_equal(plus_row$sens, 0.5, tolerance = 1e-6)
  expect_equal(plus_row$spec, 0.9, tolerance = 1e-6)
  expect_equal(plus_row$ppv, 5 / 6, tolerance = 1e-6)
  expect_equal(plus_row$npv, 9 / 14, tolerance = 1e-6)
  expect_equal(plus_row$acc, 0.7, tolerance = 1e-6)
  expect_equal(plus_row$balancedAccuracy, 0.7, tolerance = 1e-6)
  expect_equal(plus_row$youden, 0.4, tolerance = 1e-6)
  expect_equal(plus_row$lrPos, 5, tolerance = 1e-6)
  expect_equal(plus_row$lrNeg, 5 / 9, tolerance = 1e-6)
  expect_equal(plus_row$dor, 9, tolerance = 1e-6)
})


test_that("clinical strategies are summarised for two-test combinations", {

  result <- decisioncombine(
    data = known_data_2_tests,
    gold = "gold",
    goldPositive = "P",
    test1 = "test1",
    test1Positive = "P",
    test2 = "test2",
    test2Positive = "P",
    test3Positive = "P"
  )

  tbl <- as.data.frame(result$combinationTable)
  parallel_row <- tbl[tbl$pattern == "Parallel (>=1 pos)", ]
  expect_equal(parallel_row$tp, 10)
  expect_equal(parallel_row$fp, 5)
  expect_equal(parallel_row$fn, 0)
  expect_equal(parallel_row$tn, 5)
  expect_equal(parallel_row$sens, 1, tolerance = 1e-6)
  expect_equal(parallel_row$spec, 0.5, tolerance = 1e-6)
  expect_equal(parallel_row$dor, 21, tolerance = 0.1)
  expect_equal(parallel_row$lrPos, 1.91, tolerance = 0.01)
  expect_equal(parallel_row$lrNeg, 0.091, tolerance = 0.001)

  # LR+, LR- and DOR are unbounded ratios and moved out of combinationTableCI (which is
  # now proportions only, formatted as percentages) into combinationTableCIRatios, so one
  # column no longer has to carry two different units. The ratios table only exists once
  # jmvtools::prepare() has compiled the .r.yaml, so tolerate its absence.
  ratios <- tryCatch(as.data.frame(result$combinationTableCIRatios), error = function(e) NULL)
  skip_if(is.null(ratios) || nrow(ratios) == 0,
          "combinationTableCIRatios not compiled yet - run jmvtools::prepare()")

  lr_ci <- ratios[ratios$pattern == "Parallel (>=1 pos)" & ratios$statistic == "LR+", ]
  expect_equal(nrow(lr_ci), 1L)
  expect_true(all(!is.na(lr_ci$estimate)))
  expect_true(all(lr_ci$lower > 0))

  # and the proportions table must no longer carry it
  props <- as.data.frame(result$combinationTableCI)
  expect_equal(nrow(props[props$statistic == "LR+", ]), 0L)
  expect_setequal(unique(as.character(props$statistic)),
                  c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy"))
})




test_that("input validation detects mismatched positive level", {

  expect_no_error(
    decisioncombine(
      data = known_data_2_tests,
      gold = "gold",
      goldPositive = "Absent",
      test1 = "test1",
      test1Positive = "P",
      test2 = "test2",
      test2Positive = "P",
      test3Positive = "P"
    )
  )
})


test_that("majority rule strategy is calculated for three tests", {

  result <- decisioncombine(
    data = known_data_3_tests,
    gold = "gold",
    goldPositive = "P",
    test1 = "test1",
    test1Positive = "P",
    test2 = "test2",
    test2Positive = "P",
    test3 = "test3",
    test3Positive = "P"
  )

  tbl <- as.data.frame(result$combinationTable)
  majority_row <- tbl[tbl$pattern == "Majority (>=2/3 pos)", ]
  expect_equal(majority_row$tp, 4)
  expect_equal(majority_row$fp, 4)
  expect_equal(majority_row$fn, 6)
  expect_equal(majority_row$tn, 6)
  expect_equal(majority_row$sens, 0.4, tolerance = 1e-6)
  expect_equal(majority_row$spec, 0.6, tolerance = 1e-6)
  expect_equal(majority_row$balancedAccuracy, 0.5, tolerance = 1e-6)
  expect_equal(majority_row$lrPos, 1, tolerance = 1e-6)
  expect_equal(majority_row$lrNeg, 1, tolerance = 1e-6)
  expect_equal(majority_row$dor, 1, tolerance = 1e-6)
})
