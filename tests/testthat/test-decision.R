context("Medical Decision Analysis")

test_that("sensitivity calculation is correct", {
  expect_equal(calculate_sensitivity(tp = 90, fn = 10), 0.9)
  expect_equal(calculate_sensitivity(tp = 0, fn = 10), 0)
  expect_equal(calculate_sensitivity(tp = 50, fn = 50), 0.5)
  expect_true(is.na(calculate_sensitivity(tp = NA, fn = 1)))
})

test_that("specificity calculation is correct", {
  expect_equal(calculate_specificity(tn = 80, fp = 20), 0.8)
  expect_equal(calculate_specificity(tn = 0, fp = 20), 0)
  expect_equal(calculate_specificity(tn = 50, fp = 50), 0.5)
  expect_true(is.na(calculate_specificity(tn = 1, fp = NA)))
})

test_that("AUC calculation is correct", {
  expect_equal(calculate_auc(sens = 0.9, spec = 0.8), 0.53)
  expect_true(calculate_auc(sens = 1, spec = 1) <= 1)
  expect_true(calculate_auc(sens = 0, spec = 0) >= 0)
  expect_true(is.na(calculate_auc(NA, 0.5)))
})
