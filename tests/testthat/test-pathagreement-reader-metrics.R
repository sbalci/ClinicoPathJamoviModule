test_that("reader accuracy is disease-positive, not macro-averaged", {
  gold <- rep(c(TRUE, FALSE), each = 10)
  x <- c(rep(TRUE, 9), FALSE, rep(FALSE, 6), rep(TRUE, 4))
  a <- .paReaderAccuracy(x, gold)
  expect_equal(unname(a[c("tp", "fn", "tn", "fp")]), c(9, 1, 6, 4))
  expect_equal(unname(a[c("sensitivity", "specificity", "balanced_accuracy")]), c(.9, .6, .75))
  expect_equal(.paReaderAccuracy(!x, !gold)[["sensitivity"]], .6)
  expect_equal(.paReaderExactCI(9, 10, .90), as.numeric(binom.test(9, 10, conf.level = .90)$conf.int))
})

test_that("agreement counts match independent hand calculations and irr", {
  x <- c(rep(TRUE, 10), rep(FALSE, 10))
  y <- c(rep(TRUE, 8), rep(FALSE, 2), rep(TRUE, 3), rep(FALSE, 7))
  a <- .paReaderAgreement(x, y)
  expect_equal(a[["observed_agreement"]], .75)
  expect_equal(a[["kappa"]], .5)
  expect_equal(a[["positive_agreement"]], 16 / 21)
  expect_equal(a[["negative_agreement"]], 14 / 19)
  b <- .paReaderAgreement(y, x)
  expect_equal(a[["first_positive"]], b[["second_positive"]])
  expect_equal(a[["kappa"]], b[["kappa"]])
  expect_equal(.paReaderAgreement(x, !x)[["kappa"]], -1)
  expect_true(is.na(.paReaderAgreement(rep(TRUE, 4), rep(TRUE, 4))[["kappa"]]))
  expect_true(is.na(.paReaderAgreement(rep(FALSE, 4), rep(FALSE, 4))[["positive_agreement"]]))
  skip_if_not_installed("irr")
  expect_equal(a[["kappa"]], irr::kappa2(data.frame(x, y))$value)
})

test_that("original ordinal kappa uses ordered weights", {
  x <- ordered(c("L", "L", "M", "M", "H", "H"), c("L", "M", "H"))
  y <- ordered(c("L", "M", "M", "H", "H", "L"), levels(x))
  skip_if_not_installed("irr")
  for (w in c("unweighted", "equal", "squared")) {
    expect_equal(.paReaderOrdinal(x, y, w),
      irr::kappa2(data.frame(x = as.integer(x), y = as.integer(y)), weight = w)$value)
  }
})

test_that("composite domain and familiar-scale counterexamples are explicit", {
  expect_equal(.paReaderComposite(.5, 1), sqrt(.5))
  expect_equal(.paReaderComposite(.95, .5), .paReaderComposite(.5, .95))
  expect_equal(.paReaderComposite(.9, 0), 0)
  expect_true(is.na(.paReaderComposite(.8, -.2)))
  expect_true(is.na(.paReaderComposite(.8, NA_real_)))
})
