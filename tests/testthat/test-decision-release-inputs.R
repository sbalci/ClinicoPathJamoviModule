# Self-contained installed-package regression; no umbrella data or load_all().
test_that("decision distinguishes pending, fatal and valid input", {
  ns <- environment(decision)
  d <- data.frame(gold = factor(rep(c("N", "P"), each = 40)),
    a = factor(rep(c("N", "P", "P", "P"), 20)),
    b = factor(rep(c("N", "N", "P", "P"), 20)),
    score = seq_len(80) / 100 + rep(c(.2, -.2), 40))
  args <- list(gold = "gold", goldPositive = "P", goldNegative = "N", newtest = "a", testPositive = "P", testNegative = "N")
  make <- function(changes = list()) {
    opts <- do.call(ns$decisionOptions$new, utils::modifyList(args, changes, keep.null = TRUE))
    a <- ns$decisionClass$new(data = d, options = opts)
    a$init()
    a
  }
  expect_error(do.call(ns$decision, c(list(data = d),
    utils::modifyList(args, list(goldPositive = "NotObserved")))), "NotObserved", fixed = TRUE)
  invalid <- make(list(goldPositive = "NotObserved"))
  # noThrow uses the real jmvcore handler: an ERROR banner alone cannot pass.
  suppressWarnings(invalid$run(noThrow = TRUE))
  expect_identical(invalid$status, "error")
  expect_true(invalid$errored)
  expect_false(invalid$complete)
  unfinished <- make(list(goldPositive = NULL))
  suppressWarnings(unfinished$run(noThrow = TRUE))
  expect_false(unfinished$errored)
  expect_identical(unfinished$status, "complete")
  good <- make()
  suppressWarnings(good$run(noThrow = TRUE))
  expect_false(good$errored)
  expect_true(good$complete)
})
