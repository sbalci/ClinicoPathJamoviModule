# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: oddsratio
# ═══════════════════════════════════════════════════════════
library(testthat)
data(oddsratio_small, package = "ClinicoPath")
data(oddsratio_large, package = "ClinicoPath")
data(oddsratio_perfect, package = "ClinicoPath")
data(oddsratio_zerocell, package = "ClinicoPath")

test_that("oddsratio handles small datasets", {
  result <- oddsratio(
    data = oddsratio_small,
    explanatory = "predictor",
    outcome = "outcome",
    outcomeLevel = NULL,
    outcomeNegativeLevel = NULL,
    predictorLevel = NULL,
    predictorNegativeLevel = NULL
  )
  expect_s3_class(result, "oddsratioResults")
})

test_that("oddsratio handles large datasets", {
  result <- oddsratio(
    data = oddsratio_large,
    explanatory = c("var1", "var2"),
    outcome = "outcome",
    outcomeLevel = NULL,
    outcomeNegativeLevel = NULL,
    predictorLevel = NULL,
    predictorNegativeLevel = NULL
  )
  expect_no_error(result)
})

test_that("oddsratio handles perfect separation", {
  result <- oddsratio(
    data = oddsratio_perfect,
    explanatory = "predictor",
    outcome = "outcome",
    outcomeLevel = NULL,
    outcomeNegativeLevel = NULL,
    predictorLevel = NULL,
    predictorNegativeLevel = NULL
  )
  # May warn about perfect separation
  expect_s3_class(result, "oddsratioResults")
})

test_that("oddsratio handles zero cells", {
  result <- oddsratio(
    data = oddsratio_zerocell,
    explanatory = "predictor",
    outcome = "outcome",
    outcomeLevel = NULL,
    outcomeNegativeLevel = NULL,
    predictorLevel = NULL,
    predictorNegativeLevel = NULL
  )
  # May warn about zero cells
  expect_s3_class(result, "oddsratioResults")
})

test_that("oddsratio hardens syntax-mode codegen (Category G1) against quote-breaking and undeclared options", {
  df <- data.frame(
    y = factor(c("Alive", "Dead", "Alive", "Dead")),
    x = factor(c("Pos", "Neg", "Pos", "Neg"))
  )

  # 1. Quote-breaking option values cannot inject top-level arguments
  opts1 <- oddsratioOptions$new(
    explanatory = c("x = 1, usePenalized = {stop(\"X\"); TRUE}, z", "weird\"name", "path\\to\\thing"),
    outcome = "y",
    outcomeLevel = "Dead\", usePenalized = {stop(\"X\"); TRUE}, outcomeNegativeLevel = \"Alive",
    outcomeNegativeLevel = "Alive"
  )
  an1 <- oddsratioClass$new(options = opts1, data = df)
  src1 <- an1$asSource()
  parsed1 <- parse(text = src1)[[1]]
  expect_null(as.list(parsed1)$usePenalized)
  expect_identical(as.list(parsed1)$outcomeLevel, opts1$outcomeLevel)
  expect_identical(eval(as.list(parsed1)$explanatory), opts1$explanatory)

  # 2. Undeclared options injected into the option list are ignored by .sourcifyOption
  opts2 <- oddsratioOptions$new(explanatory = "x", outcome = "y")
  evil <- jmvcore::OptionBool$new("x = 1, y = {stop(\"X\")}, z", FALSE, default = TRUE)
  opts2$.__enclos_env__$super$.addOption(evil)
  an2 <- oddsratioClass$new(options = opts2, data = df)
  parsed2 <- parse(text = an2$asSource())[[1]]
  expect_false(any(c("x", "y", "z") %in% names(as.list(parsed2))))
})

test_that("oddsratio resets cached nomogram state across .run() calls (Category I5)", {
  set.seed(101)
  df <- data.frame(
    y = factor(sample(c("Alive", "Dead"), 60, replace = TRUE)),
    x = factor(sample(c("Neg", "Pos"), 60, replace = TRUE)),
    age = rnorm(60, 50, 10)
  )
  opts <- oddsratioOptions$new(
    explanatory = c("x", "age"),
    outcome = "y",
    outcomeLevel = "Dead",
    outcomeNegativeLevel = "Alive",
    showNomogram = TRUE,
    diagnosticPredictor = "x",
    predictorLevel = "Pos",
    predictorNegativeLevel = "Neg"
  )
  an <- oddsratioClass$new(options = opts, data = df)
  an$run()
  expect_false(is.null(an$.__enclos_env__$private$.nom_object))
  expect_false(is.null(an$results$plot_nomogram$state))

  # Re-run on the same instance after clearing outcome levels (early return)
  opts_cleared <- oddsratioOptions$new(
    explanatory = c("x", "age"),
    outcome = "y",
    outcomeLevel = NULL,
    outcomeNegativeLevel = NULL,
    showNomogram = TRUE
  )
  an$.__enclos_env__$private$.options <- opts_cleared
  an$run()
  expect_null(an$.__enclos_env__$private$.nom_object)
  expect_null(an$results$plot_nomogram$state)
})

