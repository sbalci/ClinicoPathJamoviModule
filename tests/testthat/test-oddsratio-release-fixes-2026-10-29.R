# Regression tests for the 2026-10-29 oddsratio release-check findings:
# Hosmer-Lemeshow df, forest plot under separation, CI-method note, wording,
# and a numeric 0/1 default diagnostic predictor.

library(testthat)

if (!exists("oddsratio", mode = "function") || !exists("oddsratioClass")) {
  for (d in c("R", "../R", "../../R")) {
    if (file.exists(file.path(d, "oddsratio.h.R")) && file.exists(file.path(d, "oddsratio.b.R"))) {
      for (u in c("utils.R", "utils-formula.R")) {
        if (file.exists(file.path(d, u))) source(file.path(d, u), local = globalenv())
      }
      if (!exists(".", mode = "function")) assign(".", function(x, ...) x, envir = globalenv())
      source(file.path(d, "oddsratio.h.R"), local = globalenv())
      source(file.path(d, "oddsratio.b.R"), local = globalenv())
      break
    }
  }
}

rf_strip <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  txt <- gsub("<[^>]+>", " ", paste(x, collapse = " "))
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&#39;", "'", txt, fixed = TRUE)
  gsub("\\s+", " ", trimws(txt))
}

rf_all_text <- function(res) {
  items <- c("text", "text2", "diagnosticMetrics", "errors", "strongWarnings",
             "warnings", "infoMessages", "diagnosticTestExplanation", "nomogramAnalysisExplanation")
  paste(vapply(items, function(i) {
    it <- tryCatch(res[[i]], error = function(e) NULL)
    if (is.null(it)) "" else rf_strip(it$content)
  }, ""), collapse = " | ")
}

# Two binary predictors -> 4 distinct fitted risks -> the decile breaks
# collapse to 3 Hosmer-Lemeshow groups, so df must be 1, not 8.
rf_four_risk <- function() {
  a <- rep(c(0, 0, 1, 1), times = c(40, 30, 30, 50))
  b <- rep(c(0, 1, 0, 1), times = c(40, 30, 30, 50))
  y <- c(rep(c(1, 0), c(4, 36)), rep(c(1, 0), c(15, 15)),
         rep(c(1, 0), c(15, 15)), rep(c(1, 0), c(30, 20)))
  data.frame(
    outcome = factor(ifelse(y == 1, "Event", "NoEvent"), levels = c("NoEvent", "Event")),
    testA = factor(ifelse(a == 1, "Pos", "Neg"), levels = c("Neg", "Pos")),
    testB = factor(ifelse(b == 1, "Pos", "Neg"), levels = c("Neg", "Pos")))
}

test_that("Hosmer-Lemeshow df equals groups formed - 2 and matches an independent HL", {
  d <- rf_four_risk()
  res <- oddsratio(d, c("testA", "testB"), "outcome",
                   outcomeLevel = "Event", outcomeNegativeLevel = "NoEvent",
                   predictorLevel = NULL, predictorNegativeLevel = NULL)
  t2 <- rf_strip(res$text2$content)
  expect_false(grepl("Chi-sq(8)", t2, fixed = TRUE))

  m <- glm(I(outcome == "Event") ~ testA + testB, binomial, d)
  yhat <- fitted(m); y <- m$y
  qq <- unique(quantile(yhat, seq(0, 1, 0.1)))
  g <- cut(yhat, qq, include.lowest = TRUE)
  o <- tapply(y, g, sum); e <- tapply(yhat, g, sum); n <- as.vector(table(g))
  chi <- sum((o - e)^2 / e + (o - e)^2 / (n - e))
  df <- length(n) - 2L
  expect_equal(df, 1L)
  p <- pchisq(chi, df, lower.tail = FALSE)
  expect_match(t2, sprintf("H&L = Chi-sq(%d) %.2f (p = %.3f)", df, chi, p), fixed = TRUE)
  if (requireNamespace("ResourceSelection", quietly = TRUE)) {
    hl <- suppressWarnings(ResourceSelection::hoslem.test(y, yhat, g = 10))
    expect_equal(unname(hl$statistic), chi, tolerance = 1e-8)
    expect_equal(unname(hl$parameter), df)
    expect_equal(hl$p.value, p, tolerance = 1e-8)
  }
  expect_match(t2, "C-statistic (in-sample) =", fixed = TRUE)
})

test_that("CI note names profile likelihood, the Wald fallback, and Firth", {
  d <- rf_four_risk()
  res <- oddsratio(d, c("testA", "testB"), "outcome",
                   outcomeLevel = "Event", outcomeNegativeLevel = "NoEvent",
                   predictorLevel = NULL, predictorNegativeLevel = NULL)
  expect_match(rf_strip(res$text$content),
               "Confidence intervals: profile likelihood. P-values: Wald test.", fixed = TRUE)

  # Complete separation: profiling fails and finalfit falls back to Wald.
  d_sep <- data.frame(
    outcome = factor(c(rep("NoEvent", 25), rep("Event", 25)), levels = c("NoEvent", "Event")),
    exposure = factor(c(rep("Neg", 25), rep("Pos", 25)), levels = c("Neg", "Pos")),
    age = seq(41, 90, length.out = 50))
  res_sep <- suppressWarnings(oddsratio(d_sep, c("exposure", "age"), "outcome",
                   outcomeLevel = "Event", outcomeNegativeLevel = "NoEvent",
                   predictorLevel = NULL, predictorNegativeLevel = NULL))
  expect_match(rf_strip(res_sep$text$content),
               "Confidence intervals: Wald (profile-likelihood intervals could not be computed for this model). P-values: Wald test.",
               fixed = TRUE)

  skip_if_not_installed("logistf")
  res_f <- oddsratio(d, c("testA", "testB"), "outcome",
                     outcomeLevel = "Event", outcomeNegativeLevel = "NoEvent",
                     predictorLevel = NULL, predictorNegativeLevel = NULL, usePenalized = TRUE)
  expect_match(rf_strip(res_f$text$content),
               "Confidence intervals: profile penalized likelihood. P-values: penalized likelihood-ratio test.",
               fixed = TRUE)
})

test_that("forest plot omits terms the table marks not estimable and captions them", {
  set.seed(7)
  n <- 200
  grade <- factor(sample(c("G1", "G2", "G3"), n, TRUE))
  age <- round(rnorm(n, 60, 10))
  y <- rbinom(n, 1, plogis(-1 + 0.03 * (age - 60) + 0.7 * (grade == "G2")))
  y[grade == "G3"] <- 1  # G3 quasi-separates the outcome
  d <- data.frame(
    outcome = factor(ifelse(y == 1, "Dead", "Alive"), levels = c("Alive", "Dead")),
    grade = grade, age = age)

  a <- oddsratioClass$new(
    options = oddsratioOptions$new(
      explanatory = c("grade", "age"), outcome = "outcome",
      outcomeLevel = "Dead", outcomeNegativeLevel = "Alive",
      predictorLevel = NULL, predictorNegativeLevel = NULL),
    data = d)
  a$init()
  suppressWarnings(a$run())
  expect_match(rf_strip(a$results$text$content), "not estimable", fixed = TRUE)
  st <- a$results$plot$state
  expect_identical(st$excludedFitIds, "gradeG3")
  expect_match(st$excludedLabels, "G3", fixed = TRUE)

  captured <- new.env()
  trace("or_plot", where = asNamespace("finalfit"), print = FALSE,
        tracer = bquote({
          assign("factorlist", factorlist, envir = .(captured))
          assign("plot_opts", plot_opts, envir = .(captured))
        }))
  on.exit(untrace("or_plot", where = asNamespace("finalfit")), add = TRUE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  ok <- suppressWarnings(a$.__enclos_env__$private$.plot(a$results$plot, ggtheme = NULL, theme = NULL))
  expect_true(isTRUE(ok))
  expect_false("gradeG3" %in% as.character(captured$factorlist$fit_id))
  expect_true(all(c("gradeG1", "gradeG2", "age") %in% as.character(captured$factorlist$fit_id)))
  caps <- unlist(lapply(captured$plot_opts, function(o) if (inherits(o, c("labels", "ggplot2::labels"))) o$caption))
  expect_match(gsub("\n", " ", paste(caps, collapse = " ")), "Not shown (odds ratio not estimable)", fixed = TRUE)
})

test_that("diagnostics wording: test-result levels, zero cells, no non-diagnostic examples", {
  d <- data.frame(
    outcome = factor(c(rep("Event", 20), rep("NoEvent", 5), rep("NoEvent", 15)),
                     levels = c("NoEvent", "Event")),
    marker = factor(c(rep("Pos", 20), rep("Pos", 5), rep("Neg", 15)), levels = c("Neg", "Pos")))
  res <- suppressWarnings(oddsratio(d, "marker", "outcome",
                   outcomeLevel = "Event", outcomeNegativeLevel = "NoEvent",
                   predictorLevel = "Pos", predictorNegativeLevel = "Neg",
                   showNomogram = TRUE, showExplanations = TRUE))
  txt <- rf_all_text(res)
  expect_match(txt, "is treated as a positive test result as specified (negative test result: 'Neg')", fixed = TRUE)
  expect_false(grepl("positive/exposure category", txt, fixed = TRUE))
  expect_false(grepl("may affect odds ratio calculation", txt, fixed = TRUE))
  expect_match(txt, "Zero cells detected in the 2x2 table. Sensitivity or specificity may be exactly 0 or 1", fixed = TRUE)
  expect_match(txt, "Examples: LVI (Absent/Present), an immunostain (Negative/Positive)", fixed = TRUE)
  expect_false(grepl("Sex (Male/Female)", txt, fixed = TRUE))
  expect_false(grepl("Treatment (Control/Treated)", txt, fixed = TRUE))
})

test_that("a numeric 0/1 first explanatory variable is accepted as the default diagnostic predictor", {
  d <- data.frame(
    outcome = factor(c(rep("Event", 18), rep("NoEvent", 6), rep("Event", 4), rep("NoEvent", 22)),
                     levels = c("NoEvent", "Event")),
    marker = c(rep(1, 24), rep(0, 26)))
  res <- suppressWarnings(oddsratio(d, "marker", "outcome",
                   outcomeLevel = "Event", outcomeNegativeLevel = "NoEvent",
                   predictorLevel = "1", predictorNegativeLevel = "0", showNomogram = TRUE))
  dm <- rf_strip(res$diagnosticMetrics$content)
  expect_false(grepl("could not be formed", dm, fixed = TRUE))
  # 18 / (18 + 4) = 81.8%; 22 / (22 + 6) = 78.6%
  expect_match(dm, "Sensitivity: 81.8%", fixed = TRUE)
  expect_match(dm, "Specificity: 78.6%", fixed = TRUE)
})
