# Regression tests for the 2026-10-30 independent review of oddsratio (F1 to F10).
# development-ideas/oddsratio-independent-review-2026-10-30.md

library(testthat)

NE <- "not estimable"

rr_or <- function(d, expl, pos = "Y", neg = "N", dp = NULL, pl = NULL, nl = NULL, ...) {
  suppressWarnings(do.call(ClinicoPath::oddsratio, c(
    list(data = d, explanatory = expl, outcome = "y", outcomeLevel = pos,
         outcomeNegativeLevel = neg, diagnosticPredictor = dp, predictorLevel = pl,
         predictorNegativeLevel = nl), list(...))))
}

# Native finalfit result grid; first row retains the display headers.
rr_table <- function(res) {
  tb <- res$fittedTable
  headers <- vapply(tb$columns, function(x) x$title, character(1))
  cells <- as.matrix(tb$asDF)
  cells[is.na(cells)] <- ""
  unname(rbind(headers, cells))
}
rr_txt <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " ")))
rr_notices <- function(res, item = "strongWarnings") rr_txt(res[[item]]$content)

# a,b = Y,N in arm P; c,d = Y,N in arm A
rr_2x2 <- function(a, b, c, d) data.frame(
  y = factor(c(rep("Y", a), rep("N", b), rep("Y", c), rep("N", d)), levels = c("N", "Y")),
  x = factor(c(rep("P", a + b), rep("A", c + d)), levels = c("A", "P")))

rr_cut <- function(unit, rising) {
  x <- 1:50
  data.frame(y = factor(ifelse(x > 25, "Y", "N"), levels = c("N", "Y")),
             m = if (rising) x * unit else (50 - x) * unit)
}

rr_ki <- function() {
  set.seed(3)
  k <- c(runif(30, 1, 29), runif(30, 31, 95))
  d <- data.frame(y = factor(c(rep("N", 30), rep("Y", 30)), levels = c("N", "Y")),
                  ki = round(k, 1), age = rnorm(60, 60, 8))
  d$ki2 <- 100 - d$ki
  d
}

rr_finite <- function() {
  set.seed(5)
  xx <- rnorm(400, .002, .001)
  d <- data.frame(y = factor(ifelse(runif(400) < plogis(-2 + 1000 * (xx - .002)), "Y", "N"),
                             levels = c("N", "Y")), frac = xx)
  d$frac1000 <- d$frac * 1000
  d
}

# ---- F1 separation: flagged ------------------------------------------------

test_that("F1: a zero cell is not estimable, whichever way it reads", {
  for (d in list(rr_2x2(0, 100, 50, 50), rr_2x2(0, 300, 60, 240), rr_2x2(0, 10, 5, 5),
                 rr_2x2(50, 50, 0, 100))) {
    res <- rr_or(d, "x")
    tb <- rr_table(res)
    expect_identical(tb[3, 5], NE)
    expect_identical(tb[3, 6], NE)
    expect_match(rr_notices(res), "could not be estimated", fixed = TRUE)
    expect_match(rr_notices(res), "neither a large odds ratio nor evidence of no association", fixed = TRUE)
    expect_match(rr_txt(res$text$content), "no finite odds ratio exists", fixed = TRUE)
  }
})

test_that("F1: complete separation is caught at any unit and in both directions", {
  for (unit in c(1, 100, 1000, 1e6)) for (rising in c(TRUE, FALSE)) {
    res <- rr_or(rr_cut(unit, rising), "m")
    expect_identical(rr_table(res)[2, 5], NE)
    expect_true(nzchar(res$strongWarnings$content))
  }
})

test_that("F1: quasi-complete separation on a numeric tie is caught", {
  # Outcomes overlap only at x = 25, so the MLE is infinite with some rows moderate.
  x <- c(1:25, 25, 26:50)
  y <- c(rep("N", 25), "Y", rep("Y", 25))
  res <- rr_or(data.frame(y = factor(y, levels = c("N", "Y")), x = x), "x")
  expect_identical(rr_table(res)[2, 5], NE)
})

test_that("F1: a reference level with a zero cell flags every contrast against it", {
  d <- data.frame(y = factor(rep(c("N", "Y", "N", "N", "Y"), c(30, 20, 20, 10, 20)),
                             levels = c("N", "Y")),
                  g = factor(rep(c("a", "b", "c"), c(30, 40, 30))))
  tb <- rr_table(rr_or(d, "g"))
  expect_identical(tb[3:4, 5], c(NE, NE))
})

test_that("F1: Ki-67 and 100 - Ki-67 get the same verdict", {
  d <- rr_ki()
  expect_identical(rr_table(rr_or(d, "ki"))[2, 5], NE)
  expect_identical(rr_table(rr_or(d, "ki2"))[2, 5], NE)
})

test_that("F1: under complete separation no coefficient has a finite estimate", {
  # A covariate's own estimate is as arbitrary here as the marker's: its SE diverges
  # too. Its univariable column is unaffected and stays a number.
  res <- rr_or(rr_ki(), c("ki2", "age"))
  tb <- rr_table(res)
  expect_identical(tb[2:3, 6], c(NE, NE))
  expect_identical(tb[2, 5], NE)
  expect_false(identical(tb[3, 5], NE))
  expect_match(rr_notices(res), "for: ki2", fixed = TRUE)
})

test_that("F1: with a zero cell, the innocent covariate keeps its odds ratio", {
  d <- rr_2x2(0, 100, 50, 50)
  d$age <- 40 + (seq_len(nrow(d)) %% 37)
  res <- rr_or(d, c("x", "age"))
  tb <- rr_table(res)
  expect_identical(tb[3, 6], NE)
  expect_false(identical(tb[4, 6], NE))
  expect_false(grepl("age", rr_notices(res), fixed = TRUE))
})

test_that("F1: a separated level nested in another factor's level leaves that level alone", {
  # g = b sits wholly inside h = c, so both columns have a huge SE, but the h = c
  # rows with g = a still have both outcomes and determine its coefficient.
  d <- data.frame(
    y = factor(c(rep(c("N", "Y"), 10), rep(c("N", "Y", "Y"), 6), rep("Y", 4)), levels = c("N", "Y")),
    g = factor(c(rep("a", 38), rep("b", 4))),
    h = factor(c(rep("a", 20), rep("c", 22))))
  tb <- rr_table(rr_or(d, c("g", "h")))
  expect_identical(tb[tb[, 2] == "b", 6], NE)
  expect_false(identical(tb[tb[, 2] == "c", 6], NE))
})

test_that("F1: joint separation with no zero cell is caught", {
  du <- data.frame(y = factor(c(rep("Y", 36), rep("N", 12), rep("Y", 24), rep("N", 48)), levels = c("N", "Y")),
                   x1 = factor(c(rep("P", 48), rep("A", 72)), levels = c("A", "P")), x2 = c(rep(NA, 20), 1:100))
  expect_identical(rr_table(rr_or(du, c("x1", "x2")))[3:4, 6], c(NE, NE))
})

test_that("F1: no nomogram from a separated model, and Hosmer-Lemeshow says why", {
  d <- rr_2x2(0, 100, 50, 50)
  d$age <- 40 + (seq_len(nrow(d)) %% 37)
  res <- rr_or(d, c("x", "age"), dp = "x", pl = "P", nl = "A", showNomogram = TRUE)
  expect_null(res$plot_nomogram$state)
  expect_match(rr_txt(res$nomogram$content), "Prediction nomogram not generated", fixed = TRUE)
  expect_match(rr_txt(res$text2$content), "not applicable (separation)", fixed = TRUE)
})

# ---- F1/F3 separation: not flagged ------------------------------------------

test_that("F3: a tiny-unit slope and a large-unit slope are not called separated", {
  set.seed(9)
  n <- 400
  z <- rnorm(n)
  d <- data.frame(y = factor(rbinom(n, 1, plogis(z)), labels = c("N", "Y")),
                  big = z * 1e6, tiny = z * 1e-6)
  for (v in c("big", "tiny")) {
    res <- rr_or(d, v)
    expect_false(identical(rr_table(res)[2, 5], NE))
    expect_false(grepl("separate", rr_notices(res), fixed = TRUE))
  }
})

test_that("F3: x and 1000x get the same verdict, and a nomogram", {
  d <- rr_finite()
  for (v in c("frac", "frac1000")) {
    res <- rr_or(d, v, showNomogram = TRUE)
    expect_false(identical(rr_table(res)[2, 5], NE))
    expect_false(grepl("separate the outcome", rr_notices(res), fixed = TRUE))
    expect_true(inherits(res$plot_nomogram$state$nom, "nomogram"))
  }
})

test_that("F3: a per-unit OR too large to print says so, and gives the OR per SD", {
  d <- rr_finite()
  res <- rr_or(d, "frac")
  expect_identical(rr_table(res)[2, 5], "too extreme to print")
  expect_match(rr_notices(res, "warnings"), "This is not separation", fixed = TRUE)
  g <- glm(y ~ frac, binomial, d)
  per_sd <- exp(coef(g)[["frac"]] * sd(d$frac))
  expect_match(rr_notices(res, "infoMessages"), sprintf("units) is %.3g", per_sd), fixed = TRUE)
  expect_identical(res$plot$state$excludedFitIds, "frac")
  expect_identical(res$plot$state$extremeLabels, "frac")
  expect_length(res$plot$state$excludedLabels, 0L)
})

test_that("F1: a large finite estimate with an extreme fitted risk is not separation", {
  # Each of these passes se * sd(column) > 10 with rows at fitted 0 or 1; each has
  # a finite MLE, so the threshold alone would be a false alarm.
  x <- 1:50
  y <- as.integer(x > 25)
  y[c(25, 26)] <- c(1L, 0L)                      # one reversal in the overlap
  reversal <- data.frame(y = factor(y, labels = c("N", "Y")), x = x)
  set.seed(9)
  n <- 400
  z <- rnorm(n)
  strong <- data.frame(y = factor(rbinom(n, 1, plogis(30 * z)), labels = c("N", "Y")), x = z)
  outlier <- data.frame(y = rbinom(n, 1, plogis(z)), x = z)
  outlier$x[which.max(z)] <- 1e6
  outlier$y[which.max(z)] <- 1L
  outlier$y <- factor(outlier$y, labels = c("N", "Y"))
  for (d in list(reversal, strong, outlier)) {
    expect_false(identical(rr_table(rr_or(d, "x"))[2, 5], NE))
    expect_false(grepl("could not be estimated", rr_notices(rr_or(d, "x")), fixed = TRUE))
  }
})

test_that("F3: Firth tables are never flagged", {
  skip_if_not_installed("logistf")
  for (d in list(rr_2x2(0, 100, 50, 50), rr_finite())) {
    expl <- intersect(c("x", "frac"), names(d))
    res <- rr_or(d, expl, usePenalized = TRUE)
    expect_false(any(rr_table(res) == NE))
    expect_false(grepl("could not be estimated", rr_notices(res), fixed = TRUE))
  }
})

# ---- F2 / F10 configuration --------------------------------------------------

test_that("F2: variables sharing a coefficient name are refused, naming both", {
  set.seed(10)
  n <- 200
  d <- data.frame(y = factor(rbinom(n, 1, .4), labels = c("N", "Y")),
                  grade = factor(sample(1:3, n, TRUE)), grade2 = round(rnorm(n, 50, 10)))
  for (firth in c(FALSE, TRUE)) {
    expect_error(rr_or(d, c("grade", "grade2"), usePenalized = firth),
                 "same model term name.*grade, grade2")
  }
  # the reference level joins on the same name
  names(d)[3] <- "grade1"
  expect_error(rr_or(d, c("grade", "grade1")), "same model term name")
  names(d)[3] <- "marker"
  expect_identical(nrow(rr_table(rr_or(d, c("grade", "marker")))), 5L)
})

test_that("F10: aliased predictors are refused with a reason", {
  set.seed(13)
  n <- 300
  d <- data.frame(arm = factor(sample(c("a", "b"), n, TRUE)), age = rnorm(n, 60, 10),
                  y = factor(rbinom(n, 1, .4), labels = c("N", "Y")))
  d$copy <- d$arm
  expect_error(rr_or(d, c("arm", "copy", "age")), "Perfect collinearity: copy")
})

test_that("F10: a large-mean numeric is not mistaken for an aliased one", {
  set.seed(13)
  n <- 300
  z <- rnorm(n)
  d <- data.frame(y = factor(rbinom(n, 1, plogis(z)), labels = c("N", "Y")), x = 1e9 + z)
  expect_false(identical(rr_table(rr_or(d, "x"))[2, 5], NE))
})

test_that("F10: a column called LP still draws its nomogram", {
  set.seed(13)
  n <- 300
  d <- data.frame(arm = factor(sample(c("a", "b"), n, TRUE)), LP = factor(sample(c("u", "v"), n, TRUE)),
                  y = factor(rbinom(n, 1, .4), labels = c("N", "Y")))
  a <- ClinicoPath:::oddsratioClass$new(
    options = ClinicoPath:::oddsratioOptions$new(
      explanatory = c("arm", "LP"), outcome = "y", outcomeLevel = "Y",
      outcomeNegativeLevel = "N", showNomogram = TRUE),
    data = d)
  a$init()
  suppressWarnings(a$run())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(a$.__enclos_env__$private$.plot_nomogram(a$results$plot_nomogram, NULL, NULL))
})

# ---- F4 to F9 ---------------------------------------------------------------

test_that("F4: Hosmer-Lemeshow is not reported on a saturated model", {
  set.seed(10)
  n <- 200
  d <- data.frame(y = factor(rbinom(n, 1, .4), labels = c("N", "Y")),
                  g4 = factor(sample(letters[1:4], n, TRUE)), g6 = factor(sample(letters[1:6], n, TRUE)))
  for (v in c("g4", "g6")) {
    expect_match(rr_txt(rr_or(d, v)$text2$content), "H&amp;L = not applicable", fixed = TRUE)
  }
})

test_that("F5: a per-unit OR that rounds to 1.00 (1.00-1.00) gets the OR per SD", {
  set.seed(6)
  pl <- round(rnorm(300, 250000, 70000))
  d <- data.frame(y = factor(ifelse(runif(300) < plogis(-3 + 1e-5 * pl), "Y", "N"), levels = c("N", "Y")),
                  platelets = pl)
  res <- rr_or(d, "platelets")
  expect_match(rr_table(res)[2, 5], "^1\\.00 \\(1\\.00-1\\.00")
  g <- glm(y ~ platelets, binomial, d)
  expect_match(rr_notices(res, "infoMessages"),
               sprintf("units) is %.3g", exp(coef(g)[[2]] * sd(pl))), fixed = TRUE)
})

test_that("F5: an ordinary numeric predictor gets no such note", {
  res <- rr_or(rr_finite(), "frac1000")
  expect_false(grepl("per one unit is unreadable", rr_notices(res, "infoMessages"), fixed = TRUE))
})

test_that("F6: the univariable column's sample is stated, and Firth reports Missing", {
  du <- data.frame(y = factor(c(rep("Y", 36), rep("N", 12), rep("Y", 24), rep("N", 48)), levels = c("N", "Y")),
                   x1 = factor(c(rep("P", 48), rep("A", 72)), levels = c("A", "P")),
                   x2 = c(rep(NA, 20), rnorm(100)))
  expect_match(rr_txt(rr_or(du, c("x1", "x2"))$text$content),
               "same 100 complete cases", fixed = TRUE)
  skip_if_not_installed("logistf")
  expect_match(rr_txt(rr_or(du, c("x1", "x2"), usePenalized = TRUE)$text2$content),
               "Missing: 20", fixed = TRUE)
})

test_that("F7: the outcome-window caution reaches the to-do and the explanations", {
  d <- rr_2x2(10, 40, 20, 30)
  expect_match(rr_txt(rr_or(d, "x", pos = NULL, neg = NULL)$todo$content),
               "censored or unequal follow-up", fixed = TRUE)
  expect_match(rr_txt(rr_or(d, "x", showExplanations = TRUE)$oddsRatioExplanation$content),
               "censored or unequal follow-up", fixed = TRUE)
})

test_that("F8: the nomogram texts state that its probabilities need cohort sampling", {
  d <- rr_2x2(10, 40, 20, 30)
  d$age <- 40 + (seq_len(nrow(d)) %% 37)
  res <- rr_or(d, c("x", "age"), showNomogram = TRUE, showExplanations = TRUE)
  expect_match(rr_txt(res$nomogram$content), "case-control sample", fixed = TRUE)
  expect_match(rr_txt(res$nomogramAnalysisExplanation$content), "case-control sample", fixed = TRUE)
})

test_that("F9: an infinite likelihood ratio names the cell that is actually empty", {
  mk <- function(tp, fn, fp, tn) data.frame(
    y = factor(c(rep("Y", tp + fn), rep("N", fp + tn)), levels = c("N", "Y")),
    t = factor(c(rep("P", tp), rep("A", fn), rep("P", fp), rep("A", tn)), levels = c("A", "P")),
    k = seq_len(tp + fn + fp + tn) + .5)
  neg <- rr_txt(rr_or(mk(5, 5, 10, 0), "k", dp = "t", pl = "P", nl = "A", showNomogram = TRUE)$diagnosticMetrics$content)
  expect_match(neg, "Negative LR: infinite (no true negatives)", fixed = TRUE)
  pos <- rr_txt(rr_or(mk(5, 5, 0, 10), "k", dp = "t", pl = "P", nl = "A", showNomogram = TRUE)$diagnosticMetrics$content)
  expect_match(pos, "Positive LR: infinite (no false positives)", fixed = TRUE)
})
