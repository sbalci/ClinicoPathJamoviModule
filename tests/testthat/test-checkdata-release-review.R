# Regression cover for the defects found during the checkdata release review.
# Each block fails against the pre-review backend.

library(testthat)

cd_outliers <- function(x, ...) {
  checkdata(data = data.frame(lab = x), var = "lab", showOutliers = TRUE, ...)$outliers$asDF
}

test_that("the modified Z-score matches Iglewicz & Hoaglin", {
  # The scale correction was applied twice: R's mad() already multiplies by
  # constant = 1.4826, and the code multiplied by 0.6745 = 1/1.4826 again. Every
  # modified Z came out 1.4826x too small, turning the >3.5 cut-off into an
  # effective >5.19 and making the method labelled "most robust" the least
  # sensitive of the three.
  ns <- asNamespace("ClinicoPath")
  obj <- get("checkdataClass", ns)$new(
    options = get("checkdataOptions", ns)$new(var = "lab", outlierTransform = "none"),
    data = data.frame(lab = rnorm(50)))
  detect <- obj$.__enclos_env__$private$.advancedOutlierDetection

  set.seed(1)
  x <- c(rnorm(100, 5, 0.5), 7.3, 7.4, 7.2)
  res <- detect(x)

  med <- median(x)
  # NIST e-Handbook 1.3.5.17 / Iglewicz & Hoaglin (1993)
  expected <- 0.6745 * (x - med) / median(abs(x - med))

  got <- res$all_methods$mad
  expect_false(is.null(got))
  # The published constant 0.6745 is a 4-dp rounding of 1/1.4826 = 0.674490...,
  # so the textbook form and the mad(constant = 1.4826) form agree to ~1.5e-5,
  # not exactly. The implementation uses the exact form; assert against the
  # published one loosely and against the exact equivalent tightly below.
  expect_equal(unname(got$scores), unname(expected[got$indices]), tolerance = 1e-4)

  # and the equivalent single-correction form
  expect_equal(unname(got$scores),
               unname(((x - med) / mad(x, constant = 1.4826))[got$indices]),
               tolerance = 1e-8)
})

test_that("moderate outliers are no longer missed by the MAD method", {
  # At 3.5 SD from the mean the old formula flagged 0 of 900 contaminants across
  # 300 replicates; the correct one flags a substantial fraction.
  set.seed(42)
  x <- c(rnorm(100, 5, 0.5), 7.3, 7.35, 7.25)

  ns <- asNamespace("ClinicoPath")
  obj <- get("checkdataClass", ns)$new(
    options = get("checkdataOptions", ns)$new(var = "lab"),
    data = data.frame(lab = x))
  res <- obj$.__enclos_env__$private$.advancedOutlierDetection(x)

  expect_true(all(101:103 %in% res$all_methods$mad$indices))
  # all three methods should agree on contaminants this far out
  expect_true(all(101:103 %in% res$outlier_indices))
})

test_that("the per-method flag columns say which method fired", {
  # ifelse(flagged, "", " - ") was inverted: a method that DID detect the point
  # rendered as an empty cell and one that did not rendered as a dash, so a row
  # whose severity read "3/3 methods" showed three blank method columns.
  set.seed(1)
  df <- cd_outliers(c(rnorm(100, 5, 0.5), 7.3, 7.4, 7.2))

  expect_gt(nrow(df), 0)
  expect_true(all(df$zscoreFlag %in% c("Yes", "-")))
  expect_true(all(df$iqrFlag %in% c("Yes", "-")))
  expect_true(all(df$madFlag %in% c("Yes", "-", "N/A")))

  # a row reporting 3 methods must show three "Yes"
  three <- grepl("3 (of|/) 3 methods", df$severity)
  expect_true(any(three))
  expect_true(all(df$zscoreFlag[three] == "Yes"))
  expect_true(all(df$iqrFlag[three] == "Yes"))
  expect_true(all(df$madFlag[three] == "Yes"))

  # the count in the severity label must equal the number of "Yes" flags
  for (i in seq_len(nrow(df))) {
    n_yes <- sum(c(df$zscoreFlag[i], df$iqrFlag[i], df$madFlag[i]) == "Yes")
    n_lab <- as.integer(sub(".*(?:\\(|flagged by )([0-9]+)(?:/| of ).*", "\\1", df$severity[i]))
    expect_equal(n_yes, n_lab, info = df$severity[i])
  }
})

test_that("the MCAR option produces output instead of doing nothing", {
  # The block was guarded on `!is.null(data_context)`, and both call sites pass
  # only `variable`, so it was unreachable: ticking the box did nothing while the
  # option's description promised "a formal test vs. heuristic assessment".
  set.seed(3)
  d <- data.frame(lab = c(rnorm(60, 5, 1), rep(NA, 25)))

  on_df <- checkdata(data = d, var = "lab", showPatterns = TRUE, mcarTest = TRUE)$patterns$asDF
  off_df <- checkdata(data = d, var = "lab", showPatterns = TRUE, mcarTest = FALSE)$patterns$asDF

  expect_true(any(grepl("Mcar|MCAR", on_df$pattern)))
  expect_false(any(grepl("Mcar|MCAR", off_df$pattern)))

  msg <- on_df$description[grepl("Mcar|MCAR", on_df$pattern)]
  # it must say the test is not computable here, not claim to have run one
  expect_match(msg, "multivariate")
  expect_match(msg, "cannot be computed")
  expect_false(grepl("p\\s*=", msg))
})

test_that("the runs test matches the Wald-Wolfowitz formulas", {
  ns <- asNamespace("ClinicoPath")
  obj <- get("checkdataClass", ns)$new(
    options = get("checkdataOptions", ns)$new(var = "lab"),
    data = data.frame(lab = rnorm(30)))
  analyze <- obj$.__enclos_env__$private$.analyzeMissingPatterns

  set.seed(5)
  v <- rnorm(60); v[c(2, 9, 14, 27, 33, 41, 52, 58)] <- NA
  p <- analyze(v)

  n_miss <- sum(is.na(v)); n_comp <- sum(!is.na(v)); n <- length(v)
  n_runs <- length(rle(is.na(v))$lengths)
  expected_runs <- 2 * n_miss * n_comp / n + 1
  runs_var <- (2 * n_miss * n_comp * (2 * n_miss * n_comp - n)) / (n^2 * (n - 1))
  z <- (n_runs - expected_runs) / sqrt(runs_var)
  p_expected <- 2 * pnorm(-abs(z))

  txt <- paste(unlist(p), collapse = " ")
  expect_match(txt, "runs test")
  expect_match(txt, sprintf("p=%.3f", p_expected), fixed = TRUE)
})

test_that("skewness matches the population moment coefficient g1", {
  ns <- asNamespace("ClinicoPath")
  obj <- get("checkdataClass", ns)$new(
    options = get("checkdataOptions", ns)$new(var = "lab"),
    data = data.frame(lab = rnorm(30)))
  skew <- obj$.__enclos_env__$private$.computeSkewness

  set.seed(8)
  x <- rexp(200, 0.5)
  mu <- mean(x)
  g1 <- (sum((x - mu)^3) / length(x)) / (sum((x - mu)^2) / length(x))^1.5

  expect_equal(skew(x), g1, tolerance = 1e-10)
  expect_equal(skew(c(1, 2)), 0)          # n < 3
  expect_equal(skew(rep(4, 20)), 0)       # zero variance
})

# ---------------------------------------------------------------------------
# User-facing notes for the heuristic clinical checks.
#
# clinicalValidation defaults to TRUE while showCaveats defaults to FALSE, so by
# default the analysis lowers the headline quality grade using hard-coded
# reference ranges, a possibly-guessed unit system and name pattern-matching,
# with nothing on screen to say so. These notes close that gap.
# ---------------------------------------------------------------------------

cd_clinical <- function(...) {
  set.seed(2)
  d <- data.frame(age = c(rnorm(80, 55, 12), 250, -4, 190))   # name triggers checks
  checkdata(data = d, var = "age", showPatterns = TRUE, ...)
}

table_note <- function(tbl, key) {
  e <- tbl$.__enclos_env__$private$.notes[[key]]
  if (is.null(e)) return(NA_character_)
  get("note", e)
}

test_that("the quality score explains the clinical penalty it just applied", {
  qt <- as.character(cd_clinical()$qualityText$content)

  expect_match(qt, "NOTE ON THE CLINICAL PENALTY")
  expect_match(qt, "not validated reference ranges")
  expect_match(qt, "matching the variable NAME")
  expect_match(qt, "Confirm each flag against your study protocol")
  # it must name the actual cost, not a generic warning
  expect_match(qt, "This component cost [0-9]+ points")

  # and it must sit AFTER the grade, not split the component breakdown
  expect_lt(regexpr("HEURISTIC GRADE", qt), regexpr("NOTE ON THE CLINICAL PENALTY", qt))
})

test_that("the note says units were inferred from the data range", {
  qt <- as.character(cd_clinical()$qualityText$content)
  expect_match(qt, "inferred from the data\\s+range")
})

test_that("no clinical note appears when no clinical penalty was applied", {
  # a variable whose name triggers nothing, with unremarkable values
  set.seed(4)
  d <- data.frame(widget_count = rnorm(80, 50, 5))
  qt <- as.character(checkdata(data = d, var = "widget_count",
                               showPatterns = TRUE)$qualityText$content)
  expect_false(grepl("NOTE ON THE CLINICAL PENALTY", qt))
})

test_that("clinical validation rows carry a footnote on the patterns table", {
  res <- cd_clinical()
  df <- res$patterns$asDF
  expect_true(any(grepl("Clinical Validation", df$pattern)))

  note <- table_note(res$patterns, "clinicalHeuristic")
  expect_false(is.na(note))
  expect_match(note, "heuristic screening flags, not clinical judgements")
  expect_match(note, "pattern-matching the variable NAME")
  expect_match(note, "misread unit")
  # setNote allows only i/em/b/strong/sub/sup - no other markup may leak in
  tags <- unlist(regmatches(note, gregexpr("</?([a-zA-Z]+)", note)))
  tags <- unique(gsub("</?", "", tags))
  expect_true(all(tags %in% c("i", "em", "b", "strong", "sub", "sup")))
})

test_that("the caveats panel warns about mixed scales under a transform", {
  set.seed(6)
  d <- data.frame(lab = c(rlnorm(100, 1, 0.5), 90, 95))
  html <- as.character(checkdata(data = d, var = "lab", outlierTransform = "log",
                                 showCaveats = TRUE)$caveatsAssumptions$content)
  expect_match(html, "Mixed scales when a transform is applied")
  expect_match(html, "transformed scale")
})

# ---- 2026-09-03 release review: imperial removal and the four follow-up fixes ----

test_that("the unit-system option is gone and clinical checks assume kg / cm / m", {
  expect_false("unitSystem" %in% names(formals(checkdata)))
  pats <- function(d, v) do.call(checkdata, list(data = d, var = v, showPatterns = TRUE))$patterns$asDF$description
  set.seed(3)
  expect_match(paste(pats(data.frame(weight = c(rnorm(50, 75, 10), 260)), "weight"), collapse = " "),
               "Weight >200 kg")
  expect_match(paste(pats(data.frame(height = c(rnorm(50, 1.7, 0.08), 3.1)), "height"), collapse = " "),
               "outside 0.5-2.5 m")
  expect_false(any(grepl("PLAUSIBILITY", pats(data.frame(height = rnorm(50, 170, 8)), "height"))))
  expect_false(any(grepl("auto-detect", pats(data.frame(height = rnorm(50, 170, 8)), "height"))))
})

test_that("a constant variable fills every Distribution cell", {
  d <- checkdata(data = data.frame(x = rep(5, 30)), var = "x", showDistribution = TRUE)$distribution$asDF
  expect_false(any(is.na(d$interpretation)))
  expect_match(d$interpretation[d$metric == "Median"], "constant")
  expect_match(d$interpretation[grepl("Coefficient", d$metric)], "No variability")
})

test_that("the Data Patterns rare-category rule follows rareCategoryThreshold", {
  g <- data.frame(g = factor(c(rep("A", 120), rep("B", 70), rep("C", 8), rep("D", 2))))
  at5 <- checkdata(data = g, var = "g", showPatterns = TRUE, rareCategoryThreshold = 5)$patterns$asDF$description
  at1 <- checkdata(data = g, var = "g", showPatterns = TRUE, rareCategoryThreshold = 1)$patterns$asDF$description
  expect_match(grep("Rare categories", at5, value = TRUE), "below 5.0%.*C, D")
  expect_false(any(grepl("Rare categories", at1)))
})

test_that("a capped duplicates list says so in a footnote", {
  set.seed(1)
  many <- checkdata(data = data.frame(k = factor(sample(letters[1:25], 300, TRUE))),
                    var = "k", showDuplicates = TRUE)$duplicates
  expect_equal(nrow(many$asDF), 20)
  expect_match(many$notes$truncated$note, "20 most frequent of 25 categories")
  few <- checkdata(data = data.frame(v = rep(1:5, each = 3)), var = "v", showDuplicates = TRUE)$duplicates
  expect_equal(nrow(few$asDF), 5)
  expect_null(few$notes$truncated)
})

test_that("outlier flags and consensus rows match an independent computation", {
  set.seed(11)
  x <- c(round(rnorm(60, 100, 10), 1), 160, 175, 40)
  r <- checkdata(data = data.frame(lab = x), var = "lab", showDistribution = TRUE)
  ms <- r$outlierMethodSummary$asDF; ot <- r$outliers$asDF; ds <- r$distribution$asDF

  z_flag   <- which(abs(scale(x)[, 1]) > 3)
  q        <- quantile(x, c(.25, .75)); iqr <- diff(q)
  iqr_flag <- which(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
  mz       <- 0.6745 * (x - median(x)) / median(abs(x - median(x)))   # Iglewicz & Hoaglin
  mad_flag <- which(abs(mz) > 3.5)
  all_flags <- c(z_flag, iqr_flag, mad_flag)
  consensus <- sort(unique(all_flags[duplicated(all_flags)]))

  expect_equal(ms$outliers_detected, c(length(z_flag), length(iqr_flag), length(mad_flag)))
  expect_equal(ot$rowNumber, consensus)
  expect_equal(ot$zscore, round(scale(x)[consensus, 1], 3), tolerance = 1e-6)
  expect_equal(ds$value[ds$metric == "Skewness"], round(moments::skewness(x), 3))
  expect_equal(ds$value[grepl("MAD", ds$metric)], round(mad(x), 4))
  expect_equal(ds$value[grepl("Interquartile", ds$metric)], round(unname(iqr), 4))
})

test_that("the runs test and dropout interval match hand computation", {
  set.seed(5)
  y <- rnorm(120); y[sample(120, 30)] <- NA; y[100:120] <- NA
  desc <- checkdata(data = data.frame(v = y), var = "v", showPatterns = TRUE)$patterns$asDF$description
  m <- is.na(y); n1 <- sum(m); n2 <- sum(!m); N <- n1 + n2
  R <- length(rle(m)$lengths); ER <- 2 * n1 * n2 / N + 1
  expect_match(grep("runs test", desc, value = TRUE), sprintf("%d vs %.1f expected", R, ER))
  lq <- round(N * 0.75); k <- sum(which(m) > lq)
  w <- Hmisc::binconf(k, n1, method = "wilson")
  expect_match(grep("dropout", desc, value = TRUE),
               sprintf("%.1f%% of missing values.*95%% CI: %.1f%%-%.1f%%", 100 * k / n1, 100 * w[2], 100 * w[3]))
})
