# Regression tests for the 2026-09-20 diagnosticmeta release pass.
#
# Every test here fails on the backend as it stood before that pass; the
# comment on each one names the defect it guards.

notes_of <- function(tbl) vapply(tbl$notes, function(x) paste(x$note, collapse = " "), "")
notice_text <- function(res) gsub("\\s+", " ", gsub("<[^>]+>", " ",
                                                    paste(res$notices$content, collapse = " ")))

dm_data <- function() data.frame(
  study = paste0("S", 1:12),
  tp = c(45, 30, 60, 22, 80, 15, 55, 38, 70, 25, 50, 33),
  fp = c(10,  8, 15,  5, 20,  4, 12,  9, 18,  6, 11,  7),
  fn = c( 5,  7, 10,  3, 15,  2,  8,  6, 12,  4,  9,  5),
  tn = c(90, 70,120, 45,160, 35,100, 80,140, 55, 95, 65),
  stringsAsFactors = FALSE)

dm_run <- function(d, ...) ClinicoPath::diagnosticmeta(
  data = d, study = "study", true_positives = "tp", false_positives = "fp",
  false_negatives = "fn", true_negatives = "tn", ...)

# A jamovi Nominal column holding integers: a factor carrying a `values`
# attribute. as.numeric() on one gives the LEVEL CODES, not the counts.
as_nominal_int <- function(x) {
  f <- factor(x, levels = sort(unique(x)))
  attr(f, "values") <- as.integer(levels(f))
  f
}


test_that("counts typed as Nominal integers are read as counts, not level codes", {
  # Was: 2x2 cells passed through as.numeric(factor), so a Nominal-typed count
  # column became its level index and the pooled estimates were nonsense
  # (85.8/89.4 became 59.0/50.0) with nothing in the output saying so.
  d <- dm_data()
  num <- dm_run(d)$bivariateresults$asDF

  f <- d
  for (cc in c("tp", "fp", "fn", "tn")) f[[cc]] <- as_nominal_int(f[[cc]])
  fac <- dm_run(f)$bivariateresults$asDF

  expect_equal(fac$estimate[1:2], num$estimate[1:2], tolerance = 1e-8)
  expect_gt(fac$estimate[1], 80)

  expect_equal(dm_run(f, show_individual_studies = TRUE)$individualstudies$asDF$tp[1], 45)
})


test_that("non-integer counts are excluded and disclosed, and impossible ones rejected", {
  # Was: fractional or proportion-valued cells were pooled silently.
  d <- dm_data(); d$tp[c(2, 5)] <- d$tp[c(2, 5)] + 0.6
  r <- dm_run(d, bivariate_analysis = FALSE, heterogeneity_analysis = TRUE,
              show_individual_studies = TRUE)
  expect_match(notice_text(r), "fractional counts", fixed = TRUE)
  expect_equal(nrow(r$individualstudies$asDF), 10)

  allfrac <- dm_data(); allfrac$tp <- allfrac$tp + 0.6
  expect_error(dm_run(allfrac), "whole counts|fractional")

  prop <- dm_data()
  prop$tp <- prop$tp / (prop$tp + prop$fn); prop$fn <- 1 - prop$tp
  prop$fp <- prop$fp / (prop$fp + prop$tn); prop$tn <- 1 - prop$fp
  expect_error(dm_run(prop), "proportion")
})


test_that("duplicate or missing study identifiers are made unique and disclosed", {
  # Was: two rows labelled "S1" were shown as one study's results twice.
  d <- dm_data(); d$study[2] <- "S1"; d$study[4] <- "S3"; d$study[6] <- NA
  r <- dm_run(d, show_individual_studies = TRUE)
  expect_match(notice_text(r), "not unique")
  lab <- r$individualstudies$asDF$study
  expect_equal(anyDuplicated(lab), 0L)
})


test_that("studies dropped for a missing covariate are disclosed", {
  # Was: the meta-regression silently used fewer studies than the pooled
  # estimates directly above it.
  d <- dm_data(); d$year <- c(2001:2010, NA, NA); d$year[3] <- NA
  r <- dm_run(d, covariate = "year", meta_regression = TRUE)
  expect_true(any(grepl("3 of 12 studies were excluded from the meta-regression",
                        notes_of(r$metaregression), fixed = TRUE)))
})


test_that("zero-cell handling says which models keep those studies and which drop them", {
  # Was: the notice claimed the 0.5 correction was applied "inside the models"
  # while the univariate models silently excluded the zero-cell studies.
  d <- dm_data(); d$fn[2] <- 0; d$fp[5] <- 0; d$fn[9] <- 0
  r <- dm_run(d, heterogeneity_analysis = TRUE)
  expect_match(notice_text(r), "bivariate model only", fixed = TRUE)
  expect_true(any(grepl("zero cell", notes_of(r$heterogeneity), fixed = TRUE)))
})


test_that("Deeks' test corrects every study and withholds a verdict when zeros dominate", {
  # Was: only the zero-cell studies were corrected - the small, near-perfect
  # ones - which tilts the test's own outcome against study size. On null data
  # (no study ever discarded) that produced "Significant asymmetry detected" in
  # two thirds of meta-analyses.
  d <- dm_data(); d$fn <- 0                       # every study has a zero cell
  pb <- dm_run(d, bivariate_analysis = FALSE, publication_bias = TRUE)$publicationbias$asDF
  expect_match(pb$interpretation[1], "Not interpretable")

  # correction applied to all 12 studies, so the statistic matches a hand fit
  # on the uniformly corrected table
  cd <- d; for (cc in c("tp", "fp", "fn", "tn")) cd[[cc]] <- cd[[cc]] + 0.5
  n1 <- d$tp + d$fn; n0 <- d$fp + d$tn
  ess <- 4 * n1 * n0 / (n1 + n0)
  ldor <- log((cd$tp * cd$tn) / (cd$fp * cd$fn))
  cf <- summary(stats::lm(ldor ~ I(1 / sqrt(ess)), weights = ess))$coefficients
  expect_equal(pb$statistic[1], cf[2, 3], tolerance = 1e-6)

  # a quarter or fewer zero-cell studies still gets a verdict, and it names
  # the alpha it used
  one <- dm_data(); one$fn[3] <- 0
  pb1 <- dm_run(one, bivariate_analysis = FALSE, publication_bias = TRUE)$publicationbias$asDF
  expect_match(pb1$interpretation[1], "p (<|>=) 0.05")
})


test_that("the fixed-effect method discloses what it removes", {
  # Was: choosing "fixed" silently deleted the prediction interval, the SROC
  # prediction region and the heterogeneity warning, left the forest plot's
  # pooled diamond without error bars, and kept notes pointing at the
  # prediction interval that no longer existed.
  set.seed(9); k <- 10
  n1 <- sample(40:200, k, TRUE); n0 <- sample(60:300, k, TRUE)
  se <- stats::plogis(stats::rnorm(k, stats::qlogis(0.80), 0.9))
  sp <- stats::plogis(stats::rnorm(k, stats::qlogis(0.85), 0.9))
  d <- data.frame(study = paste0("S", seq_len(k)),
                  tp = stats::rbinom(k, n1, se), tn = stats::rbinom(k, n0, sp))
  d$fn <- n1 - d$tp; d$fp <- n0 - d$tn

  r <- dm_run(d, method = "fixed", heterogeneity_analysis = TRUE,
              forest_plot = TRUE, show_analysis_summary = TRUE)
  bn <- notes_of(r$bivariateresults)

  expect_true(any(grepl("No prediction interval is available", bn, fixed = TRUE)))
  expect_match(notice_text(r), "Fixed-effect model", fixed = TRUE)
  expect_true(any(grepl("FIXED-effect", notes_of(r$heterogeneity), fixed = TRUE)))
  expect_false(any(grepl("use the prediction interval above", bn, fixed = TRUE)))

  # forest plot and table must show the same interval
  st <- r$forestplot$state
  tbl <- r$bivariateresults$asDF
  expect_equal(st$pooled_sens_ci * 100, c(tbl$ci_lower[1], tbl$ci_upper[1]), tolerance = 1e-6)
})


test_that("no heterogeneity is claimed when none was estimated", {
  # Was: the warning and the summary fired on prediction-interval WIDTH, which
  # is large at small k because of t on k-2 df. Three identical studies (Q = 0,
  # I2 = 0, tau2 = 0) were reported as "studies differ more than sampling error
  # explains".
  ident <- data.frame(study = paste0("S", 1:3), tp = 85, fp = 10, fn = 15, tn = 90)
  # mada warns "There are very few primary studies!" - that is the point here
  r <- suppressWarnings(dm_run(ident, show_analysis_summary = TRUE))
  summary_txt <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(r$summary$content, collapse = " ")))

  expect_false(grepl("Substantial between-study heterogeneity", notice_text(r), fixed = TRUE))
  expect_false(grepl("heterogeneity is substantial", summary_txt, fixed = TRUE))

  bn <- notes_of(r$bivariateresults)
  # the wide interval is still shown, with its width attributed correctly
  expect_true(any(grepl("most of the width comes from having only", bn, fixed = TRUE)))
  # and no correlation is printed off two variance components that are zero
  expect_true(any(grepl("not identifiable", bn, fixed = TRUE)))
  expect_false(any(grepl("suggests a threshold effect", bn, fixed = TRUE)))
})


test_that("genuine heterogeneity is still reported", {
  set.seed(4); k <- 12
  n1 <- sample(40:200, k, TRUE); n0 <- sample(60:300, k, TRUE)
  se <- stats::plogis(stats::rnorm(k, stats::qlogis(0.8), 1.2))
  sp <- stats::plogis(stats::rnorm(k, stats::qlogis(0.85), 1.2))
  d <- data.frame(study = paste0("S", seq_len(k)),
                  tp = stats::rbinom(k, n1, se), tn = stats::rbinom(k, n0, sp))
  d$fn <- n1 - d$tp; d$fp <- n0 - d$tn

  r <- dm_run(d, show_analysis_summary = TRUE)
  expect_match(notice_text(r), "Substantial between-study heterogeneity", fixed = TRUE)
  expect_true(any(grepl("threshold effect", notes_of(r$bivariateresults), fixed = TRUE)))
})


test_that("the HSROC model is refitted when mada's iteration limit is reached", {
  # Was: the "Reached maximum number of iterations!" warning was caught, the
  # model refitted with suppressWarnings at the same limit, and the last
  # iterate reported as a fit. Here l = 100 gives theta 0.056 / AUC 0.947 and
  # the converged fit gives theta 1.574 / AUC 0.389.
  skip_if_not_installed("mada")
  d <- data.frame(study = paste0("S", 1:6),
                  tp = c(45, 2, 60, 22, 80, 5), fp = c(10, 30, 30, 5, 2, 40),
                  fn = c(5, 30, 2, 6, 25, 20),  tn = c(90, 10, 60, 60, 150, 20))
  r <- dm_run(d, hsroc_analysis = TRUE)
  est <- r$hsrocresults$asDF

  md <- data.frame(TP = d$tp, FP = d$fp, FN = d$fn, TN = d$tn)
  ref <- suppressWarnings(mada::phm(md, correction = 0.5,
                                    correction.control = "single", l = 5000))
  expect_equal(est$estimate[1], unname(ref$coefficients[["theta"]]), tolerance = 1e-4)
  expect_true(any(grepl("more than mada's default 100 profile-likelihood iterations",
                        notes_of(r$hsrocresults), fixed = TRUE)))
})


test_that("the SROC curve stops at the observed false-positive range", {
  # Was: mada::sroc()'s default grid drew the curve from FPR 0.01 to 0.99
  # whatever the studies covered, so most of the line a reader picks an
  # operating point from was extrapolation.
  set.seed(11); k <- 10
  n1 <- sample(40:200, k, TRUE); n0 <- sample(60:300, k, TRUE)
  se <- stats::plogis(stats::rnorm(k, stats::qlogis(0.85), 0.5))
  sp <- stats::plogis(stats::rnorm(k, stats::qlogis(0.92), 0.5))
  d <- data.frame(study = paste0("S", seq_len(k)),
                  tp = stats::rbinom(k, n1, se), tn = stats::rbinom(k, n0, sp))
  d$fn <- n1 - d$tp; d$fp <- n0 - d$tn

  crv <- dm_run(d, sroc_plot = TRUE)$srocplot$state$sroc_curve
  obs <- d$fp / (d$fp + d$tn)

  expect_gt(nrow(crv), 10)
  expect_gte(min(crv$fpr), min(obs) - 1e-9)
  expect_lte(max(crv$fpr), max(obs) + 1e-9)
})
