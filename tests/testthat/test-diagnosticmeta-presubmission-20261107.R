# Regression tests for the diagnosticmeta pre-submission review of 2026-11-07.
# The estimators matched mada/metafor and are unchanged; these are fixes to
# sentences that were false at extreme or non-default inputs. Each test fails
# on the backend as it stood before the fix.

ps_run <- function(d, ...) {
  a <- diagnosticmetaClass$new(options = diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn", ...), data = d)
  a$init(noThrow = TRUE)
  if (!a$errored) a$run(noThrow = TRUE)
  a
}
ps_text <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))
ps_notes <- function(tbl) vapply(tbl$notes, function(x) paste(x$note, collapse = " "), "")

# Large series with 0-1 false negatives: pooled sensitivity 99.979%.
ps_near_perfect <- function() data.frame(
  study = paste0("S", 1:10),
  tp = c(2400, 3100, 2800, 4100, 2200, 3600, 2900, 3300, 2500, 3900),
  fn = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
  fp = c(40, 55, 61, 38, 70, 45, 52, 66, 48, 59),
  tn = c(900, 1100, 950, 1200, 1000, 870, 1300, 1050, 990, 1150),
  stringsAsFactors = FALSE)

# The 20 studies of the bundled diagnosticmeta_test_categorical example.
ps_twenty <- function() data.frame(
  study = paste0("Study_", 1:20),
  tp = c(81, 56, 84, 23, 36, 44, 19, 75, 65, 58, 88, 54, 22, 79, 49, 74, 51, 84, 66, 52),
  fp = c(10, 6, 4, 13, 32, 28, 3, 6, 18, 14, 12, 30, 11, 17, 23, 11, 10, 2, 5, 7),
  fn = c(30, 6, 11, 5, 20, 10, 1, 6, 21, 11, 15, 4, 17, 14, 7, 49, 16, 16, 24, 4),
  tn = c(153, 120, 100, 98, 95, 76, 27, 105, 67, 93, 111, 126, 60, 135, 90, 131, 127, 127, 53, 89),
  stringsAsFactors = FALSE)


test_that("near-100% sensitivity is not printed as 100.0% with zero misses", {
  # Was: "Sensitivity: 100.0% - ... 1000 out of 1,000", "missing only 0" and
  # "LR- = 0.00" beside a table showing 99.979 and data with 2 false negatives.
  a <- ps_run(ps_near_perfect(), show_analysis_summary = TRUE)
  expect_false(a$errored)
  est <- a$results$bivariateresults$asDF$estimate[1]
  expect_true(est > 99.95 && est < 100)
  html <- a$results$summary$content
  txt <- ps_text(html)
  expect_true(nzchar(txt))
  expect_no_match(txt, "100.0%", fixed = TRUE)
  expect_no_match(txt, "missing only 0.", fixed = TRUE)
  expect_no_match(txt, "1000 out of 1,000", fixed = TRUE)
  expect_no_match(txt, "LR- = 0.00,", fixed = TRUE)
  expect_match(txt, "99.98%", fixed = TRUE)
  expect_match(txt, "fewer than 1 in 1,000 patients with disease", fixed = TRUE)
  # the copy-ready text carries the same digits
  expect_match(html, "pooled sensitivity of 99.98%", fixed = TRUE)
})

test_that("near-100% specificity is not printed as zero false alarms", {
  d <- ps_near_perfect()
  names(d)[2:5] <- c("tn", "fp", "fn", "tp")   # swap the two margins
  a <- ps_run(d, show_analysis_summary = TRUE)
  txt <- ps_text(a$results$summary$content)
  expect_true(nzchar(txt))
  expect_no_match(txt, "only 0 out of 1,000", fixed = TRUE)
  expect_no_match(txt, "100.0%", fixed = TRUE)
  expect_match(txt, "Fewer than 1 in 1,000 healthy individuals test positive", fixed = TRUE)
  expect_match(txt, "fewer than 1 in 1,000 healthy individuals will test positive", fixed = TRUE)
})

test_that("percentages keep the digits that decide their band", {
  a <- ps_run(ps_twenty())
  priv <- a$.__enclos_env__$private
  expect_identical(priv$.fmtPct(81.6715), "81.7")
  expect_identical(priv$.fmtPct(99.97894), "99.98")
  expect_identical(priv$.fmtPct(89.96), "89.96")
  expect_identical(priv$.fmtRatio(0.2055), "0.21")
  expect_identical(priv$.fmtRatio(0.00021), "0.00021")
  # Was: rounded to 90.0 first and called "excellent".
  txt <- priv$.getInterpretationText(89.96, 85, 6, 0.12)
  expect_match(txt, "sensitivity of 89.96%", fixed = TRUE)
  expect_match(txt, "<em>good</em> band", fixed = TRUE)
  expect_no_match(txt, "sensitivity of 89.96%</strong> falls in the <em>excellent</em>", fixed = TRUE)
})

test_that("zero-FP notice claims no unbounded LR+ when some studies have false positives", {
  d <- ps_twenty(); d$fp <- 0; d$fp[c(1, 5, 9, 13)] <- 3
  txt <- ps_text(ps_run(d)$results$notices$content)
  expect_match(txt, "Specificity set by the continuity correction", fixed = TRUE)
  expect_no_match(txt, "no upper limit", fixed = TRUE)
  # with no false positive anywhere the clause is true and stays
  d$fp <- 0
  txt0 <- ps_text(ps_run(d)$results$notices$content)
  expect_match(txt0, "LR+ would have no upper limit the data can set", fixed = TRUE)
})

test_that("an ordered covariate is labelled with the user's variable name", {
  d <- ps_twenty()
  d$q <- factor(rep(c("low", "mid", "high", "mid"), 5),
                levels = c("low", "mid", "high"), ordered = TRUE)
  a <- ps_run(d, meta_regression = TRUE, covariate = "q")
  params <- a$results$metaregression$asDF$parameter
  expect_true(all(c("q.L", "q.Q") %in% params))
  scale_note <- paste(ps_notes(a$results$metaregression), collapse = " ")
  expect_match(scale_note, "q.L: low=", fixed = TRUE)
  expect_no_match(scale_note, "covariate.L", fixed = TRUE)
  expect_no_match(scale_note, "e-1[0-9]")
})

test_that("SROC help no longer says the prediction region does not shrink", {
  a <- ps_run(ps_twenty(), sroc_plot = TRUE, show_plot_explanations = TRUE)
  txt <- ps_text(a$results$srocplot_explanation$content)
  expect_true(nzchar(txt))
  expect_no_match(txt, "does NOT shrink", fixed = TRUE)
  expect_match(txt, "never to a single point", fixed = TRUE)
})

test_that("predictive values carry a heterogeneity note only when heterogeneity is substantial", {
  a <- ps_run(ps_twenty(), show_interpretation = TRUE)
  expect_true(isTRUE(a$.__enclos_env__$private$.heterogeneity_substantial))
  expect_match(paste(ps_notes(a$results$predictiveValues), collapse = " "),
               "predictive values in any one setting can differ markedly", fixed = TRUE)
  # homogeneous studies: no such note
  d <- data.frame(study = paste0("H", 1:8), tp = rep(c(80, 160), 4), fn = rep(c(20, 40), 4),
                  fp = rep(c(10, 20), 4), tn = rep(c(90, 180), 4))
  b <- ps_run(d, show_interpretation = TRUE)
  expect_false(isTRUE(b$.__enclos_env__$private$.heterogeneity_substantial))
  b_notes <- paste(ps_notes(b$results$predictiveValues), collapse = " ")
  expect_no_match(b_notes, "differ markedly", fixed = TRUE)
  # a fit of exactly 90% is "90.0", not "90.0000"
  expect_match(b_notes, "specificity (90.0%)", fixed = TRUE)
})

test_that("IEEE-754 90.0% boundary is classified as excellent and LR note is not duplicated", {
  # (1 - plogis(qlogis(0.10))) * 100 evaluates to 89.999999999999986 in IEEE-754
  d <- data.frame(study = paste0("H", 1:8), tp = rep(c(80, 160), 4), fn = rep(c(20, 40), 4),
                  fp = rep(c(10, 20), 4), tn = rep(c(90, 180), 4))
  b <- ps_run(d, show_analysis_summary = TRUE)
  interp <- ps_text(b$results$summary$content)
  expect_match(interp, "Your pooled specificity of 90.0% (88.2%-91.6%) falls in the excellent band", fixed = TRUE)
  expect_no_match(interp, "Your pooled specificity of 90.0% (88.2%-91.6%) falls in the good band", fixed = TRUE)
  # When sens >= 90 or spec >= 90 already emits the LR note, the LR paragraph does not repeat it
  d_high <- data.frame(study = paste0("K", 1:8), tp = rep(92, 8), fn = rep(8, 8),
                       fp = rep(5, 8), tn = rep(95, 8))
  b_high <- ps_run(d_high, show_analysis_summary = TRUE)
  raw_html <- b_high$results$summary$content
  n_occ <- length(gregexpr("even if sensitivity or specificity is high", raw_html, fixed = TRUE)[[1]])
  expect_equal(n_occ, 1L)
})

test_that("fmtRatio preserves digits when 2-decimal rounding crosses an LR threshold", {
  a <- ps_run(ps_twenty())
  priv <- a$.__enclos_env__$private
  expect_identical(priv$.fmtRatio(0.1973), "0.197")
  expect_identical(priv$.fmtRatio(9.996), "9.996")
  expect_identical(priv$.fmtRatio(1.996), "1.996")
  expect_identical(priv$.fmtRatio(0.214), "0.21")
})

test_that("SROC curve is withheld when between-study variance collapses or FPR has zero range", {
  # Identical proportions across studies -> zero heterogeneity -> degenerate vertical SROC
  d_hom <- data.frame(study = paste0("H", 1:8), tp = rep(80, 8), fn = rep(20, 8),
                      fp = rep(10, 8), tn = rep(90, 8))
  a_hom <- ps_run(d_hom, sroc_plot = TRUE)
  expect_null(a_hom$results$srocplot$state$sroc_curve)
})

test_that("meta-regression treats Inf covariate values as missing and matches factor contrast spacing", {
  d <- ps_twenty()
  d$cov_inf <- seq_len(nrow(d))
  d$cov_inf[1] <- Inf
  a_inf <- ps_run(d, meta_regression = TRUE, covariate = "cov_inf")
  expect_false(a_inf$errored)
  expect_gt(a_inf$results$metaregression$rowCount, 0)

  d$grp <- factor(rep(c("Reference", "Assay B"), 10), levels = c("Reference", "Assay B"))
  stats::contrasts(d$grp) <- stats::contr.sum(2)
  a_fac <- ps_run(d, meta_regression = TRUE, covariate = "grp")
  params <- a_fac$results$metaregression$asDF$parameter
  scale_note <- paste(ps_notes(a_fac$results$metaregression), collapse = " ")
  expect_true("grp 1" %in% params)
  expect_match(scale_note, "grp 1: Reference=1, Assay B=-1", fixed = TRUE)
})
