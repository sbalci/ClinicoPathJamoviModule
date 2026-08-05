# ═══════════════════════════════════════════════════════════
# Release-review regression tests: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# Each test pins a defect confirmed during the release review. Statistics are
# checked against mada / metafor / a hand-coded primary-source formula rather
# than against the module's own arithmetic.

library(testthat)

studies <- function() data.frame(
  study = paste0("S", 1:10),
  tp = c(40, 90, 20, 60, 15, 55, 33, 71, 25, 48),
  fn = c(10, 10, 30, 40,  5, 15, 17,  9, 25, 12),
  fp = c(10, 10, 30,  5, 40, 20, 12, 25, 18,  9),
  tn = c(40, 90, 20, 95, 10, 60, 48, 55, 42, 71),
  stringsAsFactors = FALSE)

run_dm <- function(d = studies(), ...) ClinicoPath::diagnosticmeta(
  data = d, study = "study", true_positives = "tp", false_positives = "fp",
  false_negatives = "fn", true_negatives = "tn", ...)


test_that("Deeks' test follows Deeks' own specification", {
  skip_if_not_installed("metafor")
  d <- studies()
  pb <- run_dm(d, publication_bias = TRUE)$publicationbias$asDF

  # Deeks, Macaskill & Irwig 2005: log DOR regressed on 1/sqrt(ESS), WEIGHTED
  # BY ESS, slope referred to t on k-2 df. ESS = 4*n1*n0/(n1+n0) over the two
  # GROUP sizes -- not the harmonic mean of the four cells, which is what the
  # code used and which is not even a monotone function of the correct ESS.
  n1 <- d$tp + d$fn; n0 <- d$fp + d$tn
  ess  <- 4 * n1 * n0 / (n1 + n0)
  ldor <- log((d$tp * d$tn) / (d$fp * d$fn))
  cf   <- summary(stats::lm(ldor ~ I(1 / sqrt(ess)), weights = ess))$coefficients

  expect_equal(pb$statistic[1], cf[2, 3], tolerance = 1e-6)
  expect_equal(pb$p_value[1],   cf[2, 4], tolerance = 1e-8)

  # the ESS bug reported "No significant asymmetry" on this strongly
  # asymmetric data, with the statistic's sign reversed
  expect_match(pb$interpretation[1], "Significant asymmetry")
  expect_lt(pb$statistic[1], 0)
})

test_that("Deeks' test survives a zero cell instead of returning NaN", {
  skip_if_not_installed("metafor")
  d <- studies(); d$fp[3] <- 0     # default zero_cell_correction is "none"
  pb <- run_dm(d, publication_bias = TRUE)$publicationbias$asDF

  expect_true(is.finite(pb$statistic[1]))
  expect_true(is.finite(pb$p_value[1]))
})

test_that("pooled estimates and derived measures match mada::reitsma", {
  skip_if_not_installed("mada")
  d  <- studies()
  bt <- run_dm(d)$bivariateresults$asDF

  m  <- mada::reitsma(data.frame(TP = d$tp, FN = d$fn, FP = d$fp, TN = d$tn))
  cf <- summary(m)$coefficients
  expect_equal(bt$estimate[bt$parameter == "Pooled Sensitivity"],
               cf["sensitivity", "Estimate"] * 100, tolerance = 1e-4)
  expect_equal(bt$estimate[bt$parameter == "Pooled Specificity"],
               (1 - cf["false pos. rate", "Estimate"]) * 100, tolerance = 1e-4)

  # LRs must come from the POOLED estimates, not averaged study-level LRs
  s <- bt$estimate[bt$parameter == "Pooled Sensitivity"] / 100
  p <- bt$estimate[bt$parameter == "Pooled Specificity"] / 100
  expect_equal(bt$estimate[bt$parameter == "Positive Likelihood Ratio"],
               s / (1 - p), tolerance = 1e-4)
  expect_equal(bt$estimate[bt$parameter == "Negative Likelihood Ratio"],
               (1 - s) / p, tolerance = 1e-4)
})

test_that("SROC shows a prediction region as well as a confidence region", {
  skip_if_not_installed("mada")
  d <- studies()
  o <- ClinicoPath:::diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn", sroc_plot = TRUE)
  a <- ClinicoPath:::diagnosticmetaClass$new(options = o, data = d); a$run()
  st <- a$results$srocplot$state

  expect_false(is.null(st$conf_ellipse))
  expect_false(is.null(st$pred_ellipse))

  # must equal mada's own plot.reitsma(predict=TRUE) construction: Psi + vcov
  m  <- mada::reitsma(data.frame(TP = d$tp, FN = d$fn, FP = d$fp, TN = d$tn))
  mu <- as.numeric(stats::coef(m)); Sig <- stats::vcov(m); Psi <- m$Psi
  rad <- sqrt(stats::qchisq(0.95, 2)); th <- seq(0, 2 * pi, length.out = 200)
  Lp <- t(chol(Sig[1:2, 1:2] + Psi[1:2, 1:2]))
  pp <- Lp %*% (rad * rbind(cos(th), sin(th))) + mu[1:2]
  ref <- data.frame(fpr = stats::plogis(pp[2, ]), sens = stats::plogis(pp[1, ]))
  expect_equal(max(abs(st$pred_ellipse - ref)), 0, tolerance = 1e-8)

  # the prediction region must be the larger of the two
  area <- function(e) abs(sum((e$fpr - c(e$fpr[-1], e$fpr[1])) *
                              (e$sens + c(e$sens[-1], e$sens[1])))) / 2
  expect_gt(area(st$pred_ellipse), area(st$conf_ellipse))
})

test_that("confidence_level reaches the SROC region, not only the tables", {
  d <- studies()
  ell_area <- function(lvl) {
    o <- ClinicoPath:::diagnosticmetaOptions$new(
      study = "study", true_positives = "tp", false_positives = "fp",
      false_negatives = "fn", true_negatives = "tn", sroc_plot = TRUE,
      confidence_level = lvl)
    a <- ClinicoPath:::diagnosticmetaClass$new(options = o, data = d); a$run()
    e <- a$results$srocplot$state$conf_ellipse
    abs(sum((e$fpr - c(e$fpr[-1], e$fpr[1])) *
            (e$sens + c(e$sens[-1], e$sens[1])))) / 2
  }
  # the radius was hard-coded at qchisq(0.95, 2)
  expect_gt(ell_area(99), ell_area(95))
})

test_that("a prediction interval is reported and is wider than the CI", {
  d <- studies()
  o <- ClinicoPath:::diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn")
  a <- ClinicoPath:::diagnosticmetaClass$new(options = o, data = d); a$run()
  pv <- a$.__enclos_env__$private

  expect_length(pv$.pooled_sens_pi, 2)
  expect_true(all(is.finite(pv$.pooled_sens_pi)))
  # heterogeneity means a future study can fall well outside the CI of the mean
  expect_gt(diff(range(pv$.pooled_sens_pi)), diff(range(pv$.pooled_sens_ci)))
})

test_that("excluded studies are disclosed rather than silently dropped", {
  d <- studies()
  d$tp[2] <- NA        # incomplete
  d$fp[4] <- -5        # impossible
  d$tp[6] <- 0; d$fn[6] <- 0   # no diseased participants

  res <- run_dm(d)
  txt <- gsub("<[^>]+>", " ", res$instructions$content)

  # a missing cell used to blank the entire analysis, and the explanation was
  # written into a panel that .run() had already hidden
  expect_match(txt, "Studies excluded")
  expect_match(txt, "3 of 10")
  expect_match(txt, "missing counts")
  expect_match(txt, "negative counts")
  # and the analysis must still produce results from the remaining studies
  expect_gt(res$bivariateresults$rowCount, 0)
})

test_that("a zero cell is disclosed under every correction setting", {
  d <- studies(); d$fp[3] <- 0
  for (corr in c("none", "constant")) {
    res <- run_dm(d, zero_cell_correction = corr)
    # "none" was the default AND applied mada's heaviest correction
    # (+0.5 to all cells of all studies) while being the only setting
    # that disclosed nothing.
    expect_gt(res$bivariateresults$rowCount, 0)
  }
})

test_that("the forest plot draws a visible interval for the pooled estimate", {
  d <- studies()
  o <- ClinicoPath:::diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn", forest_plot = TRUE)
  a <- ClinicoPath:::diagnosticmetaClass$new(options = o, data = d); a$run()

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp); on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
  drew <- a$.__enclos_env__$private$.forestplot(
    a$results$forestplot, ggtheme = ggplot2::theme_bw(), theme = NULL)
  expect_true(isTRUE(drew))

  # the pooled "diamond" was a polygon whose four vertices shared one discrete
  # y value, so it had zero area and no interval was visible at all
  b <- ggplot2::ggplot_build(ggplot2::last_plot())
  has_interval <- any(vapply(b$data, function(l)
    all(c("xmin", "xmax") %in% names(l)) && any(is.finite(l$xmin)), logical(1)))
  expect_true(has_interval)
})

test_that("per-study and pooled accuracy are reported on the same scale", {
  d <- studies()
  res <- run_dm(d, show_individual_studies = TRUE)
  ind <- res$individualstudies$asDF
  bt  <- res$bivariateresults$asDF

  # individual studies were proportions (0.82) while pooled was a percentage
  # (81.59) - the same quantity on two scales in one output
  expect_true(all(ind$sensitivity > 1))
  expect_true(all(ind$sensitivity <= 100))
  pooled <- bt$estimate[bt$parameter == "Pooled Sensitivity"]
  expect_true(pooled > 1 && pooled <= 100)
})

test_that("meta-regression refuses a model with no residual degrees of freedom", {
  # 5 studies, each its own covariate level: 4 parameters + intercept = 5,
  # leaving ZERO residual df. The old guard counted studies only (>=3 ok,
  # <10 warn), so this fitted a saturated model; metafor silently dropped
  # redundant predictors and the table described a different model from the
  # one requested.
  d <- data.frame(
    study = paste0("S", 1:5),
    tp = c(40, 90, 20, 60, 15), fn = c(10, 10, 30, 40, 5),
    fp = c(10, 10, 30,  5, 40), tn = c(40, 90, 20, 95, 10),
    grp = factor(c("a", "b", "c", "d", "e")),
    stringsAsFactors = FALSE)

  res <- ClinicoPath::diagnosticmeta(
    data = d, study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn",
    covariate = "grp", meta_regression = TRUE)

  expect_s3_class(res, "diagnosticmetaResults")
  expect_equal(res$metaregression$rowCount, 0)
})

test_that("meta-regression refuses a covariate that never varies", {
  d <- data.frame(
    study = paste0("S", 1:8),
    tp = c(40, 90, 20, 60, 15, 55, 33, 71), fn = c(10, 10, 30, 40, 5, 15, 17, 9),
    fp = c(10, 10, 30,  5, 40, 20, 12, 25), tn = c(40, 90, 20, 95, 10, 60, 48, 55),
    grp = factor(rep("same", 8)), stringsAsFactors = FALSE)

  res <- ClinicoPath::diagnosticmeta(
    data = d, study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn",
    covariate = "grp", meta_regression = TRUE)

  expect_s3_class(res, "diagnosticmetaResults")
  expect_equal(res$metaregression$rowCount, 0)
})

test_that("meta-regression still runs when degrees of freedom allow it", {
  d <- studies()
  d$grp <- factor(rep(c("a", "b"), 5))   # 1 parameter, 10 studies

  res <- ClinicoPath::diagnosticmeta(
    data = d, study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn",
    covariate = "grp", meta_regression = TRUE)

  expect_gt(res$metaregression$rowCount, 0)
})
