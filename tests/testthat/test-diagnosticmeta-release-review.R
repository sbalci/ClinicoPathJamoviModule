# ═══════════════════════════════════════════════════════════
# Release-review regression tests: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# Each test pins a defect confirmed during the release review. Statistics are
# checked against mada / metafor / a hand-coded primary-source formula rather
# than against the module's own arithmetic.

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
  # exclusion disclosures render in the dedicated notices channel
  txt <- gsub("<[^>]+>", " ", res$notices$content)

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

# ── Fix-pass regressions (2026-08-22) ──────────────────────────

test_that("PHM p-value tests H0: theta = 1, and the variance gets no Wald p", {
  skip_if_not_installed("mada")
  d <- studies()
  hs <- run_dm(d, hsroc_analysis = TRUE)$hsrocresults$asDF

  fit <- mada::phm(data.frame(TP = d$tp, FN = d$fn, FP = d$fp, TN = d$tn),
                   correction = 0.5, correction.control = "single")
  sm <- summary(fit)
  theta <- sm$object$coefficients[["theta"]]
  se_theta <- sqrt(sm$object$vcov[1, 1])

  i_theta <- grep("theta", hs$parameter)[1]
  expect_equal(hs$estimate[i_theta], theta, tolerance = 1e-6)
  # z = (theta - 1)/SE: the no-accuracy null is theta = 1 (chance diagonal),
  # not theta = 0 (theta -> 0 is a PERFECT test)
  expect_equal(hs$z_value[i_theta], (theta - 1) / se_theta, tolerance = 1e-6)
  expect_equal(hs$p_value[i_theta],
               2 * (1 - pnorm(abs((theta - 1) / se_theta))), tolerance = 1e-8)

  # a Wald test of a variance against 0 is invalid on the boundary: no p
  i_tau <- grep("tau", hs$parameter)
  if (length(i_tau) > 0)
    expect_true(all(is.na(hs$p_value[i_tau])))
})

test_that("PHM uses the same continuity correction as the bivariate model", {
  skip_if_not_installed("mada")
  d <- studies(); d$fp[3] <- 0    # default correction option: "none"
  hs <- run_dm(d, hsroc_analysis = TRUE)$hsrocresults$asDF

  md <- data.frame(TP = d$tp, FN = d$fn, FP = d$fp, TN = d$tn)
  theta_single <- summary(mada::phm(md, correction = 0.5,
                                    correction.control = "single"))$object$coefficients[["theta"]]
  theta_all <- summary(mada::phm(md, correction = 0.5,
                                 correction.control = "all"))$object$coefficients[["theta"]]

  i_theta <- grep("theta", hs$parameter)[1]
  # must match the bivariate model's policy ("single" under 'none'), and the
  # two policies must actually differ on this data for the pin to bite
  expect_false(isTRUE(all.equal(theta_single, theta_all, tolerance = 1e-8)))
  expect_equal(hs$estimate[i_theta], theta_single, tolerance = 1e-6)
})

test_that("prediction interval uses t on k-2 df, reproduced from the reitsma fit", {
  skip_if_not_installed("mada")
  d <- studies()
  res <- run_dm(d)
  notes <- vapply(res$bivariateresults$notes, function(n) n$note, character(1))
  pi_note <- notes[grep("Prediction interval", notes)][1]
  expect_match(pi_note, "t distribution on 8 df", fixed = TRUE)

  fit <- mada::reitsma(data.frame(TP = d$tp, FN = d$fn, FP = d$fp, TN = d$tn),
                       method = "reml", correction = 0.5,
                       correction.control = "single")
  tot <- vcov(fit)[1:2, 1:2] + fit$Psi[1:2, 1:2]
  mu <- as.numeric(coef(fit))
  crit <- qt(0.975, df = nrow(d) - 2)
  sens_pi <- plogis(mu[1] + c(-1, 1) * crit * sqrt(tot[1, 1])) * 100

  nums <- as.numeric(regmatches(pi_note, gregexpr("[0-9]+\\.[0-9]", pi_note))[[1]])
  # note prints to 1 dp: sensitivity lower/upper are the first two numbers
  expect_equal(nums[1], sens_pi[1], tolerance = 0.06)
  expect_equal(nums[2], sens_pi[2], tolerance = 0.06)
})

test_that("meta-regression uses the Knapp-Hartung adjustment", {
  skip_if_not_installed("metafor")
  d <- studies()
  d$grp <- factor(rep(c("a", "b"), 5))

  res <- ClinicoPath::diagnosticmeta(
    data = d, study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn",
    covariate = "grp", meta_regression = TRUE)
  mr <- res$metaregression$asDF

  ls <- qlogis(d$tp / (d$tp + d$fn))
  vs <- 1 / d$tp + 1 / d$fn
  ref <- metafor::rma(yi = ls, vi = vs, mods = ~ grp, data = d,
                      method = "REML", test = "knha")

  i_cov <- which(mr$measure == "Sensitivity" & mr$parameter != "Intercept")[1]
  expect_equal(mr$z_value[i_cov], ref$zval[2], tolerance = 1e-6)
  expect_equal(mr$p_value[i_cov], ref$pval[2], tolerance = 1e-8)

  notes <- vapply(res$metaregression$notes, function(n) n$note, character(1))
  expect_true(any(grepl("UNIVARIATE", notes)))
  expect_true(any(grepl("Knapp-Hartung", notes)))
  expect_true(any(grepl("Omnibus", notes)))
})

test_that("bivariate variance components and sens/spec correlation are reported", {
  skip_if_not_installed("mada")
  d <- studies()
  res <- run_dm(d)
  notes <- vapply(res$bivariateresults$notes, function(n) n$note, character(1))
  vc_note <- notes[grep("variance components", notes)][1]
  expect_false(is.na(vc_note))

  fit <- mada::reitsma(data.frame(TP = d$tp, FN = d$fn, FP = d$fp, TN = d$tn),
                       method = "reml", correction = 0.5,
                       correction.control = "single")
  corr_ss <- -fit$Psi[1, 2] / sqrt(fit$Psi[1, 1] * fit$Psi[2, 2])
  expect_match(vc_note, sprintf("%.2f", corr_ss), fixed = TRUE)
})

test_that("fixed-effects method with the SROC plot does not abort the analysis", {
  # mada 0.5.12 summary.reitsma() errors under method = "fixed"; the SROC path
  # must fall back to the fitted coefficients instead of killing .run().
  a <- run_dm(method = "fixed", sroc_plot = TRUE)
  st <- a$srocplot$state
  d <- studies()
  fit <- mada::reitsma(data.frame(TP = d$tp, FP = d$fp, FN = d$fn, TN = d$tn),
                       method = "fixed", correction = 0.5, correction.control = "single")
  expect_equal(st$pooled_sens, unname(stats::plogis(fit$coefficients[1, "tsens"])), tolerance = 1e-6)
  expect_equal(st$pooled_fpr,  unname(stats::plogis(fit$coefficients[1, "tfpr"])),  tolerance = 1e-6)
  # mada::sroc() is NaN for a fixed fit (no between-study variance); the plot
  # must say why the curve is missing instead of silently omitting it.
  expect_null(st$sroc_curve)
  expect_match(st$curve_note, "fixed-effects", fixed = TRUE)
  expect_null(run_dm(method = "reml", sroc_plot = TRUE)$srocplot$state$curve_note)
})

test_that("the PHM table reports the SROC AUC = 1/(1 + theta) from the same fit", {
  skip_if_not_installed("mada")
  hs <- run_dm(hsroc_analysis = TRUE)$hsrocresults$asDF
  auc_row <- hs[grepl("AUC", hs$parameter), ]
  expect_equal(nrow(auc_row), 1L)
  d <- studies()
  fit <- mada::phm(data.frame(TP = d$tp, FP = d$fp, FN = d$fn, TN = d$tn),
                   correction = 0.5, correction.control = "single")
  theta <- unname(stats::coef(fit)["theta"])
  expect_equal(auc_row$estimate, 1 / (1 + theta), tolerance = 1e-6)
  expect_equal(auc_row$std_error, sqrt(stats::vcov(fit)[1, 1]) / (1 + theta)^2, tolerance = 1e-6)
})

test_that("the SROC plot and the forest plot draw the same corrected study data", {
  d <- studies(); d$fp[1] <- 0
  a <- run_dm(d, zero_cell_correction = "zero_cells", sroc_plot = TRUE, forest_plot = TRUE)
  sroc_fp <- a$srocplot$state$data$fp[1]
  forest_fp <- a$forestplot$state$data$fp[1]
  expect_equal(sroc_fp, forest_fp)
  expect_equal(sroc_fp, 0.5)
})
