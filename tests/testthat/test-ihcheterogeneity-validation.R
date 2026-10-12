test_that("C08, C09, C10, C11, C12, C13: Hand arithmetic matches sampling bias and Bland-Altman table", {
  # 6-case hand dataset
  d <- data.frame(
    ws = c(50, 60, 70, 80, 90, 100),
    b1 = c(52, 59, 73, 78, 95, 102),
    b2 = c(48, 62, 69, 81, 88, 103)
  )

  r <- ihcheterogeneity(data = d, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2")
  res <- r$samplingbiastable$asDF[1, ]

  # Mean difference = 1.5, SD of differences = 2.5884358
  expect_equal(res$mean_diff, 1.5, tolerance = 1e-10)
  expect_equal(res$ci_lower, -1.2163968, tolerance = 1e-6)
  expect_equal(res$ci_upper, 4.2163968, tolerance = 1e-6)
  expect_equal(res$p_value, 0.2149846, tolerance = 1e-6)
  expect_equal(res$effect_size, 0.4880005, tolerance = 1e-6) # Hedges g

  # Bland-Altman Limits of Agreement: 1.5 +/- 1.96 * 2.5884358 = [-3.573334, 6.573334]
  expect_equal(res$loa_lower, -3.573334, tolerance = 1e-5)
  expect_equal(res$loa_upper, 6.573334, tolerance = 1e-5)

  # LoA 95% Confidence Intervals via Bland-Altman SE
  expect_equal(res$loa_lower_lcl, -8.511621, tolerance = 1e-5)
  expect_equal(res$loa_lower_ucl, 1.364953, tolerance = 1e-5)
  expect_equal(res$loa_upper_lcl, 1.635047, tolerance = 1e-5)
  expect_equal(res$loa_upper_ucl, 11.511621, tolerance = 1e-5)
})

test_that("C16, C17, C18, C19: Closed-form two-way ANOVA variance components decomposition", {
  d <- data.frame(
    ws = c(10, 20, 30, 40, 50),
    b1 = c(12, 22, 32, 42, 52)
  )

  r <- ihcheterogeneity(data = d, wholesection = "ws", biopsy1 = "b1",
                        variance_components = TRUE)
  vt <- r$variancetable$asDF

  expect_equal(vt$variance[1], 250, tolerance = 1e-8)
  expect_equal(vt$variance[2], 0, tolerance = 1e-8)
  expect_equal(vt$variance[3], 2, tolerance = 1e-8)
  expect_equal(vt$variance[4], 252, tolerance = 1e-8)

  expect_equal(vt$percentage[1], 250 / 252 * 100, tolerance = 1e-8)
  expect_equal(vt$percentage[3], 2 / 252 * 100, tolerance = 1e-8)
  expect_equal(sum(vt$percentage[1:3]), 100, tolerance = 1e-8)
})

test_that("C20: Bonett (2002) sample size planning exact formula", {
  d <- data.frame(
    ws = c(50, 60, 70, 80, 90, 100),
    b1 = c(52, 59, 73, 78, 95, 102),
    b2 = c(48, 62, 69, 81, 88, 103)
  )
  r <- ihcheterogeneity(data = d, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2",
                        sample_size_planning = TRUE)
  st <- r$samplesizetable$asDF

  # At ICC ~ 0.9873, k = 3, alpha = 0.05:
  # width 0.20 requires n = 52
  # width 0.10 requires n = 202
  expect_equal(st$n_w20[1], 52)
  expect_equal(st$n_w10[1], 202)
})

test_that("C03, C04: Parity for ICC(2,1) and ICC(3,1) with psych and irr packages", {
  load(testthat::test_path("../../data/ihcheterogeneity_test.rda"))
  d <- ihcheterogeneity_test
  r <- ihcheterogeneity(data = d, wholesection = "wholesection",
                        biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3")
  rt <- r$reproducibilitytable$asDF

  Y <- d[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")]
  psych_res <- psych::ICC(Y, lmer = FALSE)$results

  # ICC(2,1)
  icc2_obs <- rt$value[rt$metric == "ICC(2,1) - absolute agreement"]
  icc2_exp <- psych_res$ICC[psych_res$type == "ICC2"]
  expect_equal(icc2_obs, icc2_exp, tolerance = 1e-7)

  # Check with irr package as secondary oracle
  irr_res <- irr::icc(Y, model = "twoway", type = "agreement", unit = "single")
  expect_equal(icc2_obs, irr_res$value, tolerance = 1e-7)

  # ICC(3,1)
  icc3_obs <- rt$value[rt$metric == "ICC(3,1) - consistency (bias-blind)"]
  icc3_exp <- psych_res$ICC[psych_res$type == "ICC3"]
  expect_equal(icc3_obs, icc3_exp, tolerance = 1e-7)
})

test_that("C14: Proportional bias test agrees with HC3 robust standard errors from sandwich package", {
  load(testthat::test_path("../../data/ihcheterogeneity_test.rda"))
  d <- ihcheterogeneity_test
  r <- ihcheterogeneity(data = d, wholesection = "wholesection",
                        biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3")
  st <- r$samplingbiastable$asDF

  fit <- lm(I(biopsy1 - wholesection) ~ wholesection, data = d)
  V_hc3 <- sandwich::vcovHC(fit, type = "HC3")
  se_hc3 <- sqrt(diag(V_hc3))[2]
  p_hc3 <- 2 * pt(-abs(coef(fit)[2] / se_hc3), df.residual(fit))

  expect_equal(st$slope[1], unname(coef(fit)[2]), tolerance = 1e-10)
  expect_equal(st$slope_p[1], unname(p_hc3), tolerance = 1e-8)
})

test_that("C23, C24, C25: Spatial compartment tests match stats::kruskal.test and ANOVA", {
  load(testthat::test_path("../../data/ihcheterogeneity_compartments.rda"))
  d <- ihcheterogeneity_compartments
  r <- ihcheterogeneity(data = d, wholesection = "wholesection",
                        biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3",
                        spatial_id = "spatial_id", compareCompartments = TRUE, compartmentTests = TRUE)
  ct <- r$compartmentTests$asDF

  m <- cbind(d$wholesection, d$biopsy1, d$biopsy2, d$biopsy3)
  cvs <- apply(m, 1, function(x) stats::sd(x) / mean(x) * 100)
  means <- rowMeans(m)

  # Kruskal-Wallis on per-case CV
  kw_cv <- stats::kruskal.test(cvs ~ factor(d$spatial_id))
  expect_equal(ct$statistic[1], unname(kw_cv$statistic), tolerance = 1e-8)
  expect_equal(ct$p_value[1], unname(kw_cv$p.value), tolerance = 1e-8)

  # Brown-Forsythe spread test on per-case CV
  meds <- tapply(cvs, d$spatial_id, stats::median)
  abs_dev <- abs(cvs - meds[d$spatial_id])
  bf <- summary(stats::aov(abs_dev ~ factor(d$spatial_id)))[[1]]
  expect_equal(ct$statistic[2], bf[["F value"]][1], tolerance = 1e-8)
  expect_equal(ct$p_value[2], bf[["Pr(>F)"]][1], tolerance = 1e-8)

  # Kruskal-Wallis on per-case means
  kw_means <- stats::kruskal.test(means ~ factor(d$spatial_id))
  expect_equal(ct$statistic[3], unname(kw_means$statistic), tolerance = 1e-8)
  expect_equal(ct$p_value[3], unname(kw_means$p.value), tolerance = 1e-8)
})

test_that("Battery C: Metamorphic transformations hold mathematically", {
  load(testthat::test_path("../../data/ihcheterogeneity_test.rda"))
  d <- ihcheterogeneity_test
  r_orig <- ihcheterogeneity(data = d, wholesection = "wholesection",
                             biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3")
  orig_diff <- r_orig$samplingbiastable$asDF$mean_diff[1]
  orig_icc2 <- r_orig$reproducibilitytable$asDF$value[r_orig$reproducibilitytable$asDF$metric == "ICC(2,1) - absolute agreement"]

  # C1: Row permutation invariance
  set.seed(42)
  d_perm <- d[sample(nrow(d)), ]
  r_perm <- ihcheterogeneity(data = d_perm, wholesection = "wholesection",
                             biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3")
  perm_diff <- r_perm$samplingbiastable$asDF$mean_diff[1]
  perm_icc2 <- r_perm$reproducibilitytable$asDF$value[r_perm$reproducibilitytable$asDF$metric == "ICC(2,1) - absolute agreement"]
  expect_equal(perm_diff, orig_diff, tolerance = 1e-12)
  expect_equal(perm_icc2, orig_icc2, tolerance = 1e-12)

  # C2: Location shift invariance
  d_shift <- d
  d_shift[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")] <- d_shift[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")] + 10
  r_shift <- ihcheterogeneity(data = d_shift, wholesection = "wholesection",
                              biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3")
  expect_equal(r_shift$samplingbiastable$asDF$mean_diff[1], orig_diff, tolerance = 1e-12)
  expect_equal(r_shift$samplingbiastable$asDF$loa_lower[1], r_orig$samplingbiastable$asDF$loa_lower[1], tolerance = 1e-12)

  # C3: Scale equivariance (Y * 2)
  d_scale <- d
  d_scale[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")] <- d_scale[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")] * 2
  r_scale <- ihcheterogeneity(data = d_scale, wholesection = "wholesection",
                              biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3")
  expect_equal(r_scale$samplingbiastable$asDF$mean_diff[1], 2 * orig_diff, tolerance = 1e-8)
  scale_icc2 <- r_scale$reproducibilitytable$asDF$value[r_scale$reproducibilitytable$asDF$metric == "ICC(2,1) - absolute agreement"]
  expect_equal(scale_icc2, orig_icc2, tolerance = 1e-8)

  # C4: Sample duplication exact Bessel correction
  d_dup <- rbind(d, d)
  r_dup <- ihcheterogeneity(data = d_dup, wholesection = "wholesection",
                            biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3")
  se_orig <- (r_orig$samplingbiastable$asDF$ci_upper[1] - r_orig$samplingbiastable$asDF$ci_lower[1]) / (2 * qt(0.975, nrow(d) - 1))
  se_dup  <- (r_dup$samplingbiastable$asDF$ci_upper[1] - r_dup$samplingbiastable$asDF$ci_lower[1]) / (2 * qt(0.975, nrow(d_dup) - 1))
  expect_equal(se_dup, se_orig * sqrt((nrow(d) - 1) / (2 * nrow(d) - 1)), tolerance = 1e-6)
})

test_that("Battery D: Boundary condition handling and explicit refusal", {
  load(testthat::test_path("../../data/ihcheterogeneity_test.rda"))
  d <- ihcheterogeneity_test

  # D1: Explicit refusal when n < 5
  expect_error(ihcheterogeneity(data = d[1:4, ], wholesection = "wholesection", biopsy1 = "biopsy1"),
               regexp = "(?i)at least 5 complete cases")

  # D2: Exact minimum boundary n = 5 executes
  r5 <- ihcheterogeneity(data = d[1:5, ], wholesection = "wholesection", biopsy1 = "biopsy1")
  expect_true(is.finite(r5$samplingbiastable$asDF$mean_diff[1]))

  # D3: Constant reference variable generates notice and does not crash
  d_const <- d
  d_const$wholesection <- 50
  r_const <- ihcheterogeneity(data = d_const, wholesection = "wholesection", biopsy1 = "biopsy1")
  expect_true(is.na(r_const$reproducibilitytable$asDF$value[1]))
  expect_true(grepl("constant", tolower(r_const$notices$content)))
})

test_that("Battery E & G: Option semantics and cross-output mathematical consistency", {
  d <- data.frame(
    ws = c(50, 60, 70, 80, 90, 100),
    b1 = c(52, 59, 73, 78, 95, 102),
    b2 = c(48, 62, 69, 81, 88, 103)
  )

  # E2: bias_margin sensitivity
  r_m1  <- ihcheterogeneity(data = d, wholesection = "ws", biopsy1 = "b1", bias_margin = 1.0)
  r_m10 <- ihcheterogeneity(data = d, wholesection = "ws", biopsy1 = "b1", bias_margin = 10.0)
  expect_false(grepl("ruled out", r_m1$samplingbiastable$asDF$clinical_impact[1]))
  expect_true(grepl("ruled out", r_m10$samplingbiastable$asDF$clinical_impact[1]))

  # E3: cv_threshold sensitivity
  r_cv5  <- ihcheterogeneity(data = d, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2", cv_threshold = 5.0)
  r_cv50 <- ihcheterogeneity(data = d, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2", cv_threshold = 50.0)
  interp_cv5  <- r_cv5$reproducibilitytable$asDF$interpretation[grepl("Within-case CV", r_cv5$reproducibilitytable$asDF$metric)]
  interp_cv50 <- r_cv50$reproducibilitytable$asDF$interpretation[grepl("Within-case CV", r_cv50$reproducibilitytable$asDF$metric)]
  expect_true(grepl("Moderate", interp_cv5))
  expect_true(grepl("Low", interp_cv50))

  # G: Ordering consistency
  tb <- r_m1$samplingbiastable$asDF[1, ]
  expect_true(tb$loa_upper > tb$mean_diff && tb$mean_diff > tb$loa_lower)
  expect_true(tb$ci_upper > tb$mean_diff && tb$mean_diff > tb$ci_lower)
})
