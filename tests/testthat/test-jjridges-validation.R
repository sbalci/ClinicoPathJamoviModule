# Validation regression tests for ClinicoPath::jjridges
# Source report: development-ideas/jjridges-validation-2026-11-24.md
# Validation script: development-scripts/validate_jjridges.R
# Live tests lock in verified behaviour; skip-guarded tests encode confirmed open defects.

quiet_run <- function(expr) suppressWarnings(suppressMessages(force(expr)))

row_by_key <- function(df, col, val) {
  idx <- which(as.character(df[[col]]) == as.character(val))
  testthat::expect_length(idx, 1L)
  df[idx, , drop = FALSE]
}

make_tiny_fixture <- function() {
  # Low (n=5 + 1 NA): 2, 4, 4, 6, 9 -> mean=5, var=7, median=4, Q1=4, Q3=6
  # High (n=6): 8, 10, 12, 14, 16, 18 -> mean=13, var=14, median=13, Q1=10.5, Q3=15.5
  data.frame(
    val = c(2, 4, 4, 6, 9, NA, 8, 10, 12, 14, 16, 18),
    grp = factor(c(rep("Low", 6), rep("High", 6)), levels = c("Low", "High"))
  )
}

# ==============================================================================
# LIVE REGRESSION TESTS (PASSING CLAIMS LOCKED IN)
# ==============================================================================

test_that("C01-C06: statistics table matches exact hand calculations on tiny fixture with NA and tie", {
  # Oracle: exact hand arithmetic on Low=c(2,4,4,6,9) and High=c(8,10,12,14,16,18)
  d <- make_tiny_fixture()
  res <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp"))
  st <- res$statistics$asDF

  lo <- row_by_key(st, "group", "Low")
  hi <- row_by_key(st, "group", "High")

  expect_equal(lo$n, 5L)
  expect_equal(lo$mean, 5.0, tolerance = 1e-12)
  expect_equal(lo$sd, sqrt(7), tolerance = 1e-12)
  expect_equal(lo$median, 4.0, tolerance = 1e-12)
  expect_equal(lo$q1, 4.0, tolerance = 1e-12)
  expect_equal(lo$q3, 6.0, tolerance = 1e-12)
  expect_equal(lo$min, 2.0, tolerance = 1e-12)
  expect_equal(lo$max, 9.0, tolerance = 1e-12)

  expect_equal(hi$n, 6L)
  expect_equal(hi$mean, 13.0, tolerance = 1e-12)
  expect_equal(hi$sd, sqrt(14), tolerance = 1e-12)
  expect_equal(hi$median, 13.0, tolerance = 1e-12)
  expect_equal(hi$q1, 10.5, tolerance = 1e-12)
  expect_equal(hi$q3, 15.5, tolerance = 1e-12)
  expect_equal(hi$min, 8.0, tolerance = 1e-12)
  expect_equal(hi$max, 18.0, tolerance = 1e-12)
})

test_that("C08: parametric Welch t-test matches closed-form Welch-Satterthwaite formula", {
  # Oracle: closed-form Welch t = (5-13)/sqrt(7/5 + 14/6), Welch-Satterthwaite df
  d <- make_tiny_fixture()
  res <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp",
    show_stats = TRUE, test_type = "parametric", effsize_type = "d"
  ))
  tb <- res$tests$asDF
  v1 <- 7 / 5; v2 <- 14 / 6
  t_exp <- (5 - 13) / sqrt(v1 + v2)
  df_exp <- (v1 + v2)^2 / (v1^2 / 4 + v2^2 / 5)
  p_exp <- 2 * stats::pt(-abs(t_exp), df = df_exp)

  expect_equal(tb$method[1], "t-test")
  expect_equal(tb$statistic[1], t_exp, tolerance = 1e-10)
  expect_equal(tb$p_value[1], p_exp, tolerance = 1e-10)
})

test_that("C09: nonparametric Wilcoxon matches exact permutation and coin::wilcox_test", {
  # Oracle: exact permutation distribution on untied (1,3,5,7,9) vs (8,10,12,14,16) and coin::wilcox_test
  skip_if_not_installed("coin")
  d <- data.frame(
    val = c(1, 3, 5, 7, 9, 8, 10, 12, 14, 16),
    grp = factor(rep(c("A", "B"), each = 5), levels = c("A", "B"))
  )
  res <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp",
    show_stats = TRUE, test_type = "nonparametric", effsize_type = "hodges_lehmann"
  ))
  cw <- coin::wilcox_test(val ~ grp, data = d, distribution = "exact", conf.int = TRUE)

  expect_equal(res$tests$asDF$statistic[1], 1.0, tolerance = 1e-12)
  expect_equal(res$tests$asDF$p_value[1], 4 / choose(10, 5), tolerance = 1e-12)
  expect_equal(res$tests$asDF$p_value[1], as.numeric(coin::pvalue(cw)), tolerance = 1e-12)
  ci_cw <- as.numeric(coin::confint(cw)$conf.int)
  expect_equal(c(res$tests$asDF$ci_lower[1], res$tests$asDF$ci_upper[1]), ci_cw, tolerance = 1e-6)
})

test_that("C10: Yuen robust p-value and |t| match hand Yuen (1974) formula", {
  # Oracle: Yuen (1974) Biometrika 61(1):165-170 20% trimmed-means formula
  skip_if_not_installed("WRS2")
  d <- make_tiny_fixture()
  res <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp",
    show_stats = TRUE, test_type = "robust"
  ))
  win <- function(v) {
    n <- length(v); g <- floor(0.2 * n); vs <- sort(v); h <- n - 2 * g
    vw <- vs; vw[1:g] <- vs[g + 1]; vw[(n - g + 1):n] <- vs[n - g]
    list(xt = mean(vs[(g + 1):(n - g)]), h = h, d = (n - 1) * stats::var(vw) / (h * (h - 1)))
  }
  s1 <- win(c(2, 4, 4, 6, 9)); s2 <- win(c(8, 10, 12, 14, 16, 18))
  t_abs <- abs(s1$xt - s2$xt) / sqrt(s1$d + s2$d)
  df_y <- (s1$d + s2$d)^2 / (s1$d^2 / (s1$h - 1) + s2$d^2 / (s2$h - 1))
  p_y <- 2 * stats::pt(-t_abs, df = df_y)

  expect_equal(abs(res$tests$asDF$statistic[1]), t_abs, tolerance = 1e-8)
  expect_equal(res$tests$asDF$p_value[1], p_y, tolerance = 1e-8)
})

test_that("C11: Bayesian t-test BF10 matches independent numerical integral of Rouder et al. (2009) Eq. 1", {
  # Oracle: Rouder et al. (2009) Psychon Bull Rev 16(2):225-237 Eq. 1 with Cauchy(0, sqrt(2)/2)
  skip_if_not_installed("BayesFactor")
  d <- make_tiny_fixture()
  res <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp",
    show_stats = TRUE, test_type = "bayes"
  ))
  x1 <- c(2, 4, 4, 6, 9); x2 <- c(8, 10, 12, 14, 16, 18)
  n1 <- length(x1); n2 <- length(x2); nu <- n1 + n2 - 2; n_eff <- (n1 * n2) / (n1 + n2)
  sp <- sqrt(((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / nu)
  t_val <- (mean(x1) - mean(x2)) / (sp * sqrt(1 / n1 + 1 / n2))
  r <- sqrt(2) / 2
  log_den <- -((nu + 1) / 2) * log(1 + t_val^2 / nu)
  bf_exp <- stats::integrate(function(u) {
    g <- exp(u)
    exp(-0.5 * log1p(n_eff * g) - ((nu + 1) / 2) * log1p(t_val^2 / ((1 + n_eff * g) * nu)) +
          log(r) - 0.5 * log(2 * pi) - 0.5 * u - r^2 / (2 * g) - log_den)
  }, lower = -40, upper = 40, rel.tol = 1e-10)$value

  expect_equal(res$tests$asDF$method[1], "Bayesian t-test")
  expect_equal(res$tests$asDF$statistic[1], bf_exp, tolerance = 1e-5)
  expect_true(is.na(res$tests$asDF$p_value[1]))
  expect_true(is.na(res$tests$asDF$p_adjusted[1]))
})

test_that("C12: p-value adjustment matches hand Bonferroni/Holm/BH and preserves partial NAs", {
  # Oracle: exact Bonferroni, Holm step-down, BH step-up formulas
  d_pna <- data.frame(
    val = c(1:10, 15:24, 50),
    grp = factor(c(rep("A", 10), rep("B", 10), "C"), levels = c("A", "B", "C"))
  )
  res_pna <- quiet_run(ClinicoPath::jjridges(
    data = d_pna, x_var = "val", y_var = "grp",
    show_stats = TRUE, test_type = "parametric", p_adjust_method = "bonferroni"
  ))
  r_ab <- row_by_key(res_pna$tests$asDF, "comparison", "A vs B")
  expect_equal(r_ab$p_adjusted, r_ab$p_value, tolerance = 1e-12)
  expect_equal(sum(is.na(res_pna$tests$asDF$p_adjusted)), 2L)

  res_none <- quiet_run(ClinicoPath::jjridges(
    data = datasets::iris, x_var = "Sepal.Length", y_var = "Species",
    show_stats = TRUE, test_type = "parametric", p_adjust_method = "none"
  ))
  p0 <- res_none$tests$asDF$p_value
  for (m in c("bonferroni", "holm", "fdr")) {
    rm <- quiet_run(ClinicoPath::jjridges(
      data = datasets::iris, x_var = "Sepal.Length", y_var = "Species",
      show_stats = TRUE, test_type = "parametric", p_adjust_method = m
    ))
    expect_equal(rm$tests$asDF$p_adjusted, stats::p.adjust(p0, method = m), tolerance = 1e-12)
  }
})

test_that("C13-C18: all six effect size point estimates match independent formulas and packages", {
  # Oracle: hand closed forms (Delacre et al. 2021 unpooled d/g) + effsize::cliff.delta + DescTools::EtaSq + DescTools::HodgesLehmann
  skip_if_not_installed("effectsize")
  skip_if_not_installed("effsize")
  skip_if_not_installed("DescTools")
  d <- make_tiny_fixture()
  x1 <- c(2, 4, 4, 6, 9); x2 <- c(8, 10, 12, 14, 16, 18)
  v1 <- stats::var(x1); v2 <- stats::var(x2)
  df_w <- (v1 / 5 + v2 / 6)^2 / ((v1 / 5)^2 / 4 + (v2 / 6)^2 / 5)
  J_w <- exp(lgamma(df_w / 2) - 0.5 * log(df_w / 2) - lgamma((df_w - 1) / 2))
  d_unp <- (5 - 13) / sqrt((v1 + v2) / 2)

  run_es <- function(es) {
    quiet_run(ClinicoPath::jjridges(
      data = d, x_var = "val", y_var = "grp",
      show_stats = TRUE, test_type = "parametric", effsize_type = es
    ))$tests$asDF
  }

  t_w <- (5 - 13) / sqrt(v1 / 5 + v2 / 6)
  F_w <- t_w^2
  eta_w <- F_w / (F_w + df_w)
  omega_w <- max(0, (F_w - 1) / (F_w + df_w + 1))

  expect_equal(run_es("d")$effect_size[1], d_unp, tolerance = 1e-10)
  expect_equal(run_es("g")$effect_size[1], d_unp * J_w, tolerance = 1e-8)
  expect_equal(run_es("eta")$effect_size[1], eta_w, tolerance = 1e-10)
  expect_equal(run_es("omega")$effect_size[1], omega_w, tolerance = 1e-10)
  expect_equal(run_es("cliff_delta")$effect_size[1], -14 / 15, tolerance = 1e-10)
  expect_equal(run_es("cliff_delta")$effect_size[1], effsize::cliff.delta(x1, x2)$estimate, tolerance = 1e-10)
  expect_equal(run_es("hodges_lehmann")$effect_size[1], -8.0, tolerance = 1e-10)
  expect_equal(run_es("hodges_lehmann")$effect_size[1], DescTools::HodgesLehmann(x1, x2), tolerance = 1e-10)
})

test_that("C17: Cliff's delta seed is reproducible, isolated via withr::local_seed, and noted in table", {
  # Oracle: deterministic replay with identical seed & caller RNG invariance
  d <- make_tiny_fixture()
  set.seed(777)
  u_ref <- stats::runif(3)
  set.seed(777)
  r1 <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp",
    show_stats = TRUE, test_type = "nonparametric", effsize_type = "cliff_delta", seed = 12345
  ))
  u_after <- stats::runif(3)
  r2 <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp",
    show_stats = TRUE, test_type = "nonparametric", effsize_type = "cliff_delta", seed = 12345
  ))

  expect_identical(u_after, u_ref)
  expect_identical(r1$tests$asDF$ci_lower[1], r2$tests$asDF$ci_lower[1])
  expect_identical(r1$tests$asDF$ci_upper[1], r2$tests$asDF$ci_upper[1])
  expect_match(r1$tests$.__enclos_env__$private$.notes$seed$note, "12345", fixed = TRUE)
})

test_that("C08/C13/C17/C18: metamorphic invariances (affine, monotone, constant facet, non-ASCII names)", {
  # Oracle: invariance properties of Welch t, Cohen's d, Wilcoxon W/p, Cliff's delta, Hodges-Lehmann
  d <- make_tiny_fixture()
  r0 <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", show_stats = TRUE, effsize_type = "d"))

  # Affine 3x + 10
  d_aff <- d; d_aff$val <- 3 * d_aff$val + 10
  r_aff <- quiet_run(ClinicoPath::jjridges(data = d_aff, x_var = "val", y_var = "grp", show_stats = TRUE, effsize_type = "hodges_lehmann"))
  expect_equal(r_aff$tests$asDF$statistic[1], r0$tests$asDF$statistic[1], tolerance = 1e-10)
  expect_equal(r_aff$tests$asDF$effect_size[1], -24.0, tolerance = 1e-10)

  # Constant facet
  d_cf <- d; d_cf$fac <- factor("All")
  r_cf <- quiet_run(ClinicoPath::jjridges(data = d_cf, x_var = "val", y_var = "grp", facet_var = "fac", show_stats = TRUE, effsize_type = "d"))
  expect_equal(r_cf$tests$asDF$statistic[1], r0$tests$asDF$statistic[1], tolerance = 1e-10)

  # Spaces + non-ASCII column names
  d_na <- d; names(d_na) <- c("Ki-67 Index (%)", "Gr\u00e9ade sc")
  r_na <- quiet_run(ClinicoPath::jjridges(data = d_na, x_var = "Ki-67 Index (%)", y_var = "Gr\u00e9ade sc", show_stats = TRUE))
  expect_equal(r_na$tests$asDF$statistic[1], r0$tests$asDF$statistic[1], tolerance = 1e-10)
})

test_that("C27/C29/C33: unfaceted plot overlays match statistics, state-only redraw succeeds, asSource() parses", {
  # Oracle: cross-output identity between ggplot_build layers and statistics table + state-only redraw
  d <- make_tiny_fixture()
  res <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp",
    add_boxplot = TRUE, add_quantiles = TRUE, add_mean = TRUE, add_median = TRUE
  ))
  bld <- quiet_run(ggplot2::ggplot_build(res$plot$state))
  bp_idx <- which(vapply(res$plot$state$layers, function(l) inherits(l$geom, "GeomBoxplot"), logical(1)))[1]
  expect_equal(sort(bld$data[[bp_idx]]$xmiddle), c(4.0, 13.0), tolerance = 1e-10)
  expect_equal(c(sort(bld$data[[bp_idx]]$xlower), sort(bld$data[[bp_idx]]$xupper)), c(4.0, 10.5, 6.0, 15.5), tolerance = 1e-10)

  # State-only redraw with private$.data = NULL
  o <- ClinicoPath:::jjridgesOptions$new(x_var = "val", y_var = "grp")
  a1 <- ClinicoPath:::jjridgesClass$new(options = o, data = d)
  a1$init(); a1$.__enclos_env__$private$.run()
  a2 <- ClinicoPath:::jjridgesClass$new(options = o, data = d)
  a2$init(); a2$.__enclos_env__$private$.data <- NULL
  a2$results$plot$setState(a1$results$plot$state)
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  grDevices::png(f, 600, 450)
  ok <- a2$.__enclos_env__$private$.plot(a2$results$plot, NULL, NULL)
  grDevices::dev.off()
  expect_true(isTRUE(ok))
  expect_gt(file.info(f)$size, 1000L)

  # R syntax export parses cleanly
  expect_no_error(parse(text = a1$asSource()))
})

# ==============================================================================
# LIVE REGRESSION TESTS FOR FIXED VALIDATION DEFECTS (VAL-jjridges-01..18)
# ==============================================================================

test_that("VAL-jjridges-01 (C08/C09/C13/C17/C19): pairwise test order and signs follow factor levels and are invariant to row permutation", {
  # Oracle: canonical ToothGrowth with levels(supp) = c("OJ", "VC")
  tg <- datasets::ToothGrowth
  tg$supp <- factor(tg$supp, levels = c("OJ", "VC"))
  res_tg <- quiet_run(ClinicoPath::jjridges(
    data = tg, x_var = "len", y_var = "supp",
    show_stats = TRUE, test_type = "parametric", effsize_type = "d"
  ))
  expect_equal(res_tg$tests$asDF$comparison[1], "OJ vs VC")
  expect_gt(res_tg$tests$asDF$statistic[1], 0)
  expect_gt(res_tg$tests$asDF$effect_size[1], 0)

  d <- make_tiny_fixture()
  r1 <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", show_stats = TRUE, effsize_type = "d"))
  r2 <- quiet_run(ClinicoPath::jjridges(data = d[c(7:12, 1:6), ], x_var = "val", y_var = "grp", show_stats = TRUE, effsize_type = "d"))
  expect_equal(r2$tests$asDF$comparison[1], r1$tests$asDF$comparison[1])
  expect_equal(r2$tests$asDF$statistic[1], r1$tests$asDF$statistic[1], tolerance = 1e-10)
  expect_equal(r2$tests$asDF$effect_size[1], r1$tests$asDF$effect_size[1], tolerance = 1e-10)
})

test_that("VAL-jjridges-02 (C27/C28/C30): faceted plot overlays and copy-ready reportSummary respect facet_var", {
  d <- data.frame(
    val = c(4, 5, 6, 7, 8,  14, 15, 16, 17, 18,   1, 2, 3,  47, 48, 49, 50, 51, 52, 53),
    grp = factor(c(rep("A", 5), rep("B", 5), rep("A", 3), rep("B", 7)), levels = c("A", "B")),
    fac = factor(c(rep("S1", 10), rep("S2", 10)), levels = c("S1", "S2"))
  )
  res <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp", facet_var = "fac",
    add_mean = TRUE, add_sample_size = TRUE, add_density_values = TRUE
  ))
  bld <- quiet_run(ggplot2::ggplot_build(res$plot$state))
  vl_idx <- which(vapply(res$plot$state$layers, function(l) inherits(l$geom, "GeomVline"), logical(1)))[1]
  txt_idx <- which(vapply(res$plot$state$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_equal(sort(bld$data[[vl_idx]]$xintercept[bld$data[[vl_idx]]$PANEL == 1]), c(6.0, 16.0), tolerance = 1e-8)
  expect_equal(sort(as.character(bld$data[[txt_idx[1]]]$label[bld$data[[txt_idx[1]]]$PANEL == 1])), c("n=5", "n=5"))
  expect_true(grepl("S1", res$reportSummary$content, fixed = TRUE) && grepl("S2", res$reportSummary$content, fixed = TRUE))
})

test_that("VAL-jjridges-03 (C23): explicit NA factor level is excluded rather than analyzed as a group", {
  d <- data.frame(
    val = c(1:5, 11:15, 21:25),
    grp = addNA(factor(c(rep("A", 5), rep("B", 5), rep(NA, 5))))
  )
  res <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", show_stats = TRUE))
  expect_equal(nrow(res$statistics$asDF), 2L)
  expect_equal(sum(res$statistics$asDF$n), 10L)
})

test_that("VAL-jjridges-04 (C25/C31): survival_time_distribution preset warns about right-censoring", {
  res <- quiet_run(ClinicoPath::jjridges(
    data = ClinicoPath::jjridges_survival, x_var = "survival_months", y_var = "treatment",
    clinicalPreset = "survival_time_distribution"
  ))
  expect_match(paste(res$notices$content, res$warnings$content), "censor", ignore.case = TRUE)
})

test_that("VAL-jjridges-05 (C13/C34): Cohen's d CI duality or pooled_sd disclosure under unequal variances", {
  set.seed(303)
  x1 <- as.numeric(scale(stats::rnorm(50))) * 1.0
  x2 <- as.numeric(scale(stats::rnorm(10))) * 4.0 + 1.6
  d <- data.frame(val = c(x1, x2), grp = factor(c(rep("A", 50), rep("B", 10))))
  res <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp",
    show_stats = TRUE, test_type = "parametric", effsize_type = "d"
  ))
  tb <- res$tests$asDF
  welch_sig <- tb$p_value[1] < 0.05
  ci_excl0 <- (tb$ci_lower[1] > 0) || (tb$ci_upper[1] < 0)
  expect_true(welch_sig == ci_excl0)
})

test_that("VAL-jjridges-06 (C10): Yuen robust test statistic preserves contrast sign", {
  skip_if_not_installed("WRS2")
  d <- make_tiny_fixture()
  res <- quiet_run(ClinicoPath::jjridges(
    data = d, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "robust"
  ))
  expect_lt(res$tests$asDF$statistic[1], 0)
})

test_that("VAL-jjridges-07 (C20): degenerate constant/trimmed-constant/extreme-Bayes inputs never emit silent Inf or NaN", {
  d1 <- data.frame(val = c(rep(5, 10), rep(7, 10)), grp = factor(rep(c("A", "B"), each = 10)))
  r1 <- quiet_run(ClinicoPath::jjridges(data = d1, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "nonparametric", effsize_type = "d"))
  expect_true(is.na(r1$tests$asDF$effect_size[1]) || is.finite(r1$tests$asDF$effect_size[1]))

  d2 <- data.frame(val = c(0, 5, 5, 5, 5, 10, 0, 7, 7, 7, 7, 10), grp = factor(rep(c("A", "B"), each = 6)))
  r2 <- quiet_run(ClinicoPath::jjridges(data = d2, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "robust"))
  expect_true(is.na(r2$tests$asDF$statistic[1]) || is.finite(r2$tests$asDF$statistic[1]))
  expect_match(r2$notices$content, "WARNING", fixed = TRUE)

  d3 <- data.frame(val = c(seq(0, 1, length.out = 100), seq(500, 501, length.out = 100)), grp = factor(rep(c("A", "B"), each = 100)))
  r3 <- quiet_run(ClinicoPath::jjridges(data = d3, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "bayes"))
  expect_true(is.na(r3$tests$asDF$statistic[1]) || is.finite(r3$tests$asDF$statistic[1]))
  expect_match(r3$notices$content, "overflow|infinite|exceeds", ignore.case = TRUE)
})

test_that("VAL-jjridges-08 (C26): zero-IQR data with bandwidth='nrd' falls back to positive finite bandwidth", {
  d <- data.frame(val = c(rep(0, 40), 1:10, rep(0, 40), 1:10), grp = factor(rep(c("A", "B"), each = 50)))
  res <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", bandwidth = "nrd"))
  bld <- quiet_run(ggplot2::ggplot_build(res$plot$state))
  expect_true(all(is.finite(bld$data[[1]]$density)) && max(bld$data[[1]]$density, na.rm = TRUE) > 0)
})

test_that("VAL-jjridges-09 (C23): jamovi Nominal-integer x_var with values attribute is accepted", {
  f <- factor(c(2, 4, 4, 6, 9, 8, 10, 12, 14, 16, 18))
  attr(f, "values") <- as.integer(levels(f))
  d <- data.frame(val = f, grp = factor(c(rep("Low", 5), rep("High", 6))))
  res <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp"))
  expect_equal(nrow(res$statistics$asDF), 2L)
})

test_that("VAL-jjridges-10 (C24): cross-sectional pathology counts and case-control y_var='case' do not trigger false repeated-measures warnings", {
  set.seed(101)
  d_ln <- data.frame(lymph_nodes = c(stats::rpois(30, 1.5), stats::rpois(30, 3.5)), stage = factor(rep(c("I", "III"), each = 30)))
  r_ln <- quiet_run(ClinicoPath::jjridges(data = d_ln, x_var = "lymph_nodes", y_var = "stage", show_stats = TRUE))
  expect_false(grepl("Independence Assumption Violation", r_ln$notices$content, fixed = TRUE))

  d_cc <- data.frame(biomarker = c(stats::rnorm(30, 10, 2), stats::rnorm(30, 12, 2)), case = factor(rep(c("Control", "Tumor"), each = 30)))
  r_cc <- quiet_run(ClinicoPath::jjridges(data = d_cc, x_var = "biomarker", y_var = "case", show_stats = TRUE))
  expect_false(grepl("Independence Assumption Violation", r_cc$notices$content, fixed = TRUE))
})

test_that("VAL-jjridges-11 (C22): Unadjusted p-values notice does not fire for Bayesian tests", {
  skip_if_not_installed("BayesFactor")
  res <- quiet_run(ClinicoPath::jjridges(
    data = datasets::iris, x_var = "Sepal.Length", y_var = "Species",
    show_stats = TRUE, test_type = "bayes", p_adjust_method = "none"
  ))
  expect_false(grepl("Unadjusted p-values", res$notices$content, fixed = TRUE))
})

test_that("VAL-jjridges-12 (C26/C27/C28/C31): boxplot outlier disclosure, density_values bandwidth fidelity, and violin fill_ridges=FALSE", {
  d <- make_tiny_fixture()
  r_bw <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", bandwidth = "custom", bandwidth_value = 5.0, add_density_values = TRUE))
  bld_bw <- quiet_run(ggplot2::ggplot_build(r_bw$plot$state))
  expect_equal(sort(as.numeric(as.character(bld_bw$data[[2]]$label))), c(4.8, 13.0), tolerance = 1e-6)

  r_v <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", plot_type = "violin_ridges", fill_ridges = FALSE))
  expect_true(all(is.na(quiet_run(ggplot2::ggplot_build(r_v$plot$state))$data[[1]]$fill)))
})

test_that("VAL-jjridges-13 (C34): Cliff's delta 95% CI achieves nominal coverage under non-null separation (n=15)", {
  set.seed(20260505)
  true_cd <- 2 * stats::pnorm(1 / sqrt(2)) - 1
  d0 <- data.frame(val = 1:6, grp = factor(rep(c("A", "B"), each = 3)))
  o <- ClinicoPath:::jjridgesOptions$new(x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "nonparametric", effsize_type = "cliff_delta", seed = 20250101L)
  runner <- ClinicoPath:::jjridgesClass$new(options = o, data = d0)
  cov <- mean(vapply(seq_len(200L), function(i) {
    res <- quiet_run(runner$.__enclos_env__$private$.calculateEffectSizeWithCI(stats::rnorm(15, 1, 1), stats::rnorm(15, 0, 1)))
    res$ci_lower <= true_cd && res$ci_upper >= true_cd
  }, logical(1)))
  expect_gte(cov, 0.9038)
})

test_that("VAL-jjridges-14 (C23/C29): post-NA sample size warning fires and validation failure hides empty outputs", {
  d_na <- data.frame(val = c(1:7, rep(NA_real_, 8)), grp = factor(c("A", "A", "A", "B", "B", "B", "B", rep("A", 8))))
  r_na <- quiet_run(ClinicoPath::jjridges(data = d_na, x_var = "val", y_var = "grp"))
  expect_match(r_na$notices$content, "below recommended minimum of 10", fixed = TRUE)

  r_emp <- quiet_run(ClinicoPath::jjridges(data = d_na[0, ], x_var = "val", y_var = "grp"))
  expect_false(r_emp$statistics$visible)
})

test_that("VAL-jjridges-15 (C25): age_by_stage preset configures multiplicity adjustment", {
  res <- quiet_run(ClinicoPath::jjridges(
    data = datasets::iris, x_var = "Sepal.Length", y_var = "Species", clinicalPreset = "age_by_stage"
  ))
  expect_false(identical(res$tests$asDF$p_adjusted, res$tests$asDF$p_value))
})

test_that("VAL-jjridges-16 (C31): AboutPanel gradient description matches after_stat(x) mapping", {
  d <- make_tiny_fixture()
  res <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", plot_type = "density_ridges_gradient", showAboutPanel = TRUE))
  expect_false(grepl("Color intensity shows density within each ridge", res$aboutPanel$content, fixed = TRUE))
})

test_that("VAL-jjridges-17 (C21/C31): diagnostic notices format p<0.001 and BF10 thresholds cite source", {
  set.seed(202)
  d <- data.frame(val = c(stats::rexp(80, 1), stats::rnorm(80, 10, 0.2)), grp = factor(rep(c("Skew", "Norm"), each = 80)))
  r1 <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "parametric"))
  expect_false(grepl("p=0[),]", r1$notices$content))

  r2 <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "bayes", showAssumptions = TRUE))
  expect_match(r2$assumptionsPanel$content, "Jeffreys|Wagenmakers|Rouder", ignore.case = TRUE)
})

test_that("VAL-jjridges-18 (C32): ggridges author accuracy and primary method citations in refs", {
  root <- testthat::test_path("..", "..")
  refs_all <- yaml::read_yaml(file.path(root, "jamovi", "00refs.yaml"))$refs
  r_refs <- yaml::read_yaml(file.path(root, "jamovi", "jjridges.r.yaml"))$refs
  expect_false(grepl("RStudio", refs_all$ggridges$author, fixed = TRUE))
  expect_true(all(c("btt", "cliff1993", "newcombe2006", "hodgeslehmann1963", "yuen1974",
                    "jeffreys1961", "leewagenmakers2013", "delacre2021") %in% r_refs))
})

# ==============================================================================
# INDEPENDENT REVIEW REGRESSION TESTS (IR-jjridges-01 through IR-jjridges-13)
# ==============================================================================

test_that("IR-jjridges-01 & 11: R6 lifecycle recovers visibility after error and hides stale preset warnings", {
  d <- make_tiny_fixture()
  o <- ClinicoPath:::jjridgesOptions$new(x_var = "val", y_var = "grp", show_stats = TRUE)
  a <- ClinicoPath:::jjridgesClass$new(options = o, data = d)
  a$init()
  quiet_run(a$.__enclos_env__$private$.run())
  a$.__enclos_env__$private$.data <- d[d$grp == "Low", ]
  quiet_run(a$.__enclos_env__$private$.run())
  expect_false(a$results$plot$visible)
  expect_false(a$results$tests$visible)

  a$.__enclos_env__$private$.data <- d
  quiet_run(a$.__enclos_env__$private$.run())
  expect_true(a$results$plot$visible)
  expect_true(a$results$statistics$visible)
  expect_true(a$results$tests$visible)
  expect_true(a$results$interpretation$visible)

  # Preset -> custom hides warnings banner
  o2 <- ClinicoPath:::jjridgesOptions$new(x_var = "val", y_var = "grp", clinicalPreset = "biomarker_distribution")
  a2 <- ClinicoPath:::jjridgesClass$new(options = o2, data = d)
  a2$init(); quiet_run(a2$.__enclos_env__$private$.run())
  expect_true(a2$results$warnings$visible)
  opt_cp <- a2$.__enclos_env__$private$.options$option("clinicalPreset")
  opt_cp$.__enclos_env__$private$.value <- "custom"
  quiet_run(a2$.__enclos_env__$private$.run())
  expect_false(a2$results$warnings$visible)
})

test_that("IR-jjridges-02: presentation option reverse_order preserves statistical contrasts and signs", {
  d <- make_tiny_fixture()
  r0 <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", reverse_order = FALSE, show_stats = TRUE, test_type = "parametric", effsize_type = "d"))
  r1 <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", reverse_order = TRUE, show_stats = TRUE, test_type = "parametric", effsize_type = "d"))
  expect_identical(r1$tests$asDF$comparison, r0$tests$asDF$comparison)
  expect_equal(r1$tests$asDF$statistic, r0$tests$asDF$statistic, tolerance = 1e-12)
  expect_equal(r1$tests$asDF$effect_size, r0$tests$asDF$effect_size, tolerance = 1e-12)
  # Plot Y levels are reversed while statistics stay invariant
  expect_identical(levels(r1$plot$state$data$y), rev(levels(r0$plot$state$data$y)))
})

test_that("IR-jjridges-03: Welch one-way eta/omega preserves test-CI duality under heteroscedastic imbalance", {
  set.seed(303)
  d_het <- data.frame(
    val = c(as.numeric(scale(stats::rnorm(50))) * 1.0, as.numeric(scale(stats::rnorm(10))) * 4.0 + 1.6),
    grp = factor(c(rep("A", 50), rep("B", 10)))
  )
  r_eta <- quiet_run(ClinicoPath::jjridges(data = d_het, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "parametric", effsize_type = "eta"))
  tb <- r_eta$tests$asDF
  expect_gt(tb$p_value[1], 0.05)
  expect_equal(tb$ci_lower[1], 0, tolerance = 1e-12)
  expect_match(r_eta$tests$.__enclos_env__$private$.notes$effsize_method$note, "Welch", ignore.case = TRUE)
})

test_that("IR-jjridges-04: complete-separation Cliff's delta uses Newcombe Method 5 non-degenerate CI with warning", {
  d_disj <- data.frame(val = 1:10, grp = factor(rep(c("Low", "High"), each = 5), levels = c("Low", "High")))
  r_disj <- quiet_run(ClinicoPath::jjridges(
    data = d_disj, x_var = "val", y_var = "grp", show_stats = TRUE,
    test_type = "nonparametric", effsize_type = "cliff_delta", seed = 20261124
  ))
  tb <- r_disj$tests$asDF
  expect_equal(tb$effect_size[1], -1, tolerance = 1e-12)
  expect_equal(tb$ci_lower[1], -1, tolerance = 1e-12)
  expect_gt(tb$ci_upper[1], -1)
  expect_lt(tb$ci_upper[1], 0)
  expect_match(r_disj$notices$content, "Complete separation|Newcombe", ignore.case = TRUE)
})

test_that("IR-jjridges-06 & 10: per-group bandwidth avoids smearing and within-group diagnostics avoid false skewness warnings", {
  set.seed(404)
  d_sep <- data.frame(val = c(stats::rnorm(40, 0, 1), stats::rnorm(40, 100, 1)), grp = factor(rep(c("Low", "High"), each = 40)))
  r_sep <- quiet_run(ClinicoPath::jjridges(data = d_sep, x_var = "val", y_var = "grp", bandwidth = "nrd"))
  bw_used <- unique(r_sep$plot$state$layers[[1]]$stat_params$bandwidth)
  within_bw <- mean(c(stats::bw.nrd(d_sep$val[d_sep$grp == "Low"]), stats::bw.nrd(d_sep$val[d_sep$grp == "High"])))
  expect_equal(bw_used, within_bw, tolerance = 1e-10)

  set.seed(405)
  d_csep <- data.frame(val = c(stats::rnorm(45, 0, 1), stats::rnorm(45, 2, 1), stats::rnorm(10, 30, 1)), grp = factor(c(rep("G1", 45), rep("G2", 45), rep("G3", 10))))
  r_csep <- quiet_run(ClinicoPath::jjridges(data = d_csep, x_var = "val", y_var = "grp"))
  expect_false(grepl("extreme outliers|high skewness", r_csep$notices$content, ignore.case = TRUE))
})

test_that("IR-jjridges-07 & 12: gradient warns on fill_var, add_mean stratifies by fill_var, and custom_annotations parses group names/commas & warns on malformed", {
  set.seed(501)
  d_g <- data.frame(val = stats::rnorm(80, 10, 2), grp = factor(rep(c("A", "B"), each = 40)), sub = factor(rep(rep(c("M", "F"), each = 20), 2)))
  r_grad <- quiet_run(ClinicoPath::jjridges(data = d_g, x_var = "val", y_var = "grp", fill_var = "sub", plot_type = "density_ridges_gradient", alpha = 0.35, fill_ridges = FALSE))
  expect_match(r_grad$notices$content, "Gradient mode overrides categorical fill", fixed = TRUE)
  expect_equal(unique(suppressMessages(ggplot2::ggplot_build(r_grad$plot$state))$data[[1]]$alpha), 0.35, tolerance = 1e-8)

  r_ov <- quiet_run(ClinicoPath::jjridges(data = d_g, x_var = "val", y_var = "grp", fill_var = "sub", add_mean = TRUE, add_sample_size = TRUE, add_boxplot = TRUE))
  bld <- suppressMessages(ggplot2::ggplot_build(r_ov$plot$state))
  vl_idx <- which(vapply(r_ov$plot$state$layers, function(l) inherits(l$geom, "GeomVline"), logical(1)))[1]
  expect_equal(nrow(bld$data[[vl_idx]]), nrow(r_ov$statistics$asDF))

  d <- make_tiny_fixture()
  r_ann <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", custom_annotations = "5,Low,Peak, A;13,2,High Mean;bad_entry"))
  n_txt <- sum(vapply(r_ann$plot$state$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_equal(n_txt, 2L)
  expect_match(r_ann$notices$content, "Invalid custom annotations", fixed = TRUE)
})

test_that("IR-jjridges-08, 09, 13: clearWith completeness, accurate Welch/Bayes copy, and clinical ID clustering detection", {
  root <- testthat::test_path("..", "..")
  ry <- yaml::read_yaml(file.path(root, "jamovi", "jjridges.r.yaml"))
  cw <- function(nm) unlist(Filter(function(x) identical(x$name, nm), ry$items)[[1]]$clearWith)
  expect_true(all(c("fill_var", "facet_var") %in% cw("reportSummary")))
  expect_true(all(c("p_adjust_method", "bandwidth", "add_quantiles", "quantiles", "fill_var", "facet_var", "plot_type") %in% cw("notices")))
  expect_true("clinicalPreset" %in% cw("assumptionsPanel"))
  expect_true(all(c("add_boxplot", "show_stats", "test_type") %in% cw("interpretation")))

  d <- make_tiny_fixture()
  rp <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "parametric", showAssumptions = TRUE))
  expect_match(rp$assumptionsPanel$content, "Welch", ignore.case = TRUE)
  expect_false(grepl("Homogeneity of Variance:</strong> Groups should have similar spread", rp$assumptionsPanel$content, fixed = TRUE))

  rb <- quiet_run(ClinicoPath::jjridges(data = d, x_var = "val", y_var = "grp", show_stats = TRUE, test_type = "bayes", showAssumptions = TRUE))
  expect_false(grepl("statistically significant", rb$interpretation$content, ignore.case = TRUE))
  expect_match(rb$assumptionsPanel$content, "equal population variances|pooled Student", ignore.case = TRUE)

  d_mrn <- data.frame(mrn = rep(paste0("M", 1:5), each = 4), val = stats::rnorm(20, 10, 2), grp = factor(rep(c("Core", "Margin"), 10)))
  r_mrn <- quiet_run(ClinicoPath::jjridges(data = d_mrn, x_var = "val", y_var = "grp", show_stats = TRUE))
  expect_match(r_mrn$notices$content, "Independence Assumption Violation", fixed = TRUE)
})
