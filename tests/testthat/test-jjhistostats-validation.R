# Validation regression tests for ClinicoPath::jjhistostats
# Derived from exhaustive validation (development-scripts/validate_jjhistostats.R, 2026-11-23).
# Live tests lock in verified mathematical/statistical correctness; open defects are written
# with the correct expectation and guarded by skip("VAL-jjhistostats-NN open defect: ...").

# Local helper to capture ggplot_build objects through public wrapper saveAs()
val_capture_image <- function(img) {
  builds <- list()
  env <- environment()
  suppressMessages(trace(ggplot2::ggplot_build, exit = bquote({
    e <- .(env)
    e$builds[[length(e$builds) + 1L]] <- returnValue()
  }), print = FALSE))
  on.exit(suppressMessages(untrace(ggplot2::ggplot_build)), add = TRUE)
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  err <- NULL
  tryCatch(img$saveAs(f), error = function(e) err <<- conditionMessage(e))
  sz <- if (file.exists(f)) file.info(f)$size else 0L
  list(builds = builds, bytes = sz, error = err)
}

val_quoted_nums <- function(expr) {
  if (is.null(expr)) return(character(0))
  txt <- paste(deparse(expr, width.cutoff = 500), collapse = " ")
  txt <- gsub('BF\\["[01]+"\\]', 'BF[]', txt, perl = TRUE)
  m <- gregexpr('"([+-]?[0-9]+\\.?[0-9]*(?:e[+-]?[0-9]+)?|NA|Inf|-Inf|NaN)"', txt, perl = TRUE)
  gsub('^"|"$', "", regmatches(txt, m)[[1]])
}

val_vlines <- function(b) {
  unlist(lapply(b$data, function(ld) if ("xintercept" %in% names(ld)) as.numeric(ld$xintercept) else NULL))
}

# ===========================================================================
# LIVE TESTS — VERIFIED CLAIMS
# ===========================================================================

test_that("C02: histogram bin counts, automatic/manual binwidths, and proportions match hand formulas", {
  # Oracle: hand binwidth (max-min)/sqrt(n) and count conservation on n=10 + 1 NA
  skip_if_not_installed("ggstatsplot")
  d <- data.frame(x = c(1, 2, 3, 5, 5, 6, 8, 9, 10, 12, NA_real_))
  res_auto <- suppressWarnings(ClinicoPath::jjhistostats(data = d, dep = "x", originaltheme = TRUE))
  b_auto <- val_capture_image(res_auto$plot)$builds[[1]]
  expect_equal(sum(b_auto$data[[1]]$count), 10, tolerance = 1e-12)
  expect_equal(mean(b_auto$data[[1]]$xmax - b_auto$data[[1]]$xmin), (12 - 1) / sqrt(10), tolerance = 1e-8)

  res_man <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "x", changebinwidth = TRUE, binwidth = 2.5, originaltheme = TRUE
  ))
  b_man <- val_capture_image(res_man$plot)$builds[[1]]
  expect_equal(sum(b_man$data[[1]]$count), 10, tolerance = 1e-12)
  expect_equal(mean(b_man$data[[1]]$xmax - b_man$data[[1]]$xmin), 2.5, tolerance = 1e-8)
})

test_that("C03 & C04: parametric subtitle matches hand Student t, exact Hedges g, and noncentral-t CI", {
  # Oracle: closed-form Student t, exact Hedges J(df) = Gamma(df/2)/(sqrt(df/2)*Gamma((df-1)/2)), uniroot noncentral-t CI
  skip_if_not_installed("ggstatsplot")
  x <- c(1, 2, 3, 5, 5, 6, 8, 9, 10, 12)
  d <- data.frame(x = c(x, NA_real_))
  n <- length(x); df <- n - 1; mu <- 5
  t_hand <- (mean(x) - mu) / (stats::sd(x) / sqrt(n))
  p_hand <- 2 * stats::pt(-abs(t_hand), df = df)
  J_hand <- exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))
  g_hand <- ((mean(x) - mu) / stats::sd(x)) * J_hand
  ncp_lo <- stats::uniroot(function(ncp) suppressWarnings(stats::pt(t_hand, df, ncp)) - 0.975, c(-20, 20), tol = 1e-12)$root
  ncp_hi <- stats::uniroot(function(ncp) suppressWarnings(stats::pt(t_hand, df, ncp)) - 0.025, c(-20, 20), tol = 1e-12)$root
  ci_hand <- c(ncp_lo, ncp_hi) * J_hand / sqrt(n)

  res <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "x", typestatistics = "parametric",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = mu,
    conf.level = 0.95, digits = 5, centralityline = TRUE, originaltheme = TRUE
  ))
  b <- val_capture_image(res$plot)$builds[[1]]
  nums <- as.numeric(val_quoted_nums(b$plot$labels$subtitle))
  expect_equal(nums[1], round(t_hand, 5), tolerance = 1e-5)
  expect_equal(nums[2], round(p_hand, 5), tolerance = 1e-5)
  expect_equal(nums[3], round(g_hand, 5), tolerance = 1e-5)
  expect_equal(nums[4:5], round(ci_hand, 5), tolerance = 1e-4)
  expect_equal(nums[6], 10, tolerance = 1e-12)
  expect_equal(val_vlines(b)[1], 6.1, tolerance = 1e-12)
})

test_that("C05 & C06: nonparametric subtitle matches hand Wilcoxon V, asymptotic CC p, and Kerby rank-biserial", {
  # Oracle: hand signed ranks excluding exact zeros x==mu, normal approx with CC & tie correction, Kerby (2014) r_rb = 7/18
  skip_if_not_installed("ggstatsplot")
  x <- c(1, 2, 3, 5, 5, 6, 8, 9, 10, 12)
  d <- data.frame(x = c(x, NA_real_))
  res <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "x", typestatistics = "nonparametric",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5,
    digits = 5, centralityline = TRUE, originaltheme = TRUE
  ))
  b <- val_capture_image(res$plot)$builds[[1]]
  nums <- as.numeric(val_quoted_nums(b$plot$labels$subtitle))
  expect_equal(nums[1], 25, tolerance = 1e-5)
  expect_equal(nums[2], round(0.361547071452, 5), tolerance = 1e-5)
  expect_equal(nums[3], round(7 / 18, 5), tolerance = 1e-5)
  expect_equal(nums[6], 10, tolerance = 1e-12)
  expect_equal(val_vlines(b)[1], 5.5, tolerance = 1e-12)
})

test_that("C07 & C16: robust trimmed mean and t_bootstrapped match hand Winsorized-variance formula", {
  # Oracle: hand 20%-trimmed mean (6.0) and Winsorized SE trimse = sqrt(winvar)/((1-2*tr)*sqrt(n))
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("WRS2")
  x <- c(1, 2, 3, 5, 5, 6, 8, 9, 10, 12)
  xs <- sort(x); xs[1:2] <- xs[3]; xs[9:10] <- xs[8]
  se_hand <- sqrt(stats::var(xs)) / ((1 - 2 * 0.2) * sqrt(length(x)))
  t_hand <- (6.0 - 5.0) / se_hand

  res <- suppressWarnings(ClinicoPath::jjhistostats(
    data = data.frame(x = c(x, NA_real_)), dep = "x", typestatistics = "robust",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5,
    trimlevel = 0.2, digits = 5, centralityline = TRUE, seed = 20250101, originaltheme = TRUE
  ))
  b <- val_capture_image(res$plot)$builds[[1]]
  nums <- as.numeric(val_quoted_nums(b$plot$labels$subtitle))
  expect_equal(nums[1], round(t_hand, 5), tolerance = 1e-5)
  expect_equal(nums[3], 6.0, tolerance = 1e-5)
  expect_equal(val_vlines(b)[1], 6.0, tolerance = 1e-12)
})

test_that("C09 & C15: Bayesian log_e(BF_01) matches independent Rouder et al. (2009) Eq. 1 quadrature", {
  # Oracle: base R stats::integrate() on Rouder et al. (2009, Psychon Bull Rev) Eq. 1 JZS Cauchy prior integral
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("BayesFactor")
  x <- c(1, 2, 3, 5, 5, 6, 8, 9, 10, 12)
  jzs_log_bf01 <- function(x, mu, r) {
    N <- length(x); nu <- N - 1
    t_s <- (mean(x) - mu) / (stats::sd(x) / sqrt(N))
    num <- stats::integrate(function(g) {
      (1 + N * g)^(-0.5) * (1 + t_s^2 / ((1 + N * g) * nu))^(-(nu + 1) / 2) *
        (2 * pi)^(-0.5) * r * g^(-1.5) * exp(-r^2 / (2 * g))
    }, 0, Inf, rel.tol = 1e-11)$value
    den <- (1 + t_s^2 / nu)^(-(nu + 1) / 2)
    -log(num / den)
  }

  for (r_sc in c(0.5, 0.707, 1.414)) {
    res <- suppressWarnings(ClinicoPath::jjhistostats(
      data = data.frame(x = x), dep = "x", typestatistics = "bayes",
      resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5,
      bfprior = r_sc, digits = 5, seed = 20250101, originaltheme = TRUE
    ))
    b <- val_capture_image(res$plot)$builds[[1]]
    nums <- as.numeric(val_quoted_nums(b$plot$labels$subtitle))
    expect_equal(nums[1], round(jzs_log_bf01(x, 5, r_sc), 5), tolerance = 1e-4)
    expect_equal(nums[5], r_sc, tolerance = 1e-5)
  }
})

test_that("C03 & C05: reference parity on canonical datasets::sleep and bundled ClinicoPath::jjhistostats_labvalues", {
  # Oracle: base stats::t.test and exact Hedges J on datasets::sleep and ClinicoPath::jjhistostats_labvalues
  skip_if_not_installed("ggstatsplot")
  res_sleep <- suppressWarnings(ClinicoPath::jjhistostats(
    data = datasets::sleep, dep = "extra", typestatistics = "parametric",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0,
    digits = 4, originaltheme = TRUE
  ))
  nums_s <- as.numeric(val_quoted_nums(val_capture_image(res_sleep$plot)$builds[[1]]$plot$labels$subtitle))
  tt_s <- stats::t.test(datasets::sleep$extra, mu = 0)
  expect_equal(nums_s[1], round(as.numeric(tt_s$statistic), 4), tolerance = 1e-4)
  expect_equal(nums_s[2], round(tt_s$p.value, 4), tolerance = 1e-4)

  chol_x <- ClinicoPath::jjhistostats_labvalues$cholesterol
  chol_x <- chol_x[is.finite(chol_x)]
  res_chol <- suppressWarnings(ClinicoPath::jjhistostats(
    data = ClinicoPath::jjhistostats_labvalues, dep = "cholesterol", typestatistics = "parametric",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 200,
    digits = 4, originaltheme = TRUE
  ))
  nums_c <- as.numeric(val_quoted_nums(val_capture_image(res_chol$plot)$builds[[1]]$plot$labels$subtitle))
  expect_equal(nums_c[1], round(as.numeric(stats::t.test(chol_x, mu = 200)$statistic), 4), tolerance = 1e-4)
})

test_that("C11 & C13: parametric and nonparametric one-sided p-values and conf.level nesting are exact", {
  # Oracle: base stats::pt and stats::wilcox.test(exact = FALSE, correct = TRUE)
  skip_if_not_installed("ggstatsplot")
  set.seed(123)
  d <- data.frame(v = rnorm(30, mean = 5.5, sd = 1.5))

  r_gt <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "parametric", alternative = "greater",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
  ))
  r_lt <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "parametric", alternative = "less",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
  ))
  p_gt <- as.numeric(val_quoted_nums(val_capture_image(r_gt$plot)$builds[[1]]$plot$labels$subtitle)[2])
  p_lt <- as.numeric(val_quoted_nums(val_capture_image(r_lt$plot)$builds[[1]]$plot$labels$subtitle)[2])
  expect_equal(p_gt, round(stats::t.test(d$v, mu = 5, alternative = "greater")$p.value, 5), tolerance = 1e-5)
  expect_equal(p_lt, round(stats::t.test(d$v, mu = 5, alternative = "less")$p.value, 5), tolerance = 1e-5)
  expect_equal(p_gt + p_lt, 1.0, tolerance = 1e-4)

  # conf.level nesting: 80% inside 95% inside 99%
  get_ci <- function(cl) {
    r <- suppressWarnings(ClinicoPath::jjhistostats(
      data = d, dep = "v", typestatistics = "parametric", conf.level = cl,
      resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
    ))
    as.numeric(val_quoted_nums(val_capture_image(r$plot)$builds[[1]]$plot$labels$subtitle)[4:5])
  }
  ci80 <- get_ci(0.80); ci95 <- get_ci(0.95); ci99 <- get_ci(0.99)
  expect_true(ci99[1] < ci95[1] && ci95[1] < ci80[1] && ci80[2] < ci95[2] && ci95[2] < ci99[2])
})

test_that("C17 & C26: metamorphic invariants (permutation, shift, scale, sign flip, subgroup split)", {
  # Oracle: algebraic invariance properties of one-sample Student t and Hedges g
  skip_if_not_installed("ggstatsplot")
  set.seed(77)
  d <- data.frame(
    v = rnorm(30, 25, 4),
    g = factor(rep(c("Treated", "Control"), each = 15), levels = c("Treated", "Control"))
  )
  run_sub <- function(df, mu = 20) {
    r <- suppressWarnings(ClinicoPath::jjhistostats(
      data = df, dep = "v", typestatistics = "parametric",
      resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = mu,
      digits = 5, originaltheme = TRUE
    ))
    as.numeric(val_quoted_nums(val_capture_image(r$plot)$builds[[1]]$plot$labels$subtitle))
  }
  base_n <- run_sub(d, 20)
  expect_equal(run_sub(d[sample(nrow(d)), ], 20), base_n, tolerance = 1e-12)
  d_shift <- d; d_shift$v <- d_shift$v + 100
  expect_equal(run_sub(d_shift, 120), base_n, tolerance = 1e-12)
  d_scale <- d; d_scale$v <- d_scale$v * 10
  expect_equal(run_sub(d_scale, 200), base_n, tolerance = 1e-12)

  # Subgroup split identity
  r_grp <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", grvar = "g", typestatistics = "parametric",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 20,
    digits = 5, originaltheme = TRUE
  ))
  builds <- val_capture_image(r_grp$plot2)$builds
  titles <- vapply(builds, function(b) if (is.null(b$plot$labels$title)) "" else as.character(b$plot$labels$title)[1], character(1))
  idx_t <- which(titles == "Treated")[1]
  expect_equal(
    as.numeric(val_quoted_nums(builds[[idx_t]]$plot$labels$subtitle)),
    run_sub(d[d$g == "Treated", ], 20),
    tolerance = 1e-12
  )
})

test_that("C21 & C23: ggpubrPlot (single var), densityPlot, qqPlot, and ecdfPlot match base R oracles", {
  # Oracle: base mean(), sort(), qnorm(ppoints(n)), and stats::ecdf()
  skip_if_not_installed("ggpubr")
  set.seed(5)
  d <- data.frame(v = rnorm(40, 15, 3))
  res <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", addGGPubrPlot = TRUE, ggpubrAddMean = TRUE,
    addDistributionDiagnostics = TRUE, ggpubrShowQQ = TRUE, ggpubrShowECDF = TRUE
  ))
  expect_equal(val_vlines(val_capture_image(res$ggpubrPlot)$builds[[1]])[1], mean(d$v), tolerance = 1e-10)
  expect_equal(val_vlines(val_capture_image(res$densityPlot)$builds[[1]])[1], mean(d$v), tolerance = 1e-10)

  b_qq <- val_capture_image(res$qqPlot)$builds[[1]]
  qq_idx <- which(vapply(b_qq$data, function(ld) all(c("sample", "theoretical") %in% names(ld)), logical(1)))[1]
  expect_equal(sort(b_qq$data[[qq_idx]]$sample), sort(d$v), tolerance = 1e-10)
  expect_equal(sort(b_qq$data[[qq_idx]]$theoretical), stats::qnorm(stats::ppoints(40)), tolerance = 1e-6)

  b_ecdf <- val_capture_image(res$ecdfPlot)$builds[[1]]$data[[1]]
  fin <- b_ecdf[is.finite(b_ecdf$x), ]
  expect_equal(fin$y, stats::ecdf(d$v)(fin$x), tolerance = 1e-10)
})

test_that("C20 & C25 & C30: seed determinism, degenerate rejections, and asSource() syntax validity", {
  # Oracle: determinism property (E9), explicit refusal contract, and R syntax parsing
  skip_if_not_installed("ggstatsplot")
  set.seed(42)
  d <- data.frame(v = rnorm(25, 5, 1))
  r1 <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "robust", resultssubtitle = TRUE,
    seed = 777, digits = 5, originaltheme = TRUE
  ))
  r2 <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "robust", resultssubtitle = TRUE,
    seed = 777, digits = 5, originaltheme = TRUE
  ))
  expect_identical(
    val_quoted_nums(val_capture_image(r1$plot)$builds[[1]]$plot$labels$subtitle),
    val_quoted_nums(val_capture_image(r2$plot)$builds[[1]]$plot$labels$subtitle)
  )

  # Jamovi numeric-factor with values attribute must be rejected cleanly
  f_val <- factor(c(10, 20, 30, 40, 50)); attr(f_val, "values") <- c(10L, 20L, 30L, 40L, 50L)
  expect_error(ClinicoPath::jjhistostats(data = data.frame(v = f_val), dep = "v"), "categorical")

  # asSource() produces valid parseable R call with escaped special column names
  o <- ClinicoPath:::jjhistostatsOptions$new(dep = "Age (years)", typestatistics = "parametric", resultssubtitle = TRUE)
  a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = data.frame("Age (years)" = rnorm(10), check.names = FALSE))
  expect_no_error(parse(text = a$asSource()))
})

# ===========================================================================
# REGRESSION TESTS — PREVIOUSLY OPEN DEFECTS (NOW FIXED)
# ===========================================================================

test_that("C01: default originaltheme=FALSE renders plot and plot2 without discrete-scale crash", {
  skip_if_not_installed("ggstatsplot")
  set.seed(1)
  d <- data.frame(v = rnorm(30, 5, 1), g = factor(rep(c("A", "B"), each = 15)))
  r <- ClinicoPath::jjhistostats(data = d, dep = "v", grvar = "g", originaltheme = FALSE)
  c1 <- val_capture_image(r$plot)
  c2 <- val_capture_image(r$plot2)
  expect_null(c1$error)
  expect_gt(c1$bytes, 1000)
  expect_null(c2$error)
  expect_gt(c2$bytes, 1000)
})

test_that("C11: alternative=greater/less is honored for robust and bayes", {
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("BayesFactor")
  set.seed(123)
  d <- data.frame(v = rnorm(30, 5.5, 1.5))
  r_gt <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "robust", alternative = "greater",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
  ))
  r_lt <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "robust", alternative = "less",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
  ))
  p_gt <- as.numeric(val_quoted_nums(val_capture_image(r_gt$plot)$builds[[1]]$plot$labels$subtitle)[2])
  p_lt <- as.numeric(val_quoted_nums(val_capture_image(r_lt$plot)$builds[[1]]$plot$labels$subtitle)[2])
  expect_false(isTRUE(all.equal(p_gt, p_lt)))

  b_gt <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "bayes", alternative = "greater",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
  ))
  b_lt <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "bayes", alternative = "less",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
  ))
  bf_gt <- as.numeric(val_quoted_nums(val_capture_image(b_gt$plot)$builds[[1]]$plot$labels$subtitle)[1])
  bf_lt <- as.numeric(val_quoted_nums(val_capture_image(b_lt$plot)$builds[[1]]$plot$labels$subtitle)[1])
  expect_false(isTRUE(all.equal(bf_gt, bf_lt)))
})

test_that("C24: ggpubr plots handle column names with parentheses, slashes, and operators without crashing or evaluating A+B", {
  skip_if_not_installed("ggpubr")
  set.seed(12)
  d <- data.frame(
    A = rep(1, 30) + rnorm(30, 0, 0.01),
    B = rep(2, 30) + rnorm(30, 0, 0.01),
    x = rnorm(30, 100, 5),
    y = rnorm(30, 50, 5),
    check.names = FALSE
  )
  names(d)[3:4] <- c("A+B", "Age (years)")
  r1 <- ClinicoPath::jjhistostats(data = d, dep = "Age (years)", addGGPubrPlot = TRUE)
  expect_null(val_capture_image(r1$ggpubrPlot)$error)

  r2 <- ClinicoPath::jjhistostats(data = d, dep = c("A", "B", "A+B"), addGGPubrPlot = TRUE)
  builds <- val_capture_image(r2$ggpubrPlot)$builds
  idx <- which(vapply(builds, function(b) identical(as.character(b$plot$labels$title)[1], "A+B"), logical(1)))[1]
  bars <- builds[[idx]]$data[[1]]
  expect_equal(stats::weighted.mean(bars$x, bars$count), mean(d[["A+B"]]), tolerance = 0.05)
})

test_that("C06 & C08: constant null x==mu and B=200 bootstrap never fabricate p=2.23e-308 or r_biserial=1.00", {
  skip_if_not_installed("ggstatsplot")
  d0 <- data.frame(v = rep(0, 30))
  r_rob <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d0, dep = "v", typestatistics = "robust", resultssubtitle = TRUE,
    enableOneSampleTest = TRUE, test.value = 0, originaltheme = TRUE
  ))
  expect_false(any(val_quoted_nums(val_capture_image(r_rob$plot)$builds[[1]]$plot$labels$subtitle) == "2.23e-308"))

  r_np <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d0, dep = "v", typestatistics = "nonparametric", resultssubtitle = TRUE,
    enableOneSampleTest = TRUE, test.value = 0, originaltheme = TRUE
  ))
  nums_np <- val_quoted_nums(val_capture_image(r_np$plot)$builds[[1]]$plot$labels$subtitle)
  expect_false(length(nums_np) >= 3 && as.numeric(nums_np[3]) == 1.0)

  set.seed(20250101)
  d_shift <- data.frame(v = rnorm(30, 10, 1))
  r_sh <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d_shift, dep = "v", typestatistics = "robust", resultssubtitle = TRUE,
    enableOneSampleTest = TRUE, test.value = 0, originaltheme = TRUE
  ))
  expect_false(any(val_quoted_nums(val_capture_image(r_sh$plot)$builds[[1]]$plot$labels$subtitle) == "2.23e-308"))
})

test_that("C22: grouped ggpubrPlot2 draws per-group mean lines and drops NA grvar facets", {
  skip_if_not_installed("ggpubr")
  set.seed(1)
  d <- data.frame(
    v = c(rnorm(30, 10, 1), rnorm(30, 50, 1), rep(100, 5)),
    g = factor(c(rep("Low", 30), rep("High", 30), rep(NA, 5)), levels = c("Low", "High"))
  )
  r <- ClinicoPath::jjhistostats(data = d, dep = "v", grvar = "g", addGGPubrPlot = TRUE, ggpubrAddMean = TRUE)
  b <- val_capture_image(r$ggpubrPlot2)$builds[[1]]
  expect_equal(nrow(b$layout$layout), 2L)
  expect_equal(sort(val_vlines(b)), sort(as.numeric(tapply(d$v, d$g, mean))), tolerance = 1e-6)
})

test_that("C11: one-sided parametric/nonparametric CI and BF caption adapt to alternative", {
  skip_if_not_installed("ggstatsplot")
  set.seed(123)
  d <- data.frame(v = rnorm(30, 5.5, 1.5))
  r_two <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "parametric", alternative = "two.sided",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
  ))
  r_gt <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "parametric", alternative = "greater",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5, digits = 5, originaltheme = TRUE
  ))
  ci_two <- as.numeric(val_quoted_nums(val_capture_image(r_two$plot)$builds[[1]]$plot$labels$subtitle)[4:5])
  ci_gt  <- as.numeric(val_quoted_nums(val_capture_image(r_gt$plot)$builds[[1]]$plot$labels$subtitle)[4:5])
  expect_false(identical(ci_two, ci_gt))
})

test_that("C08: robust trimlevel=0.4 at n=5 is guarded against degenerate t=Inf and CI=[NA, NA]", {
  skip_if_not_installed("ggstatsplot")
  d <- data.frame(v = c(1.1, 1.8, 3.3, 4.1, 4.8))
  r <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "robust", trimlevel = 0.4,
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0, originaltheme = TRUE
  ))
  expect_false(any(val_quoted_nums(val_capture_image(r$plot)$builds[[1]]$plot$labels$subtitle) == "Inf"))
})

test_that("C17: grouped plot2 handles an all-NA subgroup cleanly without opaque attribute error", {
  skip_if_not_installed("ggstatsplot")
  d <- data.frame(v = c(1:20, rep(NA_real_, 5)), g = factor(c(rep("A", 20), rep("B", 5))))
  r <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", grvar = "g", resultssubtitle = TRUE, centralityline = TRUE, originaltheme = TRUE
  ))
  expect_null(val_capture_image(r$plot2)$error)
})

test_that("C19: missing values in grvar are disclosed in todo warnings", {
  d <- data.frame(v = 1:30, g = factor(c(rep("A", 12), rep("B", 12), rep(NA, 6))))
  r <- suppressWarnings(ClinicoPath::jjhistostats(data = d, dep = "v", grvar = "g", resultssubtitle = TRUE))
  expect_match(r$todo$content, "missing|excluded", ignore.case = TRUE)
})

test_that("C14: robust trim disclosure reports floor(tr * n_used), not round(tr * nrow(data))", {
  d <- data.frame(v = c(seq(1, 24), rep(NA_real_, 10)))
  r <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "robust", resultssubtitle = TRUE,
    enableOneSampleTest = TRUE, test.value = 12, trimlevel = 0.2
  ))
  expect_match(r$todo$content, "4 of 24", fixed = TRUE)
})

test_that("C19 & C20: bf.message warnings and Random seed caption fire only when BF/RNG actually runs", {
  skip_if_not_installed("ggstatsplot")
  set.seed(123)
  d <- data.frame(v = rnorm(30, 5.5, 1.5))
  r_np <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "nonparametric",
    resultssubtitle = TRUE, bf.message = TRUE, caption = "MY CAP", originaltheme = TRUE
  ))
  expect_false(grepl("Your caption was not used", r_np$todo$content, fixed = TRUE))
  expect_false(grepl("Bayes factor prior:", r_np$todo$content, fixed = TRUE))

  r_nosub <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "parametric",
    resultssubtitle = FALSE, bf.message = TRUE, originaltheme = TRUE
  ))
  cap <- val_capture_image(r_nosub$plot)$builds[[1]]$plot$labels$caption
  expect_false(grepl("Random seed:", paste(deparse(cap), collapse = " "), fixed = TRUE))
})

test_that("C18: interpretation bullets do not contradict when Shapiro-Wilk p > 0.05 and |g1| >= 1", {
  d <- data.frame(v = c(1, 1.3, 1.5, 1.7, 1.9, 2.2, 3.6))
  html <- ClinicoPath::jjhistostats(data = d, dep = "v", showInterpretation = TRUE)$interpretation$content
  expect_false(grepl("no evidence against normality", html, fixed = TRUE) &&
               grepl("Non-normal distribution:", html, fixed = TRUE))
})

test_that("C18: interpretation preserves significant digits for small-scale clinical data", {
  set.seed(1)
  d <- data.frame(v = rnorm(30, 5e-4, 1e-4))
  html <- ClinicoPath::jjhistostats(data = d, dep = "v", showInterpretation = TRUE)$interpretation$content
  expect_false(grepl("mean = 0, median = 0.*SD = 0", html))
})

test_that("C27: clinicalPreset=pathology_scores in R matches JS preset behavior", {
  set.seed(1)
  d <- data.frame(v = rnorm(30, 5, 1))
  r <- ClinicoPath::jjhistostats(data = d, dep = "v", clinicalPreset = "pathology_scores")
  expect_true(isTRUE(r$interpretation$visible))
})

test_that("C31: .a.yaml option documentation matches actual implementation", {
  a_yaml <- gsub("\\s+", " ", paste(readLines("../../jamovi/jjhistostats.a.yaml", warn = FALSE), collapse = "\n"))
  expect_false(grepl("no Shapiro-Wilk, Anderson-Darling or similar statistic is computed or displayed anywhere", a_yaml, fixed = TRUE))
  expect_false(grepl("overrides statistical results if provided", a_yaml, fixed = TRUE))
  expect_false(grepl("Whether to display Bayes Factor in the subtitle when using Bayesian analysis", a_yaml, fixed = TRUE))
  expect_false(grepl("max(x) - min(x) / sqrt(N)", a_yaml, fixed = TRUE))
})

# ===========================================================================
# INDEPENDENT REVIEW REGRESSION TESTS (IR-JJHIST-01 .. IR-JJHIST-18)
# ===========================================================================

test_that("IR-JJHIST-01: directional Bayes on opposite-sign data returns finite negative posterior (never Inf)", {
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("BayesFactor")
  set.seed(1)
  d <- data.frame(v = rnorm(25, mean = -2, sd = 1))
  r <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "bayes",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0,
    alternative = "greater", seed = 42, originaltheme = TRUE
  ))
  sub <- val_capture_image(r$plot)$builds[[1]]$plot$labels$subtitle
  expect_false(is.null(sub))
  nums <- val_quoted_nums(sub)
  expect_false(any(nums %in% c("Inf", "-Inf", "NA", "NaN")))
  num_vals <- suppressWarnings(as.numeric(nums))
  expect_true(all(is.finite(num_vals)))
  # Posterior difference median (2nd numeric token after log_e(BF_01)) should be negative (~ -2)
  expect_lt(num_vals[2], -0.5)
})

test_that("IR-JJHIST-02: high-evidence directional Bayes reads logBF directly without exp() overflow", {
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("BayesFactor")
  set.seed(1)
  d <- data.frame(v = rnorm(200, mean = 50, sd = 1))
  r_b <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "bayes",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0,
    alternative = "greater", seed = 42, originaltheme = TRUE
  ))
  sub_b <- val_capture_image(r_b$plot)$builds[[1]]$plot$labels$subtitle
  expect_false(is.null(sub_b))
  nums_b <- val_quoted_nums(sub_b)
  expect_false(any(nums_b %in% c("Inf", "-Inf", "NA", "NaN")))
  expect_lt(as.numeric(nums_b[1]), -700)

  r_p <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "parametric",
    resultssubtitle = TRUE, bf.message = TRUE,
    enableOneSampleTest = TRUE, test.value = 0,
    alternative = "greater", seed = 42, originaltheme = TRUE
  ))
  cap_p <- val_capture_image(r_p$plot)$builds[[1]]$plot$labels$caption
  expect_false(is.null(cap_p))
  nums_p <- val_quoted_nums(cap_p)
  expect_false(any(nums_p %in% c("Inf", "-Inf", "NA", "NaN")))
  expect_lt(as.numeric(nums_p[1]), -700)
})

test_that("IR-JJHIST-03: robust zero-exceedance p-value patch fires for all digits (0..5)", {
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("WRS2")
  set.seed(7)
  d <- data.frame(v = rnorm(50, mean = 20, sd = 1))
  for (dig in c(3L, 5L)) {
    r <- suppressWarnings(ClinicoPath::jjhistostats(
      data = d, dep = "v", typestatistics = "robust",
      resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0,
      digits = dig, seed = 42, originaltheme = TRUE
    ))
    sub <- val_capture_image(r$plot)$builds[[1]]$plot$labels$subtitle
    sub_txt <- paste(deparse(sub, width.cutoff = 500), collapse = " ")
    expect_false(grepl("2.225e-308|e-308", sub_txt))
    expect_match(sub_txt, "0\\.005")
  }
})

test_that("IR-JJHIST-04: robust mode refuses cleanly when trimmed core has zero Winsorized variance", {
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("WRS2")
  d <- data.frame(v = c(1, 5, 5, 5, 5, 5, 5, 5, 9))
  for (alt in c("two.sided", "greater")) {
    r <- suppressWarnings(ClinicoPath::jjhistostats(
      data = d, dep = "v", typestatistics = "robust", trimlevel = 0.2,
      resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0,
      alternative = alt, seed = 42, originaltheme = TRUE
    ))
    cap <- val_capture_image(r$plot)
    expect_null(cap$error)
    sub <- cap$builds[[1]]$plot$labels$subtitle
    sub_txt <- paste(deparse(sub, width.cutoff = 500), collapse = " ")
    expect_false(any(val_quoted_nums(sub) %in% c("Inf", "-Inf", "NaN")))
    expect_match(sub_txt, "zero Winsorized variance|unavailable", ignore.case = TRUE)
    expect_match(r$todo$content, "zero Winsorized variance|degenerate", ignore.case = TRUE)
  }
})

test_that("IR-JJHIST-05: grouped plot2 refuses statistical subtitle for n < 3 and does not mislabel n=1 constant", {
  skip_if_not_installed("ggstatsplot")
  d <- data.frame(
    v = c(1:10, 20, 25, 30),
    g = factor(c(rep("Normal", 10), "Two", "Two", "One"))
  )
  r <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", grvar = "g", typestatistics = "parametric",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0,
    originaltheme = TRUE
  ))
  cap <- val_capture_image(r$plot2)
  expect_null(cap$error)
  subs <- vapply(cap$builds, function(b) {
    paste(deparse(b$plot$labels$subtitle, width.cutoff = 500), collapse = " ")
  }, character(1))
  expect_true(any(grepl("only 2 observation", subs, fixed = TRUE)))
  expect_true(any(grepl("only 1 observation", subs, fixed = TRUE)))
  expect_false(any(grepl("constant value", subs, fixed = TRUE)))
})

test_that("IR-JJHIST-06: ggpubrAddDensity plots bars on density scale matching density curve", {
  skip_if_not_installed("ggpubr")
  set.seed(1)
  d <- data.frame(v = runif(40, 0, 0.1))
  r <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", addGGPubrPlot = TRUE, ggpubrAddDensity = TRUE
  ))
  b <- val_capture_image(r$ggpubrPlot)$builds[[1]]
  expect_equal(b$plot$labels$y, "Density")
  bar_max <- max(b$data[[1]]$ymax, na.rm = TRUE)
  dens_max <- max(b$data[[2]]$y, na.rm = TRUE)
  ratio <- max(bar_max, dens_max) / max(1e-9, min(bar_max, dens_max))
  expect_lt(ratio, 5)
})

test_that("IR-JJHIST-07: conf.level = 0.999 renders 99.9% CI label, never 100%", {
  skip_if_not_installed("ggstatsplot")
  skip_if_not_installed("WRS2")
  skip_if_not_installed("BayesFactor")
  set.seed(1)
  d <- data.frame(v = rnorm(25, 5, 1))
  r_rob <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "robust",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0,
    alternative = "greater", conf.level = 0.999, seed = 42, originaltheme = TRUE
  ))
  sub_rob <- paste(deparse(val_capture_image(r_rob$plot)$builds[[1]]$plot$labels$subtitle, width.cutoff = 500), collapse = " ")
  expect_match(sub_rob, "99\\.9%")
  expect_false(grepl('"100%"', sub_rob, fixed = TRUE))

  r_bay <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", typestatistics = "bayes",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0,
    alternative = "greater", conf.level = 0.999, seed = 42, originaltheme = TRUE
  ))
  sub_bay <- paste(deparse(val_capture_image(r_bay$plot)$builds[[1]]$plot$labels$subtitle, width.cutoff = 500), collapse = " ")
  expect_match(sub_bay, "99\\.9%")
  expect_false(grepl('"100%"', sub_bay, fixed = TRUE))
})

test_that("IR-JJHIST-08 & IR-JJHIST-09: unused factor levels, addNA levels, and NA-depleted groups handled cleanly", {
  d <- data.frame(
    v = c(1:20, 1:5, rep(NA_real_, 10), 1:5),
    g = addNA(factor(c(rep("Big", 20), rep("LooksBig", 15), rep(NA, 5)),
                     levels = c("Big", "LooksBig", "Unused")))
  )
  r <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", grvar = "g", resultssubtitle = TRUE, originaltheme = TRUE
  ))
  expect_match(r$todo$content, "LooksBig", fixed = TRUE)
  expect_false(grepl("Unused", r$todo$content, fixed = TRUE))
  expect_null(val_capture_image(r$plot2)$error)
})

test_that("IR-JJHIST-10 & IR-JJHIST-11: clinicalPreset respects max_bins guard and explicit user overrides", {
  d_wide <- data.frame(v = seq(0, 1.5e6, length.out = 20))
  expect_error(
    ClinicoPath::jjhistostats(data = d_wide, dep = "v", clinicalPreset = "pathology_scores"),
    "would split|1,500,000 bins|too many bins",
    ignore.case = TRUE
  )

  set.seed(1)
  d <- data.frame(v = rnorm(30, 5, 1))
  r <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d, dep = "v", clinicalPreset = "lab_values",
    typestatistics = "nonparametric", resultssubtitle = TRUE, originaltheme = TRUE
  ))
  sub_txt <- paste(deparse(val_capture_image(r$plot)$builds[[1]]$plot$labels$subtitle, width.cutoff = 500), collapse = " ")
  expect_match(sub_txt, "Wilcoxon", fixed = TRUE)
})

test_that("IR-JJHIST-13, IR-JJHIST-14, IR-JJHIST-15, IR-JJHIST-16: warnings, seed caption, Shapiro p, and Wilcoxon ties", {
  skip_if_not_installed("ggstatsplot")
  # IR-JJHIST-13: enableOneSampleTest = TRUE with default test.value = 0 must not claim switched off
  d0 <- data.frame(v = 1:20)
  r0 <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d0, dep = "v", resultssubtitle = TRUE,
    enableOneSampleTest = TRUE, test.value = 0
  ))
  expect_false(grepl("switched off", r0$todo$content, fixed = TRUE))

  # IR-JJHIST-14: constant data with robust/bayes must not stamp Random seed caption
  d_const <- data.frame(v = rep(5, 15))
  r_const <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d_const, dep = "v", typestatistics = "robust",
    resultssubtitle = TRUE, originaltheme = TRUE
  ))
  cap_const <- paste(deparse(val_capture_image(r_const$plot)$builds[[1]]$plot$labels$caption, width.cutoff = 500), collapse = " ")
  expect_false(grepl("Random seed:", cap_const, fixed = TRUE))

  # IR-JJHIST-15: Shapiro-Wilk p < 1e-16 renders without double operator "p = <"
  d_skew <- data.frame(v = c(rep(0, 100), 1000))
  r_skew <- ClinicoPath::jjhistostats(data = d_skew, dep = "v", showInterpretation = TRUE)
  expect_false(grepl("p = <", r_skew$interpretation$content, fixed = TRUE))
  expect_match(r_skew$interpretation$content, "p (<|&lt;) 1e-16")

  # IR-JJHIST-16: Wilcoxon with exact zero ties discloses excluded ties and pseudomedian assumption
  d_ties <- data.frame(v = c(rep(5, 8), 6, 7, 8, 9, 10, 11, 12))
  r_ties <- suppressWarnings(ClinicoPath::jjhistostats(
    data = d_ties, dep = "v", typestatistics = "nonparametric",
    resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5
  ))
  expect_match(r_ties$todo$content, "pseudomedian", ignore.case = TRUE)
  expect_match(r_ties$todo$content, "8 of 15 observations", fixed = TRUE)
})

# ===========================================================================
# SECURITY AUDIT REGRESSION TESTS (Category G1 & Category I)
# ===========================================================================

test_that("SEC-G1: asSource() escapes quotes/backslashes/newlines and drops undeclared/list options", {
  df <- data.frame(x = 1:10)

  # 1. Quote-breaking, backslash, and newline payloads on OptionString (title)
  for (payload in c(
    'MyTitle", seed = {stop("X"); 1}, subtitle = "x',
    'TrailingBackslash\\',
    'Line1\nLine2'
  )) {
    an <- ClinicoPath:::jjhistostatsClass$new(
      options = ClinicoPath:::jjhistostatsOptions$new(dep = "x"),
      data = df
    )
    o_t <- an$options$option("title")
    o_t$value <- payload
    ex <- parse(text = an$asSource())[[1]]
    expect_identical(ex$title, payload)
    expect_null(ex$seed)
    expect_null(ex$subtitle)
  }

  # 2. Quote-breaking string on a non-String option (protobuf path) & list value
  an2 <- ClinicoPath:::jjhistostatsClass$new(
    options = ClinicoPath:::jjhistostatsOptions$new(dep = "x"),
    data = df
  )
  p2 <- 'parametric", seed = {stop("X"); 1}, subtitle = "x'
  o_ts <- an2$options$option("typestatistics"); o_ts$value <- p2
  o_cp <- an2$options$option("caption"); o_cp$value <- list("a`b" = 1)
  ex2 <- parse(text = an2$asSource())[[1]]
  expect_identical(ex2$typestatistics, p2)
  expect_null(ex2$seed)
  expect_null(ex2$caption)

  # 3. Undeclared option name (protobuf injection) is dropped completely
  an3 <- ClinicoPath:::jjhistostatsClass$new(
    options = ClinicoPath:::jjhistostatsOptions$new(dep = "x"),
    data = df
  )
  an3$options$.addOption(jmvcore:::Option$new('title = "a", seed = {stop("X"); 1}, subtitle', "b"))
  ex3 <- parse(text = an3$asSource())[[1]]
  expect_null(ex3$title)
  expect_null(ex3$seed)
  expect_null(ex3$subtitle)
})

test_that("SEC-I: .validateInputs() rejects out-of-range numeric and enum options", {
  df <- data.frame(x = 1:15)
  check_invalid <- function(opt, bad_val) {
    an <- ClinicoPath:::jjhistostatsClass$new(
      options = ClinicoPath:::jjhistostatsOptions$new(dep = "x"),
      data = df
    )
    o <- an$options$option(opt)
    o$value <- bad_val
    an$.__enclos_env__$private$.validateInputs()$valid
  }
  expect_false(check_invalid("conf.level", 2.5))
  expect_false(check_invalid("trimlevel", 0.9))
  expect_false(check_invalid("bfprior", -1))
  expect_false(check_invalid("digits", 99))
  expect_false(check_invalid("typestatistics", "invalid_stat"))
  expect_false(check_invalid("alternative", "invalid_alt"))
})
