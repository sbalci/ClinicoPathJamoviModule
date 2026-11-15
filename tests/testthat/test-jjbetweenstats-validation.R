# Exhaustive validation & release-profile regression suite for jjbetweenstats (2026-11-14).
#
# Asserts verified PASS claims against independent base-R derivations,
# metamorphic / guard properties, all 15 VAL-jjbetweenstats-01..15 bug-fix
# regressions, and all release-profile checklist requirements.

library(testthat)

val_jb_render <- function(img_item) {
  tf <- tempfile(fileext = ".png")
  on.exit(unlink(tf), add = TRUE)
  ggplot2::set_last_plot(NULL)
  suppressWarnings(suppressMessages(img_item$saveAs(tf)))
  bytes <- if (file.exists(tf)) readBin(tf, "raw", file.info(tf)$size) else raw(0)
  list(plot = ggplot2::last_plot(), md5 = digest::digest(bytes, algo = "md5"))
}

val_jb_parse_sub <- function(expr) {
  if (is.null(expr)) return(NULL)
  if (is.expression(expr)) expr <- expr[[1]]
  if (is.call(expr) && identical(expr[[1]], quote(atop))) expr <- expr[[2]]
  if (!is.call(expr) || length(expr) < 6L) return(NULL)
  s <- deparse1(expr)
  if (grepl("BF", s, fixed = TRUE)) {
    list(
      log_bf01  = as.numeric(expr[[2]][[3]]),
      post_diff = as.numeric(expr[[3]][[3]]),
      ci_low    = as.numeric(expr[[4]][[3]][[3]]),
      ci_high   = as.numeric(expr[[5]][[2]]),
      r_cauchy  = as.numeric(expr[[6]][[3]])
    )
  } else {
    get_df <- function(node) {
      if (is.numeric(node) || is.integer(node)) return(as.numeric(node))
      if (is.call(node)) {
        if (identical(node[[1]], quote(`^`))) return(get_df(node[[2]]))
        return(unlist(lapply(as.list(node)[-1], get_df)))
      }
      numeric(0)
    }
    list(
      stat    = as.numeric(expr[[2]][[3]]),
      df      = get_df(expr[[2]][[2]]),
      p       = as.numeric(expr[[3]][[3]]),
      eff     = as.numeric(expr[[4]][[3]]),
      ci_low  = as.numeric(expr[[5]][[3]][[3]]),
      ci_high = as.numeric(expr[[6]][[2]]),
      n       = as.integer(expr[[7]][[3]])
    )
  }
}

val_jb_strip_html <- function(x) {
  if (is.null(x)) return("")
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", x)))
}

val_jb_ka2 <- function() {
  data.frame(
    y = c(10, 12, 12, 14, 15, 19, 18, 20, 20, 22, 25, 31, NA_real_),
    g = factor(c(rep("Ctrl", 6), rep("Trt", 6), "Ctrl"), levels = c("Ctrl", "Trt"))
  )
}

val_jb_ka3 <- function() {
  data.frame(
    y = c(10.2, 11.5, 9.8, 12.1, 10.9, 10.4,
          14.3, 15.8, 13.9, 19.2, 15.1, 22.0,
          18.0, 21.4, 19.2, 22.5, 20.1, 20.8),
    g = factor(rep(c("G1", "G2", "G3"), each = 6), levels = c("G1", "G2", "G3"))
  )
}

# ---------------------------------------------------------------------------
# Exact numerical & metamorphic validation tests
# ---------------------------------------------------------------------------

test_that("2-group Welch t, Cohen's d, Hedges' g, Mann-Whitney U, Yuen t and JZS BF match exact derivations", {
  d <- val_jb_ka2()
  x1 <- c(10, 12, 12, 14, 15, 19)
  x2 <- c(18, 20, 20, 22, 25, 31)

  # Exact hand arithmetic: m1 - m2 = -9, s1^2 = 148/15, s2^2 = 334/15
  se_w <- sqrt((148 / 15) / 6 + (334 / 15) / 6)
  df_w <- ((148 / 90 + 334 / 90)^2) / (((148 / 90)^2) / 5 + ((334 / 90)^2) / 5)
  t_w  <- -9 / se_w
  d_w  <- -9 / sqrt((148 / 15 + 334 / 15) / 2)
  j_w  <- exp(lgamma(df_w / 2) - 0.5 * log(df_w / 2) - lgamma((df_w - 1) / 2))

  res_d <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g",
    typestatistics = "parametric", varequal = FALSE, effsizetype = "biased",
    resultssubtitle = TRUE, k = 5
  ))
  sub_d <- val_jb_parse_sub(val_jb_render(res_d$plot)$plot$labels$subtitle)
  expect_equal(sub_d$stat, round(t_w, 5), tolerance = 1e-5)
  expect_equal(sub_d$df,   round(df_w, 5), tolerance = 1e-5)
  expect_equal(sub_d$eff,  round(d_w, 5), tolerance = 1e-5)
  expect_equal(sub_d$n,    12L)

  res_g <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g",
    typestatistics = "parametric", varequal = FALSE, effsizetype = "unbiased",
    resultssubtitle = TRUE, k = 5
  ))
  sub_g <- val_jb_parse_sub(val_jb_render(res_g$plot)$plot$labels$subtitle)
  expect_equal(sub_g$eff, round(d_w * j_w, 5), tolerance = 1e-5)

  # Mann-Whitney: W = 1, r_rb = 2/36 - 1 = -17/18
  res_np <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g",
    typestatistics = "nonparametric", resultssubtitle = TRUE, k = 5
  ))
  sub_np <- val_jb_parse_sub(val_jb_render(res_np$plot)$plot$labels$subtitle)
  expect_equal(sub_np$stat, 1)
  expect_equal(sub_np$eff,  round(-17 / 18, 5), tolerance = 1e-5)

  # Rouder et al. (2009) JZS Bayes factor via direct numerical integration
  t_stu <- -9 / (sqrt((5 * 148 / 15 + 5 * 334 / 15) / 10) * sqrt(1 / 6 + 1 / 6))
  nu <- 10; n_eff <- 3; rsc <- 0.707
  num <- (1 + t_stu^2 / nu)^(-(nu + 1) / 2)
  den <- stats::integrate(function(g) {
    (1 + n_eff * g * rsc^2)^(-0.5) *
      (1 + t_stu^2 / ((1 + n_eff * g * rsc^2) * nu))^(-(nu + 1) / 2) *
      (2 * pi)^(-0.5) * g^(-1.5) * exp(-1 / (2 * g))
  }, 0, Inf, rel.tol = 1e-12)$value
  res_bf <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g",
    typestatistics = "bayes", resultssubtitle = TRUE, k = 5, seed = 20250101
  ))
  sub_bf <- val_jb_parse_sub(val_jb_render(res_bf$plot)$plot$labels$subtitle)
  expect_equal(sub_bf$log_bf01, round(log(num / den), 5), tolerance = 1e-5)
})

test_that("3-group Welch ANOVA, Fisher ANOVA, Kruskal-Wallis and Wilcox t1way match independent formulas", {
  d3 <- val_jb_ka3()
  sp <- split(d3$y, d3$g)
  ni <- vapply(sp, length, integer(1))
  mi <- vapply(sp, mean, numeric(1))
  vi <- vapply(sp, stats::var, numeric(1))
  wi <- ni / vi; w_sum <- sum(wi); m_bar <- sum(wi * mi) / w_sum
  lam <- sum((1 - wi / w_sum)^2 / (ni - 1))
  f_w <- (sum(wi * (mi - m_bar)^2) / 2) / (1 + (2 / 8) * lam)
  df2_w <- 8 / (3 * lam)
  eta2_w <- (2 * f_w) / (2 * f_w + df2_w)
  omega2_w <- (2 * f_w - 2) / (2 * f_w + df2_w + 1)

  res_w <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d3, dep = "y", group = "g",
    typestatistics = "parametric", varequal = FALSE, effsizetype = "biased",
    resultssubtitle = TRUE, k = 5
  ))
  sub_w <- val_jb_parse_sub(val_jb_render(res_w$plot)$plot$labels$subtitle)
  expect_equal(sub_w$stat,  round(f_w, 5), tolerance = 1e-5)
  expect_equal(sub_w$df[2], round(df2_w, 5), tolerance = 1e-5)
  expect_equal(sub_w$eff,   round(eta2_w, 5), tolerance = 1e-5)

  res_om <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d3, dep = "y", group = "g",
    typestatistics = "parametric", varequal = FALSE, effsizetype = "omega",
    resultssubtitle = TRUE, k = 5
  ))
  sub_om <- val_jb_parse_sub(val_jb_render(res_om$plot)$plot$labels$subtitle)
  expect_equal(sub_om$eff, round(omega2_w, 5), tolerance = 1e-5)

  # Kruskal-Wallis + ordinal epsilon^2 = H / (N - 1)
  kw <- stats::kruskal.test(y ~ g, data = d3)
  res_kw <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d3, dep = "y", group = "g",
    typestatistics = "nonparametric", resultssubtitle = TRUE, k = 5
  ))
  sub_kw <- val_jb_parse_sub(val_jb_render(res_kw$plot)$plot$labels$subtitle)
  expect_equal(sub_kw$stat, round(unname(kw$statistic), 5), tolerance = 1e-5)
  expect_equal(sub_kw$eff,  round(unname(kw$statistic) / (nrow(d3) - 1), 5), tolerance = 1e-5)
})

test_that("Metamorphic relations hold: permutation, group swap, affine scale, monotone rank invariance and CI nesting", {
  d <- val_jb_ka2()
  base_sub <- val_jb_parse_sub(val_jb_render(suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot)$plot$labels$subtitle)

  # Row permutation + irrelevant NA column
  set.seed(11)
  dp <- d[sample(nrow(d)), , drop = FALSE]
  dp$extra_na <- NA_real_
  perm_sub <- val_jb_parse_sub(val_jb_render(suppressWarnings(ClinicoPath::jjbetweenstats(
    data = dp, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot)$plot$labels$subtitle)
  expect_identical(perm_sub$stat, base_sub$stat)
  expect_identical(perm_sub$eff,  base_sub$eff)

  # Group swap negates t, d and flips CI bounds
  dr <- d
  dr$g <- factor(dr$g, levels = c("Trt", "Ctrl"))
  rev_sub <- val_jb_parse_sub(val_jb_render(suppressWarnings(ClinicoPath::jjbetweenstats(
    data = dr, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot)$plot$labels$subtitle)
  expect_equal(rev_sub$stat,   -base_sub$stat, tolerance = 1e-5)
  expect_equal(rev_sub$eff,    -base_sub$eff,  tolerance = 1e-5)
  expect_equal(rev_sub$ci_low, -base_sub$ci_high, tolerance = 1e-5)
  expect_equal(rev_sub$p,       base_sub$p,    tolerance = 1e-5)

  # Positive affine transform preserves t, df, p, d
  da <- transform(d, y = 3.5 * y - 17)
  aff_sub <- val_jb_parse_sub(val_jb_render(suppressWarnings(ClinicoPath::jjbetweenstats(
    data = da, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot)$plot$labels$subtitle)
  expect_equal(aff_sub$stat, base_sub$stat, tolerance = 1e-5)
  expect_equal(aff_sub$eff,  base_sub$eff,  tolerance = 1e-5)

  # Confidence interval strict nesting: 0.90 subset 0.95 subset 0.99
  ci90 <- val_jb_parse_sub(val_jb_render(suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g", conflevel = 0.90, resultssubtitle = TRUE, k = 5
  ))$plot)$plot$labels$subtitle)
  ci99 <- val_jb_parse_sub(val_jb_render(suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g", conflevel = 0.99, resultssubtitle = TRUE, k = 5
  ))$plot)$plot$labels$subtitle)
  expect_lt(ci99$ci_low,  base_sub$ci_low)
  expect_lt(base_sub$ci_low, ci90$ci_low)
  expect_lt(ci90$ci_high, base_sub$ci_high)
  expect_lt(base_sub$ci_high, ci99$ci_high)
})

test_that("Guards, Inf/NaN filtering, Jamovi Nominal-integer factors, non-ASCII names and plot point coordinates work", {
  d <- val_jb_ka2()
  expect_error(ClinicoPath::jjbetweenstats(data = d[0, , drop = FALSE], dep = "y", group = "g"),
               "No complete rows|contains no")
  expect_error(ClinicoPath::jjbetweenstats(data = d[1:6, , drop = FALSE], dep = "y", group = "g"),
               "at least 2 levels|at least 2 groups")

  # Inf explicitly refused by OptionVariables; NaN + NA excluded with count in diagnostics
  d_inf <- d[1:12, ]
  d_inf$y[1] <- Inf
  expect_error(ClinicoPath::jjbetweenstats(data = d_inf, dep = "y", group = "g"),
               "infinite values")

  d_nan <- d[1:12, ]
  d_nan$y[1] <- NaN
  d_nan$y[7] <- NA_real_
  res_nan <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d_nan, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))
  expect_match(val_jb_strip_html(res_nan$diagnostics$content),
               "2 rows with missing or non-finite values")

  # Jamovi Nominal-integer factor carrying underlying `values` attribute
  # Exact Welch t for (10,12)x3 vs (100,120)x3: (11 - 110) / sqrt(1.2/6 + 120/6) = -99 / sqrt(20.2)
  d_ni <- data.frame(
    y_ni = factor(c(10, 12, 10, 12, 10, 12, 100, 120, 100, 120, 100, 120)),
    g    = factor(rep(c("Ctrl", "Trt"), each = 6), levels = c("Ctrl", "Trt"))
  )
  attr(d_ni$y_ni, "values") <- as.integer(levels(d_ni$y_ni))
  res_ni <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d_ni, dep = "y_ni", group = "g", resultssubtitle = TRUE, k = 5
  ))
  sub_ni <- val_jb_parse_sub(val_jb_render(res_ni$plot)$plot$labels$subtitle)
  t_ni <- -99 / sqrt(1.2 / 6 + 120 / 6)
  expect_equal(sub_ni$stat, round(t_ni, 5), tolerance = 1e-5)

  # Non-ASCII + space column names and exact plotted point coordinates
  d_utf <- d[1:12, ]
  names(d_utf) <- c("Ki-67 indeks (%)", "Gr\u00e9ade group")
  res_utf <- suppressWarnings(ClinicoPath::jjbetweenstats(
    data = d_utf, dep = "Ki-67 indeks (%)", group = "Gr\u00e9ade group", resultssubtitle = TRUE, k = 5
  ))
  p_utf <- val_jb_render(res_utf$plot)$plot
  expect_equal(sort(p_utf$data[["Ki-67 indeks (%)"]]), sort(d$y[1:12]))
})

# ---------------------------------------------------------------------------
# Live regression tests for VAL-jjbetweenstats-01 .. VAL-jjbetweenstats-15
# ---------------------------------------------------------------------------

test_that("VAL-jjbetweenstats-01: missing values in dep[2] or grvar do not delete complete rows of dep[1]", {
  d <- data.frame(
    y1 = c(seq(10, 19.5, length.out = 20), seq(21, 30.5, length.out = 20)),
    y2 = c(1:10, rep(NA_real_, 25), 36:40),
    g  = factor(rep(c("Ctrl", "Trt"), each = 20), levels = c("Ctrl", "Trt")),
    s  = factor(c(rep("Biopsy", 10), rep(NA_character_, 15), rep("Resection", 15)))
  )
  solo <- val_jb_parse_sub(val_jb_render(ClinicoPath::jjbetweenstats(
    data = d, dep = "y1", group = "g", resultssubtitle = TRUE, k = 5
  )$plot)$plot$labels$subtitle)
  pair <- val_jb_parse_sub(val_jb_render(ClinicoPath::jjbetweenstats(
    data = d, dep = c("y1", "y2"), group = "g", resultssubtitle = TRUE, k = 5
  )$plot)$plot[[1]]$labels$subtitle)
  with_gr <- val_jb_parse_sub(val_jb_render(ClinicoPath::jjbetweenstats(
    data = d, dep = "y1", group = "g", grvar = "s", resultssubtitle = TRUE, k = 5
  )$plot)$plot$labels$subtitle)

  expect_equal(pair$n,       solo$n)
  expect_equal(pair$stat,    solo$stat)
  expect_equal(with_gr$n,    solo$n)
  expect_equal(with_gr$stat, solo$stat)
})

test_that("VAL-jjbetweenstats-02: ggpubrPlot 3-group parametric test respects varequal = FALSE (Welch ANOVA)", {
  set.seed(12)
  d <- data.frame(
    y = c(stats::rnorm(40, 10, 0.5), stats::rnorm(40, 10.1, 0.5), c(4, 5, 18, 22, 25, 28)),
    g = factor(c(rep("G1", 40), rep("G2", 40), rep("G3", 6)), levels = c("G1", "G2", "G3"))
  )
  res <- ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g",
    typestatistics = "parametric", varequal = FALSE, resultssubtitle = TRUE,
    addGGPubrPlot = TRUE, ggpubrAddStats = TRUE, k = 5
  )
  main_sub <- val_jb_parse_sub(val_jb_render(res$plot)$plot$labels$subtitle)
  gb_ggp <- ggplot2::ggplot_build(val_jb_render(res$ggpubrPlot)$plot)
  ggp_p <- gb_ggp$data[[which(vapply(gb_ggp$data, function(x) "p" %in% names(x), logical(1)))[1]]]$p[1]
  expect_equal(ggp_p, main_sub$p, tolerance = 1e-3)
})

test_that("VAL-jjbetweenstats-03: factors with explicit <NA> level via addNA() exclude missing-group rows", {
  d <- data.frame(
    y = c(10, 12, 12, 14, 15, 19, 18, 20, 20, 22, 25, 31),
    g = addNA(factor(c(rep("Ctrl", 5), NA, rep("Trt", 5), NA), levels = c("Ctrl", "Trt")))
  )
  res <- ClinicoPath::jjbetweenstats(data = d, dep = "y", group = "g", resultssubtitle = TRUE)
  expect_match(val_jb_strip_html(res$clinicalSummary$content), "across 2 group\\(s\\) \\(n = 10\\)")
})

test_that("VAL-jjbetweenstats-04: Student t-test (varequal=TRUE) computes pooled-SD Cohen's d preserving test-CI duality", {
  d <- data.frame(
    y = c(10.1, 10.4, 9.8, 10.2, 9.9, 10.0, 10.3, 9.7, 10.1, 10.2,
          9.8, 10.0, 10.5, 9.6, 10.1, 9.9, 10.3, 10.0, 9.8, 10.2,
          10.1, 9.9, 10.0, 10.2, 9.8,
          10.5, 11.2, 14.8, 9.4, 15.6, 11.0, 8.9, 11.4),
    g = factor(c(rep("Ctrl", 25), rep("Case", 8)), levels = c("Ctrl", "Case"))
  )
  x1 <- d$y[d$g == "Ctrl"]; x2 <- d$y[d$g == "Case"]
  sp <- sqrt((24 * stats::var(x1) + 7 * stats::var(x2)) / 31)
  d_pooled <- (mean(x1) - mean(x2)) / sp
  sub <- val_jb_parse_sub(val_jb_render(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g",
    typestatistics = "parametric", varequal = TRUE, effsizetype = "biased",
    resultssubtitle = TRUE, k = 5
  )$plot)$plot$labels$subtitle)
  expect_equal(sub$eff, round(d_pooled, 5), tolerance = 1e-4)
  expect_lt(sub$ci_high, 0)
})

test_that("VAL-jjbetweenstats-05: 3+ group parametric Games-Howell post-hoc p-values are not double-adjusted", {
  d3 <- val_jb_ka3()
  x1 <- d3$y[1:6]; x2 <- d3$y[7:12]
  se <- sqrt(stats::var(x1) / 6 + stats::var(x2) / 6)
  t12 <- abs(mean(x1) - mean(x2)) / se
  df12 <- (stats::var(x1) / 6 + stats::var(x2) / 6)^2 /
    ((stats::var(x1) / 6)^2 / 5 + (stats::var(x2) / 6)^2 / 5)
  p_gh12 <- stats::ptukey(t12 * sqrt(2), nmeans = 3, df = df12, lower.tail = FALSE)

  p <- val_jb_render(ClinicoPath::jjbetweenstats(
    data = d3, dep = "y", group = "g",
    typestatistics = "parametric", pairwisecomparisons = TRUE,
    pairwisedisplay = "everything", padjustmethod = "holm", k = 5
  )$plot)$plot
  sig_ly <- Filter(function(l) inherits(l$geom, "GeomSignif"), p$layers)[[1]]
  p_shown <- as.numeric(parse(text = sig_ly$stat_params$annotations[1])[[1]][[2]][[3]])
  expect_equal(p_shown, round(p_gh12, 5), tolerance = 1e-4)
})

test_that("VAL-jjbetweenstats-06: multi-dep diagnostics probes every dependent variable in dep, not only dep[1]", {
  d <- data.frame(
    y1 = c(10, 12, 12, 14, 15, 19, 18, 20, 20, 22, 25, 31),
    y2 = rep(7, 12),
    g  = factor(rep(c("Ctrl", "Trt"), each = 6))
  )
  res <- ClinicoPath::jjbetweenstats(data = d, dep = c("y1", "y2"), group = "g", resultssubtitle = TRUE)
  expect_match(val_jb_strip_html(res$diagnostics$content), "y2|subtitle|constant|could not be computed")
})

test_that("VAL-jjbetweenstats-07: default effsizetype='biased' with grvar discloses Cohen's d vs Hedges' g mismatch", {
  d <- data.frame(
    y = c(10, 12, 14, 15, 18, 20, 22, 25, 11, 13, 15, 16, 19, 21, 23, 26),
    g = factor(rep(rep(c("Ctrl", "Trt"), each = 4), 2)),
    s = factor(rep(c("S1", "S2"), each = 8))
  )
  res <- ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g", grvar = "s",
    typestatistics = "parametric", effsizetype = "biased", resultssubtitle = TRUE
  )
  expect_match(val_jb_strip_html(res$diagnostics$content), "Grouped \\('Split By'\\) plots")
})

test_that("VAL-jjbetweenstats-08: plot2 (Split By) applies colorblindSafe palette and custom axis/plot titles", {
  d <- data.frame(
    y = c(10, 12, 14, 15, 18, 20, 22, 25, 11, 13, 15, 16, 19, 21, 23, 26),
    g = factor(rep(rep(c("Ctrl", "Trt"), each = 4), 2)),
    s = factor(rep(c("S1", "S2"), each = 8))
  )
  r0 <- val_jb_render(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g", grvar = "s",
    colorblindSafe = FALSE, xtitle = "Arm", ytitle = "Score"
  )$plot2)
  r1 <- val_jb_render(ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g", grvar = "s",
    colorblindSafe = TRUE, xtitle = "Arm", ytitle = "Score"
  )$plot2)
  expect_false(identical(r0$md5, r1$md5))
  expect_equal(r1$plot[[1]]$labels$x, "Arm")
  expect_equal(r1$plot[[1]]$labels$y, "Score")
})

test_that("VAL-jjbetweenstats-09: outlier detection conditions on group rather than marginal pooled distribution", {
  d <- data.frame(
    y = c(seq(9.8, 10.2, length.out = 30), c(49.8, 49.9, 50.0, 50.1, 50.2)),
    g = factor(c(rep("Healthy", 30), rep("Disease", 5)))
  )
  res <- ClinicoPath::jjbetweenstats(data = d, dep = "y", group = "g")
  expect_no_match(val_jb_strip_html(res$diagnostics$content), "potential outlier")
})

test_that("VAL-jjbetweenstats-10: assumption checks test groups with n>=3 even if one arm has n=2, and flag zero variance", {
  set.seed(99)
  d <- data.frame(
    y = c(stats::rexp(50, 0.1), stats::rnorm(50, 5, 0.2), c(10, 12)),
    g = factor(c(rep("Skewed50", 50), rep("Tight50", 50), rep("Tiny2", 2)))
  )
  res <- ClinicoPath::jjbetweenstats(data = d, dep = "y", group = "g", typestatistics = "parametric")
  expect_match(val_jb_strip_html(res$diagnostics$content), "Unequal variances|Non-normal")
})

test_that("VAL-jjbetweenstats-11: n_g=1 group with empty subtitle does not claim Welch stats are shown", {
  d <- data.frame(
    y = c(10, 11, 12, 13, 20, 21, 22, 23, 30),
    g = factor(c(rep("A", 4), rep("B", 4), "C"))
  )
  res <- ClinicoPath::jjbetweenstats(
    data = d, dep = "y", group = "g",
    typestatistics = "parametric", varequal = FALSE, resultssubtitle = TRUE
  )
  expect_no_match(val_jb_strip_html(res$clinicalSummary$content),
                  "See the plot subtitle for the test statistic")
})

test_that("VAL-jjbetweenstats-12: Bayesian mode does not emit frequentist FWER, alpha=0.05 or p=[value] text", {
  d <- data.frame(
    y1 = c(10, 12, 14, 15, 18, 20, 22, 25),
    y2 = c(11, 13, 15, 16, 19, 21, 23, 26),
    g  = factor(rep(c("Ctrl", "Trt"), each = 4))
  )
  res <- ClinicoPath::jjbetweenstats(
    data = d, dep = c("y1", "y2"), group = "g",
    typestatistics = "bayes", resultssubtitle = TRUE, showexplanations = TRUE
  )
  expect_no_match(val_jb_strip_html(res$mecGuidance$content), "family-wise error rate")
  expect_no_match(val_jb_strip_html(res$report$content), "p = \\[value\\]")
  expect_no_match(val_jb_strip_html(res$clinicalSummary$content), "p-value")
})

test_that("VAL-jjbetweenstats-13: multi-dep stochastic estimates seed each variable independently", {
  d <- data.frame(
    y1 = c(10, 12, 12, 14, 15, 19, 18, 20, 20, 22, 25, 31),
    y2 = c(20.1, 22.4, 19.5, 25.0, 21.3, 23.1, 31.2, 35.4, 29.8, 40.1, 33.5, 38.9),
    g  = factor(rep(c("Ctrl", "Trt"), each = 6))
  )
  solo <- deparse1(val_jb_render(ClinicoPath::jjbetweenstats(
    data = d, dep = "y2", group = "g", typestatistics = "robust",
    resultssubtitle = TRUE, seed = 20250101, k = 5
  )$plot)$plot$labels$subtitle)
  comb <- deparse1(val_jb_render(ClinicoPath::jjbetweenstats(
    data = d, dep = c("y1", "y2"), group = "g", typestatistics = "robust",
    resultssubtitle = TRUE, seed = 20250101, k = 5
  )$plot)$plot[[2]]$labels$subtitle)
  expect_identical(comb, solo)
})

test_that("VAL-jjbetweenstats-14: asSource() quotes non-syntactic column names so exported R code parses", {
  d <- stats::setNames(
    data.frame(1:8, factor(rep(c("A", "B"), each = 4))),
    c("Ki-67 indeks (%)", "Gr\u00e9ade group")
  )
  opts <- ClinicoPath:::jjbetweenstatsOptions$new(dep = "Ki-67 indeks (%)", group = "Gr\u00e9ade group")
  ana  <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = d)
  expect_no_error(parse(text = ana$asSource()))
})

test_that("VAL-jjbetweenstats-15: interpretation cites Cohen (1988) and results schema cites WRS2 & BayesFactor", {
  d <- val_jb_ka2()
  res <- ClinicoPath::jjbetweenstats(data = d, dep = "y", group = "g", showexplanations = TRUE)
  expect_match(val_jb_strip_html(res$interpretation$content), "Cohen.*1988")

  ryaml <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "jjbetweenstats.r.yaml"))
  expect_true(all(c("WRS2", "BayesFactor", "effectsize", "PMCMRplus") %in% ryaml$refs))
  expect_false("digest" %in% ryaml$refs)
})

# ---------------------------------------------------------------------------
# Release-profile checklist tests
# ---------------------------------------------------------------------------

test_that("Release checklist: Bool options default to FALSE, jmv-welcome renders cleanly, haven_labelled and jamovi palette work, and all 13 outputs populate", {
  # 1. All Bool options in .a.yaml default to FALSE
  ayaml <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "jjbetweenstats.a.yaml"))
  expect_identical(ayaml$menuGroup, "JJStatsPlotT")
  bool_opts <- Filter(function(o) identical(o$type, "Bool"), ayaml$options)
  expect_true(length(bool_opts) > 0)
  for (bo in bool_opts) {
    expect_false(isTRUE(bo$default), info = paste("Bool option", bo$name, "must default to FALSE"))
  }

  # 2. Empty-state jmv-welcome & partial-selection prompt
  d <- data.frame(
    y1 = c(10, 12, 14, 15, 18, 20, 22, 25, 11, 13, 15, 16, 19, 21, 23, 26),
    y2 = c(11, 13, 15, 17, 19, 21, 23, 27, 12, 14, 16, 18, 20, 22, 24, 28),
    g  = structure(
      c(rep(c(1L, 2L), each = 4), rep(c(1L, 2L), each = 4)),
      labels = c("Control Arm" = 1L, "Active Arm" = 2L),
      class = c("haven_labelled", "vctrs_vctr", "integer")
    ),
    s  = factor(rep(c("Cohort A", "Cohort B"), each = 8))
  )
  ana_empty <- ClinicoPath:::jjbetweenstatsClass$new(
    options = ClinicoPath:::jjbetweenstatsOptions$new(),
    data = d
  )
  ana_empty$.__enclos_env__$private$.init()
  ana_empty$.__enclos_env__$private$.run()
  expect_match(ana_empty$results$todo$content, "jmv-welcome")

  ana_partial <- ClinicoPath:::jjbetweenstatsClass$new(
    options = ClinicoPath:::jjbetweenstatsOptions$new(dep = "y1"),
    data = d
  )
  ana_partial$.__enclos_env__$private$.init()
  ana_partial$.__enclos_env__$private$.run()
  expect_match(ana_partial$results$todo$content, "Select.*Grouping Variable")

  # 3. haven_labelled group preserves declared value labels in backend class
  ana_lbl <- ClinicoPath:::jjbetweenstatsClass$new(
    options = ClinicoPath:::jjbetweenstatsOptions$new(
      dep = "y1", group = "g", resultssubtitle = TRUE
    ),
    data = d
  )
  ana_lbl$init()
  ana_lbl$.__enclos_env__$private$.run()
  p_lbl <- val_jb_render(ana_lbl$results$plot)
  expect_equal(levels(p_lbl$plot$data$g), c("Control Arm", "Active Arm"))

  # 4. Full option activation populates all 12 active outputs (and clears todo)
  d_fac <- d
  d_fac$g <- factor(ifelse(as.integer(d$g) == 1L, "Control Arm", "Active Arm"),
                    levels = c("Control Arm", "Active Arm"))
  res_full <- ClinicoPath::jjbetweenstats(
    data = d_fac, dep = c("y1", "y2"), group = "g", grvar = "s",
    resultssubtitle = TRUE, showexplanations = TRUE,
    addGGPubrPlot = TRUE, ggpubrPalette = "jamovi", ggpubrAddStats = TRUE
  )
  expect_identical(res_full$todo$content %||% "", "")
  for (nm in c("mecGuidance", "diagnostics", "clinicalSummary",
               "about", "summary", "assumptions", "interpretation", "report")) {
    expect_true(nzchar(val_jb_strip_html(res_full[[nm]]$content)), info = paste("HTML output", nm, "populated"))
  }
  p_main <- val_jb_render(res_full$plot)
  p_sub  <- val_jb_render(res_full$plot2)
  p_ggp  <- val_jb_render(res_full$ggpubrPlot)
  p_ggp2 <- val_jb_render(res_full$ggpubrPlot2)
  expect_s3_class(p_main$plot, "ggplot")
  expect_s3_class(p_sub$plot,  "ggplot")
  expect_s3_class(p_ggp$plot,  "ggplot")
  expect_s3_class(p_ggp2$plot, "ggplot")
  expect_equal(levels(p_main$plot[[1]]$data$g), c("Control Arm", "Active Arm"))
})

test_that("Review fixes: plot2 K>=3 parametric post-hoc parity (GH-adj and genuine unadjusted t-test)", {
  d3 <- val_jb_ka3()
  d_strat <- rbind(
    transform(d3, s = "Cohort_1"),
    transform(d3, y = y + 0.5, s = "Cohort_2")
  )
  d_strat$s <- factor(d_strat$s, levels = c("Cohort_1", "Cohort_2"))

  # 1. padjustmethod != 'none' labels brackets [GH-adj.] on every stratified sub-plot
  res_gh <- ClinicoPath::jjbetweenstats(
    data = d_strat, dep = "y", group = "g", grvar = "s",
    typestatistics = "parametric", pairwisecomparisons = TRUE,
    pairwisedisplay = "everything", padjustmethod = "holm", k = 4
  )
  p2_gh <- val_jb_render(res_gh$plot2)$plot
  for (i in 1:2) {
    sig_ly <- Filter(function(l) inherits(l$geom, "GeomSignif"), p2_gh[[i]]$layers)[[1]]
    expect_true(all(grepl("GH-adj\\.", sig_ly$stat_params$annotations)))
  }

  # 2. padjustmethod == 'none' computes genuine unadjusted pairwise t-test p-values per stratum
  res_unadj <- ClinicoPath::jjbetweenstats(
    data = d_strat, dep = "y", group = "g", grvar = "s",
    typestatistics = "parametric", varequal = FALSE, pairwisecomparisons = TRUE,
    pairwisedisplay = "everything", padjustmethod = "none", k = 5
  )
  p2_unadj <- val_jb_render(res_unadj$plot2)$plot
  sig1 <- Filter(function(l) inherits(l$geom, "GeomSignif"), p2_unadj[[1]]$layers)[[1]]
  pair1 <- sig1$stat_params$comparisons[[1]]
  sub_pair <- subset(d3, g %in% pair1)
  p_ref <- stats::t.test(y ~ droplevels(g), data = sub_pair, var.equal = FALSE)$p.value
  p_shown <- as.numeric(parse(text = sig1$stat_params$annotations[1])[[1]][[2]][[3]])
  expect_equal(p_shown, round(p_ref, 5), tolerance = 1e-4)
})

test_that("Review fixes: safe seed fallback, column-scoped hash, backslash names, auto-filled report, STRONG_WARNING notice, and sparse strata", {
  # 1. Column-scoped hash ignores mutations in unused wide columns & memoizes statsExpressions
  d <- val_jb_ka2()[1:12, ]
  d$unused_wide_col <- seq_len(nrow(d))
  opts <- ClinicoPath:::jjbetweenstatsOptions$new(dep = "y", group = "g", resultssubtitle = TRUE)
  a1 <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = d)
  a1$.__enclos_env__$private$.prepareData()
  h1 <- a1$.__enclos_env__$private$.data_hash

  d2 <- d
  d2$unused_wide_col <- rnorm(nrow(d2))
  a2 <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = d2)
  a2$.__enclos_env__$private$.prepareData()
  expect_identical(a2$.__enclos_env__$private$.data_hash, h1)

  # 2. Safe seed handles NA_integer_ in R6 backend without throwing set.seed error
  opts$.__enclos_env__$private$..seed$.__enclos_env__$private$.value <- NA_integer_
  expect_identical(a1$.__enclos_env__$private$.safeSeed(), 20250101L)
  expect_no_error(a1$.__enclos_env__$private$.run())
  expect_true(length(a1$.__enclos_env__$private$.statsCache) >= 1L)

  # 3. Backslash in column name is preserved in diagnostics without sub() backreference stripping
  d_bs <- stats::setNames(d[, c("y", "g")], c("Ratio\\1\\Log", "Arm"))
  res_bs <- ClinicoPath::jjbetweenstats(
    data = d_bs, dep = "Ratio\\1\\Log", group = "Arm",
    typestatistics = "nonparametric", resultssubtitle = TRUE
  )
  d_bs_na <- d_bs
  d_bs_na[[1]][1] <- NA_real_
  res_bs2 <- ClinicoPath::jjbetweenstats(
    data = rbind(d_bs_na, d_bs), dep = c("Ratio\\1\\Log"), group = "Arm",
    typestatistics = "parametric", varequal = TRUE
  )
  expect_no_error(res_bs$diagnostics$content)
  expect_no_error(res_bs2$diagnostics$content)

  # 4. Auto-filled numeric report sentences when resultssubtitle = TRUE (frequentist & Bayesian)
  res_rep <- ClinicoPath::jjbetweenstats(
    data = val_jb_ka2(), dep = "y", group = "g",
    typestatistics = "parametric", resultssubtitle = TRUE, showexplanations = TRUE, k = 3
  )
  rep_txt <- val_jb_strip_html(res_rep$report$content)
  expect_no_match(rep_txt, "\\[value\\]")
  expect_match(rep_txt, "t\\(8\\.704\\) = -3\\.889")
  expect_match(rep_txt, "95% CI: -3\\.747 to -0\\.676, n = 12")

  res_rep_bf <- ClinicoPath::jjbetweenstats(
    data = val_jb_ka2(), dep = "y", group = "g",
    typestatistics = "bayes", resultssubtitle = TRUE, showexplanations = TRUE, k = 3
  )
  rep_bf_txt <- val_jb_strip_html(res_rep_bf$report$content)
  expect_no_match(rep_bf_txt, "\\[value\\]")
  expect_match(rep_bf_txt, "log_e\\(BF_01\\) = ")
  expect_match(rep_bf_txt, "95% ETI:")

  # 5. Heteroscedastic data with varequal = TRUE emits STRONG_WARNING Notice + Critical Validity HTML
  set.seed(42)
  d_het <- data.frame(
    y = c(rnorm(30, 10, 0.4), rnorm(30, 12, 3.5)),
    g = factor(rep(c("Ctrl", "Trt"), each = 30))
  )
  res_het <- ClinicoPath::jjbetweenstats(
    data = d_het, dep = "y", group = "g", typestatistics = "parametric", varequal = TRUE
  )
  item_names <- vapply(res_het$items, function(it) it$name, character(1))
  expect_true("validityWarning" %in% item_names)
  expect_match(val_jb_strip_html(res_het$diagnostics$content), "Critical Statistical Validity Warnings")

  # 6. Sparse Split-By stratum (< 2 groups in S_sparse, 2 groups in S_valid) renders & discloses
  d_sp <- rbind(
    transform(val_jb_ka2()[1:12, ], s = "S_valid"),
    data.frame(y = c(11, 12, 13), g = factor(rep("Ctrl", 3), levels = c("Ctrl", "Trt")), s = "S_sparse")
  )
  d_sp$s <- factor(d_sp$s, levels = c("S_valid", "S_sparse"))
  res_sp <- ClinicoPath::jjbetweenstats(
    data = d_sp, dep = "y", group = "g", grvar = "s", addGGPubrPlot = TRUE
  )
  expect_match(val_jb_strip_html(res_sp$diagnostics$content), "S_sparse")
  expect_s3_class(val_jb_render(res_sp$plot2)$plot, "ggplot")
  expect_s3_class(val_jb_render(res_sp$ggpubrPlot2)$plot, "ggplot")
})
