# Exhaustive validation regression suite for ClinicoPath::jjdotplotstats (2026-11-16).
#
# Playbook: .claude/commands/validate-function.md (depth=exhaustive, tests=write)
# Live tests lock in all verified PASS claims (C01-C25) against independent base-R
# derivations; confirmed open defects (VAL-jjdotplotstats-01..15) carry the exact
# expected behaviour behind skip() guards so the suite stays green until fixed.

library(testthat)

val_jdp_render_png <- function(img_item) {
  tf <- tempfile(fileext = ".png")
  on.exit(unlink(tf), add = TRUE)
  ggplot2::set_last_plot(NULL)
  suppressWarnings(suppressMessages(img_item$saveAs(tf)))
  bytes <- if (file.exists(tf)) readBin(tf, "raw", file.info(tf)$size) else raw(0)
  list(plot = ggplot2::last_plot(), bytes = bytes, md5 = digest::digest(bytes, algo = "md5"))
}

val_jdp_parse_sub <- function(expr) {
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
      df      = get_df(node = expr[[2]][[2]]),
      p       = as.numeric(expr[[3]][[3]]),
      eff     = as.numeric(expr[[4]][[3]]),
      ci_low  = as.numeric(expr[[5]][[3]][[3]]),
      ci_high = as.numeric(expr[[6]][[2]]),
      n       = as.integer(expr[[7]][[3]])
    )
  }
}

val_jdp_strip_html <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " "))))
}

val_jdp_ka2 <- function() {
  data.frame(
    y = c(2, 4, 4, 6, 8, 12, 6, 10, 10, 14, 16, 20, NA_real_),
    g = factor(c(rep("Ctrl", 6), rep("Trt", 6), "Trt"), levels = c("Ctrl", "Trt"))
  )
}

val_jdp_ka3 <- function() {
  data.frame(
    y = c(8, 10, 12, 12, 14, 15, 17, 18, 20, 22, 26, 32),
    g = factor(rep(c("Low", "Mid", "High"), each = 4), levels = c("Low", "Mid", "High"))
  )
}

val_jdp_ka3_pw <- function() {
  data.frame(
    y = c(10.2, 11.5, 9.8, 12.1, 10.9, 10.4,
          14.3, 15.8, 13.9, 19.2, 15.1, 22.0,
          18.0, 21.4, 19.2, 22.5, 20.1, 20.8),
    g = factor(rep(c("G1", "G2", "G3"), each = 6), levels = c("G1", "G2", "G3"))
  )
}

# ==============================================================================
# LIVE NUMERICAL, METAMORPHIC & GUARD TESTS (PASS CLAIMS)
# ==============================================================================

# Oracle: Welch (1947, Biometrika 34:28-35) closed-form two-sample unequal-variance t-test
test_that("C01: 2-group Welch t-statistic, Satterthwaite df, two-sided p, and complete-case N match exact hand arithmetic", {
  d <- val_jdp_ka2()
  # Ctrl s1^2 = 64/5 = 12.8; Trt s2^2 = 376/15; SE = sqrt(284/45); m1 - m2 = -20/3
  se <- sqrt((64 / 5) / 6 + (376 / 15) / 6)
  df <- ((64 / 30 + 376 / 90)^2) / (((64 / 30)^2) / 5 + ((376 / 90)^2) / 5)
  t_exp <- (-20 / 3) / se
  p_exp <- 2 * stats::pt(-abs(t_exp), df = df)

  res <- suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d, dep = "y", group = "g", typestatistics = "parametric",
    resultssubtitle = TRUE, k = 5
  ))
  sub <- val_jdp_parse_sub(res$plot$state$subtitle)
  expect_equal(sub$stat, round(t_exp, 5), tolerance = 1e-5)
  expect_equal(sub$df,   round(df, 5),    tolerance = 1e-5)
  expect_equal(sub$p,    round(p_exp, 5), tolerance = 1e-5)
  expect_identical(sub$n, 12L)
})

# Oracle: Cohen (1988) unpooled average-SD d, Hedges (1981, J Educ Stat 6:107-128) exact J(df), and noncentral-t CI
test_that("C02: 2-group unpooled Cohen's d, Hedges' g, and exact noncentral-t CIs match closed-form formulas", {
  d <- val_jdp_ka2()
  v1 <- 64 / 5; v2 <- 376 / 15
  se <- sqrt(v1 / 6 + v2 / 6)
  df <- (v1 / 6 + v2 / 6)^2 / ((v1 / 6)^2 / 5 + (v2 / 6)^2 / 5)
  t_val <- (-20 / 3) / se
  sd_avg <- sqrt((v1 + v2) / 2)
  d_exp <- (-20 / 3) / sd_avg
  j_corr <- exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))
  ncp_lo <- stats::uniroot(function(l) suppressWarnings(stats::pt(t_val, df, ncp = l)) - 0.975, c(-50, 50), tol = 1e-12)$root
  ncp_hi <- stats::uniroot(function(l) suppressWarnings(stats::pt(t_val, df, ncp = l)) - 0.025, c(-50, 50), tol = 1e-12)$root
  d_ci <- c(ncp_lo, ncp_hi) * (se / sd_avg)

  sub_d <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d, dep = "y", group = "g", effsizetype = "biased", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  sub_g <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d, dep = "y", group = "g", effsizetype = "unbiased", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)

  expect_equal(sub_d$eff,    round(d_exp, 5), tolerance = 1e-5)
  expect_equal(sub_d$ci_low, round(d_ci[1], 5), tolerance = 1e-5)
  expect_equal(sub_d$ci_high,round(d_ci[2], 5), tolerance = 1e-5)
  expect_equal(sub_g$eff,    round(d_exp * j_corr, 5), tolerance = 1e-5)
})

# Oracle: Welch (1951, Biometrika 38:330-336) heteroscedastic one-way F and Friedman (1982) partial eta^2 / omega^2
test_that("C03 & C04: 3-group Welch ANOVA F, df, p, partial eta^2, and partial omega^2 match Welch (1951)", {
  d3 <- val_jdp_ka3()
  sp <- split(d3$y, d3$g)
  ni <- vapply(sp, length, integer(1))
  mi <- vapply(sp, mean, numeric(1))
  vi <- vapply(sp, stats::var, numeric(1))
  wi <- ni / vi; w_sum <- sum(wi); m_bar <- sum(wi * mi) / w_sum
  lam <- sum((1 - wi / w_sum)^2 / (ni - 1))
  f_exp <- (sum(wi * (mi - m_bar)^2) / 2) / (1 + (2 / 8) * lam)
  df2_exp <- 8 / (3 * lam)
  eta2_exp <- (2 * f_exp) / (2 * f_exp + df2_exp)
  omega2_exp <- (2 * f_exp - 2) / (2 * f_exp + df2_exp + 1)

  sub_eta <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d3, dep = "y", group = "g", effsizetype = "eta", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  sub_om <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d3, dep = "y", group = "g", effsizetype = "omega", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)

  expect_equal(sub_eta$stat,  round(f_exp, 5),      tolerance = 1e-5)
  expect_equal(sub_eta$df[2], round(df2_exp, 5),    tolerance = 1e-5)
  expect_equal(sub_eta$eff,   round(eta2_exp, 5),   tolerance = 1e-5)
  expect_equal(sub_om$eff,    round(omega2_exp, 5), tolerance = 1e-5)
})

# Oracle: Mann & Whitney (1947) U with tie-adjusted CC normal p, Kerby (2014) r_rb, Kruskal-Wallis (1952) H & Tomczak (2014) eps^2
test_that("C05 & C06: Mann-Whitney W, rank-biserial r, Kruskal-Wallis H, and ordinal epsilon^2 match exact rank arithmetic", {
  d2 <- val_jdp_ka2()
  d3 <- val_jdp_ka3()

  sub_mw <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d2, dep = "y", group = "g", typestatistics = "nonparametric", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  expect_equal(sub_mw$stat, 4.5)
  expect_equal(sub_mw$eff,  -0.75)

  sub_kw <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d3, dep = "y", group = "g", typestatistics = "nonparametric", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  expect_equal(sub_kw$stat, round(2816 / 285, 5), tolerance = 1e-5)
  expect_equal(sub_kw$eff,  round(256 / 285, 5),  tolerance = 1e-5)
})

# Oracle: Yuen (1974, Biometrika 61:165-170) trimmed t, Algina et al. (2005) AKP delta, and Wilcox (2017) t1way F
test_that("C07 & C08: Yuen 20%-trimmed t, AKP robust delta, and Wilcox heteroscedastic t1way F match base-R Winsorized formulas", {
  d2 <- val_jdp_ka2()
  x1 <- c(2, 4, 4, 6, 8, 12); x2 <- c(6, 10, 10, 14, 16, 20)
  wv <- function(x) { s <- sort(x); s[1] <- s[2]; s[6] <- s[5]; stats::var(s) }
  d1 <- 5 * wv(x1) / (4 * 3); d2_v <- 5 * wv(x2) / (4 * 3)
  t_yuen <- abs(mean(x1, trim = 0.2) - mean(x2, trim = 0.2)) / sqrt(d1 + d2_v)
  df_yuen <- (d1 + d2_v)^2 / (d1^2 / 3 + d2_v^2 / 3)

  sub_y <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d2, dep = "y", group = "g", typestatistics = "robust", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  expect_equal(sub_y$stat, round(t_yuen, 5),  tolerance = 1e-5)
  expect_equal(sub_y$df,   round(df_yuen, 5), tolerance = 1e-5)
})

# Oracle: Rouder et al. (2009, Psychon Bull Rev 16:225-237, eq. 1) 1D JZS Bayes-factor quadrature at r = 0.707
test_that("C09: 2-group Bayesian subtitle and parametric bfmessage=TRUE caption match Rouder JZS log_e(BF_01)", {
  d2 <- val_jdp_ka2()
  x1 <- c(2, 4, 4, 6, 8, 12); x2 <- c(6, 10, 10, 14, 16, 20)
  sp <- sqrt((5 * stats::var(x1) + 5 * stats::var(x2)) / 10)
  t_stu <- (mean(x1) - mean(x2)) / (sp * sqrt(1 / 6 + 1 / 6))
  nu <- 10; n_eff <- 3; rsc <- 0.707
  num <- (1 + t_stu^2 / nu)^(-(nu + 1) / 2)
  den <- stats::integrate(function(g) {
    (1 + n_eff * g * rsc^2)^(-0.5) *
      (1 + t_stu^2 / ((1 + n_eff * g * rsc^2) * nu))^(-(nu + 1) / 2) *
      (2 * pi)^(-0.5) * g^(-1.5) * exp(-1 / (2 * g))
  }, 0, Inf, rel.tol = 1e-12)$value
  expected_log_bf01 <- log(num / den)

  res_b <- suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d2, dep = "y", group = "g", typestatistics = "bayes",
    resultssubtitle = TRUE, seed = 42, k = 5
  ))
  sub_b <- val_jdp_parse_sub(val_jdp_render_png(res_b$plot)$plot$labels$subtitle)
  expect_equal(sub_b$log_bf01, round(expected_log_bf01, 5), tolerance = 1e-5)

  res_cap <- suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d2, dep = "y", group = "g", typestatistics = "parametric",
    resultssubtitle = TRUE, bfmessage = TRUE, seed = 42, k = 5
  ))
  cap_b <- val_jdp_parse_sub(res_cap$plot$state$caption)
  expect_equal(cap_b$log_bf01, round(expected_log_bf01, 5), tolerance = 1e-5)
})

# Oracle: Dunn (1964, Technometrics 6:241-252) tie-adjusted rank-sum z-test + stats::p.adjust()
test_that("C10: nonparametric Dunn pairwise comparisons match exact tie-adjusted z formula under BH and Bonferroni", {
  d3 <- val_jdp_ka3_pw()
  N <- nrow(d3); rks <- rank(d3$y, ties.method = "average")
  sp <- split(rks, d3$g); ni <- vapply(sp, length, integer(1)); mb <- vapply(sp, mean, numeric(1))
  ties <- table(d3$y); s2 <- N * (N + 1) / 12 - sum(ties^3 - ties) / (12 * (N - 1))
  z12 <- abs(mb[1] - mb[2]) / sqrt(s2 * (1 / ni[1] + 1 / ni[2]))
  z13 <- abs(mb[1] - mb[3]) / sqrt(s2 * (1 / ni[1] + 1 / ni[3]))
  z23 <- abs(mb[2] - mb[3]) / sqrt(s2 * (1 / ni[2] + 1 / ni[3]))
  p_bh <- stats::p.adjust(2 * stats::pnorm(-c(z12, z13, z23)), method = "BH")

  p <- val_jdp_render_png(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d3, dep = "y", group = "g", typestatistics = "nonparametric",
    pairwisedisplay = "all", padjustmethod = "fdr", k = 5
  ))$plot)$plot
  sig_ly <- Filter(function(l) inherits(l$geom, "GeomSignif"), p$layers)[[1]]
  p_shown <- vapply(as.character(sig_ly$stat_params$annotations),
                    function(a) as.numeric(parse(text = a)[[1]][[2]][[3]]), numeric(1))
  expect_equal(unname(p_shown), unname(round(p_bh, 5)), tolerance = 1e-5)
})

# Oracle: exact group arithmetic mean, median, 20% trimmed mean, Sheather-Jones KDE MAP, and CoordFlip reference line
test_that("C15 & C16: centrality coordinates, boxplot medians, flipped axes, and visual-only reference line are exact", {
  d2 <- val_jdp_ka2()
  res <- suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d2, dep = "y", group = "g",
    centralityplotting = TRUE, centralitytype = "robust",
    testvalueline = TRUE, testvalue = 9.25,
    mytitle = "Main Title", xtitle = "Values (ng/mL)", ytitle = "Cohort",
    resultssubtitle = TRUE, k = 5
  ))
  p <- val_jdp_render_png(res$plot)$plot
  expect_true(inherits(p$coordinates, "CoordFlip"))
  expect_identical(c(p$labels$title, p$labels$y, p$labels$x),
                   c("Main Title", "Values (ng/mL)", "Cohort"))

  cent_ly <- Filter(function(l) is.data.frame(l$data) && "expression" %in% names(l$data), p$layers)[[1]]$data
  expect_equal(cent_ly$y[cent_ly$g == "Ctrl"], 5.5, tolerance = 1e-12)
  expect_equal(cent_ly$y[cent_ly$g == "Trt"], 12.5, tolerance = 1e-12)

  gb <- ggplot2::ggplot_build(p)
  hl_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))[1]
  expect_equal(gb$data[[hl_idx]]$yintercept[1], 9.25)
})

# Oracle: boundary guards at N < 30 (29 vs 30), min_group_n < 10 (9 vs 10), 0 rows, 1 group, and constant dep
test_that("C17: sample-size and degeneracy guards fire at exact thresholds in severity order", {
  d29 <- data.frame(y = seq_len(29), g = factor(c(rep("A", 14), rep("B", 15))))
  d30 <- data.frame(y = seq_len(30), g = factor(rep(c("A", "B"), each = 15)))
  expect_match(val_jdp_strip_html(ClinicoPath::jjdotplotstats(d29, "y", "g")$notices$content), "Small total sample size")
  expect_no_match(val_jdp_strip_html(ClinicoPath::jjdotplotstats(d30, "y", "g")$notices$content), "Small total sample size")

  d_const <- data.frame(y = rep(7, 12), g = factor(rep(c("A", "B"), each = 6)))
  raw_c <- ClinicoPath::jjdotplotstats(d_const, "y", "g")$notices$content
  sevs <- gsub(".*'", "", regmatches(raw_c, gregexpr("data-severity='[A-Z_]+", raw_c))[[1]])
  expect_identical(sevs[1], "ERROR")
})

# Oracle: algebraic invariances (row permutation, selective NA omission, level reversal, monotone & affine transforms, CI nesting)
test_that("C20: metamorphic invariants hold across row order, unused NA columns, level reversal, affine scaling, and conflevel", {
  d2 <- val_jdp_ka2()
  base_sub <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d2, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)

  set.seed(11)
  d_perm <- d2[sample(nrow(d2)), ]
  d_perm$extra_na <- NA_real_
  perm_sub <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d_perm, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  expect_identical(perm_sub$stat, base_sub$stat)
  expect_identical(perm_sub$n,    base_sub$n)

  d_rev <- d2; d_rev$g <- factor(d_rev$g, levels = c("Trt", "Ctrl"))
  rev_sub <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d_rev, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  expect_equal(rev_sub$stat,   -base_sub$stat)
  expect_equal(rev_sub$eff,    -base_sub$eff)
  expect_equal(rev_sub$ci_low, -base_sub$ci_high)

  c80 <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(d2, "y", "g", conflevel = 0.80, resultssubtitle = TRUE, k = 5))$plot$state$subtitle)
  c99 <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(d2, "y", "g", conflevel = 0.99, resultssubtitle = TRUE, k = 5))$plot$state$subtitle)
  expect_lt(c99$ci_low, base_sub$ci_low)
  expect_lt(base_sub$ci_low, c80$ci_low)
  expect_gt(c99$ci_high, base_sub$ci_high)
})

# Oracle: jmvcore::toNumeric() values-attribute unwrapping, UTF-8 names, seed preservation, asSource(), and .omv reopen
test_that("C21, C22, C23 & C24: values-attributed Nominal factors, UTF-8 names, RNG isolation, asSource(), and state-only reopen work", {
  d2 <- val_jdp_ka2()
  f_nom <- factor(c(2, 4, 4, 6, 8, 12, 6, 10, 10, 14, 16, 20))
  attr(f_nom, "values") <- as.integer(levels(f_nom))
  d_nom <- data.frame(y = f_nom, g = factor(rep(c("Ctrl", "Trt"), each = 6), levels = c("Ctrl", "Trt")))
  sub_nom <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d_nom, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  sub_ref <- val_jdp_parse_sub(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d2, dep = "y", group = "g", resultssubtitle = TRUE, k = 5
  ))$plot$state$subtitle)
  expect_equal(sub_nom$stat, sub_ref$stat)

  set.seed(321); r_before <- stats::runif(3)
  set.seed(321)
  res_utf8 <- suppressWarnings(ClinicoPath::jjdotplotstats(
    data = stats::setNames(d_nom, c("Ki-67 (%) Gr\u00e9ade", "T\u00fcm\u00f6r Grubu")),
    dep = "Ki-67 (%) Gr\u00e9ade", group = "T\u00fcm\u00f6r Grubu",
    typestatistics = "robust", resultssubtitle = TRUE, seed = 42, k = 5
  ))
  r_after <- stats::runif(3)
  expect_identical(r_before, r_after)

  # State-only .omv reopen without calling .run()
  res_bf <- suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d2, dep = "y", group = "g", resultssubtitle = TRUE, bfmessage = TRUE, seed = 42, k = 5
  ))
  opts_ro <- ClinicoPath:::jjdotplotstatsOptions$new(
    dep = "y", group = "g", resultssubtitle = TRUE, bfmessage = TRUE, seed = 42, k = 5
  )
  ana_ro <- ClinicoPath:::jjdotplotstatsClass$new(options = opts_ro, data = d2)
  ana_ro$init()
  ana_ro$results$plot$setState(res_bf$plot$state)
  im_ro <- val_jdp_render_png(ana_ro$results$plot)
  expect_false(is.null(im_ro$plot$labels$subtitle))
  expect_false(is.null(im_ro$plot$labels$caption))
})

# ==============================================================================
# REGRESSION TESTS FOR ADJUDICATED DEFECTS (VAL-jjdotplotstats-01..15)
# ==============================================================================

# Oracle: Rouder et al. (2012, J Math Psychol 56:356-374) JZS one-way ANOVA BF & Gelman et al. (2019) Bayesian R^2
test_that("C09 & C19 (VAL-jjdotplotstats-01): 3-group Bayesian mode renders JZS ANOVA subtitle and accurate prose", {
  d3 <- val_jdp_ka3_pw()
  res <- ClinicoPath::jjdotplotstats(
    data = d3, dep = "y", group = "g", typestatistics = "bayes",
    resultssubtitle = TRUE, showexplanation = TRUE, pairwisedisplay = "significant", k = 5
  )
  p <- val_jdp_render_png(res$plot)$plot
  sub_b3 <- val_jdp_parse_sub(p$labels$subtitle)
  exp_txt <- val_jdp_strip_html(res$explanation$content)
  expect_false(is.null(p$labels$subtitle))
  expect_true(is.finite(sub_b3$log_bf01))
  expect_gte(sub_b3$ci_low, 0)
  expect_lte(sub_b3$ci_high, 1)
  expect_false(grepl("posterior median difference", exp_txt, fixed = TRUE))
  expect_false(grepl("Holm adjustment", exp_txt, fixed = TRUE))
})

# Oracle: optional Split By (grvar) must not listwise-delete complete (dep, group) patients from unsplit plot
test_that("C11 (VAL-jjdotplotstats-02): missing values in optional grvar do not alter unsplit plot N or Welch t", {
  set.seed(42)
  d <- data.frame(
    v = c(stats::rnorm(20, 10, 2), stats::rnorm(20, 14, 3)),
    g = factor(rep(c("Ctrl", "Trt"), each = 20), levels = c("Ctrl", "Trt")),
    s = factor(c(rep("S1", 10), rep(NA_character_, 15), rep("S2", 15)))
  )
  solo <- val_jdp_parse_sub(ClinicoPath::jjdotplotstats(d, "v", "g", resultssubtitle = TRUE, k = 5)$plot$state$subtitle)
  grna <- val_jdp_parse_sub(ClinicoPath::jjdotplotstats(d, "v", "g", grvar = "s", resultssubtitle = TRUE, k = 5)$plot$state$subtitle)
  expect_identical(grna$n, solo$n)
  expect_equal(grna$stat, solo$stat)
})

# Oracle: missing group values encoded via addNA() must be excluded, never plotted and tested as a third cohort
test_that("C12 (VAL-jjdotplotstats-03): explicit <NA> factor levels via addNA() are excluded rather than analysed as a group", {
  d2 <- val_jdp_ka2()[1:12, ]
  d_na <- rbind(d2, data.frame(y = c(50, 55), g = NA))
  d_na$g <- addNA(d_na$g)
  res <- ClinicoPath::jjdotplotstats(d_na, "y", "g", resultssubtitle = TRUE, k = 5)
  expect_match(val_jdp_strip_html(res$notices$content), "Comparing 2 groups with N = 12")
})

# Oracle: Inf exclusion must precede input validation & missing-value counting so 1-group Inf datasets fail cleanly and Inf != NA
test_that("C13 (VAL-jjdotplotstats-04): Inf values do not double-count as missing NAs or bypass <2-group and small-N guards", {
  d3 <- val_jdp_ka3(); d3$y[1] <- Inf
  todo_txt <- val_jdp_strip_html(ClinicoPath::jjdotplotstats(d3, "y", "g")$todo$content)
  expect_no_match(todo_txt, "Rows excluded for missing values")

  d_1g <- data.frame(y = c(stats::rnorm(20, 10, 1), rep(Inf, 20)), g = factor(rep(c("A", "B"), each = 20)))
  res_1g <- ClinicoPath::jjdotplotstats(d_1g, "y", "g")
  expect_false(isTRUE(res_1g$plot$state$valid))
  expect_match(val_jdp_strip_html(res_1g$notices$content), "At least two groups")
})

# Oracle: Games & Howell (1976, J Educ Stat 1:113-125) studentized-range ptukey already controls FWER across all pairs
test_that("C10 (VAL-jjdotplotstats-05): 3+ group parametric Games-Howell pairwise p-values are not double-adjusted and padjustmethod='none' uses Welch t", {
  d3 <- val_jdp_ka3_pw()
  x1 <- d3$y[1:6]; x2 <- d3$y[7:12]
  se <- sqrt(stats::var(x1) / 6 + stats::var(x2) / 6)
  t12 <- abs(mean(x1) - mean(x2)) / se
  df12 <- (stats::var(x1) / 6 + stats::var(x2) / 6)^2 / ((stats::var(x1) / 6)^2 / 5 + (stats::var(x2) / 6)^2 / 5)
  p_gh12 <- stats::ptukey(t12 * sqrt(2), nmeans = 3, df = df12, lower.tail = FALSE)
  p_w12  <- 2 * stats::pt(-t12, df = df12)

  p <- val_jdp_render_png(ClinicoPath::jjdotplotstats(
    data = d3, dep = "y", group = "g", typestatistics = "parametric",
    pairwisedisplay = "all", padjustmethod = "holm", k = 5
  )$plot)$plot
  sig_ly <- Filter(function(l) inherits(l$geom, "GeomSignif"), p$layers)[[1]]
  p_shown <- as.numeric(parse(text = sig_ly$stat_params$annotations[1])[[1]][[2]][[3]])
  expect_equal(p_shown, round(p_gh12, 5), tolerance = 1e-4)
  expect_match(sig_ly$stat_params$annotations[1], "GH-adj.", fixed = TRUE)

  p_none <- val_jdp_render_png(suppressWarnings(ClinicoPath::jjdotplotstats(
    data = d3, dep = "y", group = "g", typestatistics = "parametric",
    pairwisedisplay = "all", padjustmethod = "none", k = 5
  ))$plot)$plot
  sig_none <- Filter(function(l) inherits(l$geom, "GeomSignif"), p_none$layers)[[1]]
  p_shown_none <- as.numeric(parse(text = sig_none$stat_params$annotations[1])[[1]][[2]][[3]])
  expect_equal(p_shown_none, round(p_w12, 5), tolerance = 1e-4)
})

# Oracle: Wilcox & Tian (2011, J Appl Stat 38:1359-1368) explanatory effect size xi = sqrt(var_explained) lies in [0, 1]
test_that("C08 (VAL-jjdotplotstats-06): robust 3-group Wilcox-Tian xi and its 95% CI upper bound do not exceed 1.0", {
  set.seed(42); invisible(stats::rnorm(40))
  d3 <- data.frame(
    y = c(stats::rnorm(20, 10, 2), stats::rnorm(20, 14, 3), stats::rnorm(20, 18, 4)),
    g = factor(rep(c("G1", "G2", "G3"), each = 20))
  )
  sub <- val_jdp_parse_sub(ClinicoPath::jjdotplotstats(
    data = d3, dep = "y", group = "g", typestatistics = "robust", resultssubtitle = TRUE, seed = 42, k = 5
  )$plot$state$subtitle)
  expect_lte(sub$eff, 1.0)
  expect_lte(sub$ci_high, 1.0)
})

# Oracle: 2-group comparisons have only 1 pair (0 pairwise brackets drawn), so neither brackets nor multiplicity warnings apply
test_that("C10 & C19 (VAL-jjdotplotstats-07): 2-group runs do not claim pairwise brackets in explanation or warn on padjustmethod='none'", {
  d2 <- val_jdp_ka2()
  res <- ClinicoPath::jjdotplotstats(d2, "y", "g", padjustmethod = "none", showexplanation = TRUE)
  expect_no_match(val_jdp_strip_html(res$notices$content), "without any adjustment for multiple testing")
  expect_no_match(val_jdp_strip_html(res$explanation$content), "shown as brackets")
})

# Oracle: singleton group (n_B = 1) has no defined group variance, so fallback notice must not claim plot shows default stats
test_that("C17 (VAL-jjdotplotstats-08): singleton group n_B=1 does not falsely claim the plot shows package-default test statistics", {
  set.seed(42)
  d_n1 <- data.frame(y = c(stats::rnorm(20, 10, 2), 15), g = factor(c(rep("A", 20), "B")))
  res <- ClinicoPath::jjdotplotstats(d_n1, "y", "g", resultssubtitle = TRUE)
  not_txt <- val_jdp_strip_html(res$notices$content)
  expect_no_match(not_txt, "shows the statistics package default instead")
  expect_match(not_txt, "Statistical test results could not be computed")
})

# Oracle: Split By (plot2) must validate each stratum so 1-group or constant-dep strata are disclosed
test_that("C14 (VAL-jjdotplotstats-09): Split By warns when an individual grvar stratum has fewer than 2 groups or constant dep", {
  d <- data.frame(
    y = c(2, 4, 4, 6, 8, 12, 6, 10, 10, 14, 16, 20, rep(5, 12)),
    g = factor(c(rep("Ctrl", 6), rep("Trt", 6), rep("Ctrl", 12)), levels = c("Ctrl", "Trt")),
    s = factor(rep(c("ValidSplit", "OneGroupConstSplit"), each = 12), levels = c("ValidSplit", "OneGroupConstSplit"))
  )
  res <- ClinicoPath::jjdotplotstats(d, "y", "g", grvar = "s", resultssubtitle = TRUE)
  expect_match(val_jdp_strip_html(res$notices$content), "OneGroupConstSplit")
})

# Oracle: Tukey (1977) 1.5*IQR outlier rule in a between-groups comparison applies within groups, not on pooled marginal data
test_that("C18 (VAL-jjdotplotstats-10): .detectOutliers screens within each group rather than flagging separated clean groups", {
  d <- data.frame(
    y = c(seq(9.8, 10.2, length.out = 30), c(49.8, 49.9, 50.0, 50.1, 50.2)),
    g = factor(c(rep("Healthy", 30), rep("Disease", 5)))
  )
  expect_no_match(val_jdp_strip_html(ClinicoPath::jjdotplotstats(d, "y", "g")$todo$content), "Potential outliers detected")
})

# Oracle: null data (p = 1) must not claim groups differ, and Bayesian guidance must name the plotted log_e(BF_01) scale
test_that("C19 (VAL-jjdotplotstats-11): explanation avoids claiming difference at p=1 and states log_e(BF_01) scale & direction", {
  d_null <- data.frame(y = c(10, 12, 14, 16, 10, 12, 14, 16), g = factor(rep(c("A", "B"), each = 4)))
  exp_null <- val_jdp_strip_html(ClinicoPath::jjdotplotstats(d_null, "y", "g", showexplanation = TRUE)$explanation$content)
  expect_no_match(exp_null, "tells you that the groups differ")

  exp_b <- val_jdp_strip_html(ClinicoPath::jjdotplotstats(d_null, "y", "g", typestatistics = "bayes", showexplanation = TRUE)$explanation$content)
  expect_match(exp_b, "log_e\\(BF_01\\)")
})

# Oracle: .a.yaml / .Rd documentation must match Welch unequal-variance execution and Bayesian MCMC seed dependence
test_that("C01 & C22 (VAL-jjdotplotstats-12): .a.yaml accurately documents Welch unequal variances and Bayesian seed dependence", {
  ayaml_path <- testthat::test_path("..", "..", "jamovi", "jjdotplotstats.a.yaml")
  skip_if_not(file.exists(ayaml_path))
  txt <- gsub("\\s+", " ", paste(readLines(ayaml_path, warn = FALSE), collapse = " "))
  expect_no_match(txt, "assumes normal distribution and equal variances", fixed = TRUE)
  expect_no_match(txt, "no effect on the parametric or Bayesian tests", fixed = TRUE)
})

# Oracle: notices in grvar + nonparametric + effsizetype='omega' must not contradict one another
test_that("C14 (VAL-jjdotplotstats-13): Split By + nonparametric + omega does not emit contradictory effect-size notices", {
  d <- data.frame(
    y = c(2, 4, 4, 6, 8, 12, 6, 10, 10, 14, 16, 20, 4, 6, 6, 8, 10, 14, 9, 13, 13, 17, 19, 23),
    g = factor(rep(rep(c("Ctrl", "Trt"), each = 6), 2), levels = c("Ctrl", "Trt")),
    s = factor(rep(c("S1", "S2"), each = 12))
  )
  not_txt <- val_jdp_strip_html(ClinicoPath::jjdotplotstats(
    data = d, dep = "y", group = "g", grvar = "s",
    typestatistics = "nonparametric", effsizetype = "omega", resultssubtitle = TRUE
  )$notices$content)
  expect_false(grepl("your selection was not used", not_txt, fixed = TRUE) &&
                 grepl("honours your selection", not_txt, fixed = TRUE))
})

# Oracle: menuGroup routing & sibling submodule ../jjstatsplot sync gate
test_that("C23 (VAL-jjdotplotstats-14): menuGroup is parked in JJStatsPlotT or synced identically with ../jjstatsplot", {
  skip_if_not_installed("yaml")
  ayaml <- testthat::test_path("..", "..", "jamovi", "jjdotplotstats.a.yaml")
  umb   <- testthat::test_path("..", "..", "R", "jjdotplotstats.b.R")
  sub   <- testthat::test_path("..", "..", "..", "jjstatsplot", "R", "jjdotplotstats.b.R")
  skip_if_not(file.exists(ayaml) && file.exists(umb))
  mg <- yaml::read_yaml(ayaml)$menuGroup
  if (grepl("[DPT]$", mg)) {
    expect_identical(mg, "JJStatsPlotT")
  } else if (file.exists(sub)) {
    expect_identical(digest::digest(file = sub, algo = "md5"), digest::digest(file = umb, algo = "md5"))
  }
})

# Oracle: declared bibliography keys in jjdotplotstats.r.yaml resolve in 00refs.yaml and credit statistical engines
test_that("C25: declared bibliography refs resolve in jamovi/00refs.yaml", {
  skip_if_not_installed("yaml")
  ryaml <- testthat::test_path("..", "..", "jamovi", "jjdotplotstats.r.yaml")
  refs0 <- testthat::test_path("..", "..", "jamovi", "00refs.yaml")
  skip_if_not(file.exists(ryaml) && file.exists(refs0))
  r_keys <- yaml::read_yaml(ryaml)$refs
  all_keys <- names(yaml::read_yaml(refs0)$refs)
  expect_true(all(r_keys %in% all_keys))
})

# Oracle: jjdotplotstats.r.yaml refs should credit WRS2, BayesFactor, effectsize, and PMCMRplus
test_that("C25 (VAL-jjdotplotstats-15): jjdotplotstats.r.yaml credits primary statistical engines", {
  skip_if_not_installed("yaml")
  ryaml <- testthat::test_path("..", "..", "jamovi", "jjdotplotstats.r.yaml")
  skip_if_not(file.exists(ryaml))
  r_keys <- yaml::read_yaml(ryaml)$refs
  expect_true(all(c("WRS2", "BayesFactor", "effectsize", "PMCMRplus") %in% r_keys))
  expect_false("digest" %in% r_keys)
})
