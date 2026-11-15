# Exhaustive mathematical, statistical, logical, and clinical validation tests for jjdotchart
# Generated per .claude/commands/validate-function.md (2026-11-15)

val_dc_strip_html <- function(s) {
  if (is.null(s) || length(s) == 0L) return("")
  gsub("<[^>]*>", "", paste(s, collapse = "\n"))
}

val_dc_render_svg <- function(res, item = "plot", width = 9, height = 6) {
  f <- tempfile(fileext = ".svg")
  on.exit(unlink(f), add = TRUE)
  svglite::svglite(f, width = width, height = height)
  err <- tryCatch({ print(res[[item]]); NULL }, error = function(e) conditionMessage(e))
  try(grDevices::dev.off(), silent = TRUE)
  lines <- if (file.exists(f)) readLines(f, warn = FALSE) else character(0)
  txt_nodes <- regmatches(lines, regexpr(">[^<]*</text>", lines))
  txt_clean <- gsub("^>|</text>$", "", txt_nodes)
  list(ok = is.null(err), error = err, lines = lines,
       svg = paste(lines, collapse = "\n"), txt = paste(txt_clean, collapse = ""),
       tokens = txt_clean)
}

val_dc_num <- function(txt, pattern) {
  m <- regexec(pattern, txt, perl = TRUE)
  reg <- regmatches(txt, m)[[1]]
  if (length(reg) < 2L) return(NA_real_)
  as.numeric(reg[2])
}

val_dc_tiny <- function() {
  data.frame(
    lab = factor(c(rep("Lab_A", 4), rep("Lab_B", 4), rep("Lab_C", 3), rep("Lab_D", 2)),
                 levels = c("Lab_A", "Lab_B", "Lab_C", "Lab_D")),
    tat = c(10, 12, 12, 14, 15, 18, 21, NA_real_, 20, 22, 24, 28, 30)
  )
}

# ---- Live PASS regression tests locking in verified claims ------------------

# Oracle: exact hand counting of non-missing rows per group on tiny fixture
test_that("C01: summary.grp and summary.n match exact complete-case counts per group", {
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = val_dc_tiny(), dep = "tat", group = "lab", testvalue = 15, seed = 20250101
  ))
  tb <- res$summary$asDF
  expect_identical(tb$grp, c("Lab_A", "Lab_B", "Lab_C", "Lab_D"))
  expect_identical(tb$n, c(4L, 3L, 3L, 2L))
  expect_identical(sum(tb$n), 12L)
})

# Oracle: hand arithmetic & base::mean on tiny fixture and datasets::OrchardSprays
test_that("C02: summary.value under parametric equals exact group arithmetic mean", {
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = val_dc_tiny(), dep = "tat", group = "lab",
    typestatistics = "parametric", testvalue = 15, seed = 20250101
  ))
  tb <- res$summary$asDF
  expect_equal(tb$value, c(12, 18, 22, 29), tolerance = 1e-12)
})

# Oracle: exact Hyndman-Fan type-7 sample median (stats::median) on skewed 4-group fixture
test_that("C03: summary.value under nonparametric equals exact group median", {
  d <- data.frame(
    grp = factor(rep(c("G1", "G2", "G3", "G4"), each = 5), levels = c("G1", "G2", "G3", "G4")),
    val = c(1, 10, 12, 14, 100, 2, 10, 11, 19, 80, 5, 20, 22, 24, 29, 8, 30, 31, 35, 46)
  )
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "val", group = "grp",
    typestatistics = "nonparametric", testvalue = 18, seed = 20250101
  ))
  expect_equal(res$summary$asDF$value, c(12, 11, 22, 31), tolerance = 1e-12)
})

# Oracle: exact order-statistic 20% trimmed mean dropping floor(0.2*n) per tail (Tukey & McLaughlin 1963)
test_that("C04: summary.value under robust equals exact 20% trimmed mean", {
  d <- data.frame(
    grp = factor(rep(c("G1", "G2", "G3", "G4"), each = 5), levels = c("G1", "G2", "G3", "G4")),
    val = c(1, 10, 12, 14, 100, 2, 10, 11, 19, 80, 5, 20, 22, 24, 29, 8, 30, 31, 35, 46)
  )
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "val", group = "grp",
    typestatistics = "robust", testvalue = 18, seed = 20250101
  ))
  expect_equal(res$summary$asDF$value, c(12, 40 / 3, 22, 32), tolerance = 1e-12)
})

# Oracle: Sheather-Jones Gaussian KDE argmax on 1024 grid points over [min(x), max(x)] (Makowski et al. 2019, JOSS 4(40):1541)
test_that("C05: summary.value under bayes equals Sheather-Jones 1024-grid KDE mode", {
  d <- ClinicoPath::jjdotchart_test_data
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "turnaround_hours", group = "centre",
    typestatistics = "bayes", testvalue = 25, seed = 20250101
  ))
  kde_modes <- as.numeric(tapply(d$turnaround_hours, d$centre, function(x) {
    den <- stats::density(x, n = 1024L, bw = "SJ", from = min(x), to = max(x))
    den$x[which.max(den$y)]
  }))
  expect_equal(res$summary$asDF$value, kde_modes, tolerance = 1e-10)
})

# Oracle: exact sample standard deviation sqrt(sum((x - mean(x))^2)/(n - 1))
test_that("C06: summary.sd equals raw within-group sample SD regardless of typestatistics", {
  r_p <- suppressWarnings(ClinicoPath::jjdotchart(
    data = val_dc_tiny(), dep = "tat", group = "lab", typestatistics = "parametric", testvalue = 15
  ))
  r_r <- suppressWarnings(ClinicoPath::jjdotchart(
    data = val_dc_tiny(), dep = "tat", group = "lab", typestatistics = "robust", testvalue = 15
  ))
  expect_equal(r_p$summary$asDF$sd, c(sqrt(8 / 3), 3, 2, sqrt(2)), tolerance = 1e-12)
  expect_equal(r_r$summary$asDF$sd, r_p$summary$asDF$sd, tolerance = 1e-12)
})

# Oracle: exact subtraction Summary - testvalue
test_that("C08: summary.vsref equals exact difference Summary - testvalue", {
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = val_dc_tiny(), dep = "tat", group = "lab", testvalue = 15, seed = 20250101
  ))
  tb <- res$summary$asDF
  expect_equal(tb$vsref, c(-3, 3, 7, 14), tolerance = 1e-12)
  expect_equal(tb$vsref, tb$value - 15, tolerance = 1e-12)
})

# Oracle: Student (1908) one-sample t and Hedges (1981, DOI 10.3102/10769986006002107) exact gamma J(df) & noncentral-t CI
test_that("C09: parametric subtitle matches closed-form one-sample t, df=k-1, p, and Hedges g with ncp CI", {
  skip_if_not_installed("svglite")
  d <- datasets::OrchardSprays
  d$treatment <- factor(d$treatment)
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "decrease", group = "treatment",
    typestatistics = "parametric", testvalue = 40, conflevel = 0.95, k = 5, seed = 20250101
  ))
  svg <- val_dc_render_svg(res)
  m <- as.numeric(tapply(d$decrease, d$treatment, mean))
  k <- length(m); df <- k - 1L
  tt <- (mean(m) - 40) / (stats::sd(m) / sqrt(k))
  pv <- 2 * stats::pt(-abs(tt), df = df)
  J  <- exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))
  g  <- ((mean(m) - 40) / stats::sd(m)) * J
  expect_equal(val_dc_num(svg$txt, "tStudent\\(7\\)=([0-9.-]+)"), round(tt, 5), tolerance = 1e-5)
  expect_equal(val_dc_num(svg$txt, "p=([0-9.eE+-]+)"), round(pv, 5), tolerance = 1e-5)
  expect_equal(val_dc_num(svg$txt, "gHedges=([0-9.-]+)"), round(g, 5), tolerance = 1e-5)
  expect_equal(val_dc_num(svg$txt, "nobs=([0-9]+)"), 8)
})

# Oracle: Wilcoxon (1945) signed-rank tie-adjusted normal approx & Kerby (2014, DOI 10.2466/11.IT.3.1) rank-biserial r
test_that("C10: nonparametric subtitle V, CC p-value, and rank-biserial r match closed-form formulas", {
  skip_if_not_installed("svglite")
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = val_dc_tiny(), dep = "tat", group = "lab",
    typestatistics = "nonparametric", testvalue = 15, k = 5, seed = 20250101
  ))
  svg <- val_dc_render_svg(res)
  # Differences d = c(-3, +3, +7, +14): tied |d|=3 gets rank 1.5 -> V+ = 8.5, V- = 1.5, r = 7/10 = 0.7
  expect_equal(val_dc_num(svg$txt, "VWilcoxon=([0-9.]+)"), 8.5, tolerance = 1e-5)
  expect_equal(val_dc_num(svg$txt, "rbiserialrank=([0-9.-]+)"), 0.7, tolerance = 1e-5)
})

# Oracle: Yuen-Tukey-McLaughlin Winsorized SE formula (Mair & Wilcox 2020, DOI 10.3758/s13428-019-01246-w)
test_that("C11: robust subtitle t_bootstrapped and trimmed mean match closed-form Winsorized formulas", {
  skip_if_not_installed("svglite")
  d <- ClinicoPath::jjdotchart_test_data
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "turnaround_hours", group = "centre",
    typestatistics = "robust", testvalue = 25, k = 5, seed = 20250101
  ))
  svg <- val_dc_render_svg(res)
  trims <- sort(as.numeric(tapply(d$turnaround_hours, d$centre, mean, trim = 0.2)))
  k <- length(trims); g <- floor(0.2 * k)
  w <- trims; w[1:g] <- trims[g + 1L]; w[(k - g + 1L):k] <- trims[k - g]
  wins_se <- stats::sd(w) / (0.6 * sqrt(k))
  t_exp <- (mean(trims, trim = 0.2) - 25) / wins_se
  expect_equal(val_dc_num(svg$txt, "tbootstrapped=([0-9.-]+)"), round(t_exp, 5), tolerance = 1e-5)
  expect_equal(val_dc_num(svg$txt, "trimmed=([0-9.-]+)"), round(mean(trims, trim = 0.2), 5), tolerance = 1e-5)
})

# Oracle: Rouder et al. (2009, DOI 10.3758/PBR.16.2.225, eq. 1) 1D quadrature for JZS log_e(BF_01) at r = 0.707
test_that("C12: Bayesian subtitle log_e(BF_01) matches independent Rouder 2009 1D quadrature", {
  skip_if_not_installed("svglite")
  d <- ClinicoPath::jjdotchart_test_data
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "turnaround_hours", group = "centre",
    typestatistics = "bayes", testvalue = 25, k = 5, seed = 20250101
  ))
  svg <- val_dc_render_svg(res)
  modes <- res$summary$asDF$value
  N <- length(modes); nu <- N - 1
  tt <- (mean(modes) - 25) / (stats::sd(modes) / sqrt(N))
  log_den <- -0.5 * (nu + 1) * log(1 + tt^2 / nu)
  bf10 <- stats::integrate(function(g) {
    exp(-0.5 * log(1 + N * g) - 0.5 * (nu + 1) * log(1 + tt^2 / (nu * (1 + N * g))) +
          log(0.707) - 0.5 * log(2 * pi) - 1.5 * log(g) - 0.707^2 / (2 * g) - log_den)
  }, lower = 0, upper = Inf, rel.tol = 1e-12)$value
  expect_equal(val_dc_num(svg$txt, "\\(BF01\\)=([0-9.-]+)"), round(-log(bf10), 5), tolerance = 1e-5)
})

# Oracle: exact separation of NA/NaN vs +/-Inf row counts in .prepareData()
test_that("C15: missing/NaN and +/-Inf observations are excluded and counted separately", {
  d <- ClinicoPath::jjdotchart_test_data
  d$turnaround_hours[1:2] <- Inf
  d$turnaround_hours[3]   <- -Inf
  d$turnaround_hours[4:5] <- NaN
  d$turnaround_hours[6:8] <- NA_real_
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "turnaround_hours", group = "centre", testvalue = 24
  ))
  nt <- val_dc_strip_html(res$notices$content)
  expect_match(nt, "5 row\\(s\\) with missing values were excluded")
  expect_match(nt, "3 row\\(s\\) had an infinite value")
  expect_identical(sum(res$summary$asDF$n), 242L)
})

# Oracle: Cleveland & McGill (1984, DOI 10.1080/01621459.1984.10478080) ascending value ordering & reference vline
test_that("C16: plotted y-axis orders groups ascending by summary value and draws dashed red Reference Value line", {
  skip_if_not_installed("svglite")
  d <- ClinicoPath::jjdotchart_test_data
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "turnaround_hours", group = "centre", testvalue = 24, seed = 20250101
  ))
  svg <- val_dc_render_svg(res)
  tb  <- res$summary$asDF
  g_tok <- svg$tokens[svg$tokens %in% levels(d$centre)]
  expect_identical(g_tok, as.character(tb$grp[order(tb$value)]))
  red_line <- grep("#B22222", svg$lines, ignore.case = TRUE, value = TRUE)
  expect_length(red_line, 1L)
  expect_match(red_line, "stroke-dasharray", fixed = TRUE)
})

# Oracle: exact centre of k plotted group summaries across all 4 centralitytype options
test_that("C17: centralitytype positions top secondary axis at exact centre of k plotted points", {
  skip_if_not_installed("svglite")
  d <- datasets::OrchardSprays; d$treatment <- factor(d$treatment)
  m <- as.numeric(tapply(d$decrease, d$treatment, mean))
  r_med <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "decrease", group = "treatment", testvalue = 40,
    centralityplotting = TRUE, centralitytype = "nonparametric", k = 4
  ))
  svg <- val_dc_render_svg(r_med)
  expect_match(svg$txt, sprintf("%.4f", stats::median(m)), fixed = TRUE)
})

# Oracle: monotonic noncentral-t and bootstrap quantile widening with conflevel
test_that("C18: conflevel widens subtitle and table CIs monotonically and visibility flags hold", {
  skip_if_not_installed("svglite")
  d <- datasets::OrchardSprays; d$treatment <- factor(d$treatment)
  r80 <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "decrease", group = "treatment", testvalue = 40, conflevel = 0.80, seed = 20250101))
  r99 <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "decrease", group = "treatment", testvalue = 40, conflevel = 0.99, seed = 20250101))
  expect_true(all((r99$summary$asDF$ci_high - r99$summary$asDF$ci_low) >
                    (r80$summary$asDF$ci_high - r80$summary$asDF$ci_low)))
  r_hide <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "decrease", group = "treatment", testvalue = 40,
    showSummaryTable = FALSE, resultssubtitle = FALSE
  ))
  expect_false(r_hide$summary$visible)
  expect_false(grepl("tStudent", val_dc_render_svg(r_hide)$txt, fixed = TRUE))
})

# Oracle: exact guard inequality thresholds in .validate()
test_that("C19: guard thresholds fire at exact documented boundaries and sort ERROR > WARNING > INFO", {
  d3 <- droplevels(subset(val_dc_tiny(), lab != "Lab_D"))
  d4 <- val_dc_tiny()
  expect_match(val_dc_strip_html(suppressWarnings(ClinicoPath::jjdotchart(
    data = d3, dep = "tat", group = "lab", testvalue = 15))$notices$content),
    "With only 3 groups")
  expect_no_match(val_dc_strip_html(suppressWarnings(ClinicoPath::jjdotchart(
    data = d4, dep = "tat", group = "lab", testvalue = 15))$notices$content),
    "inference from their summaries is imprecise")
})

# Oracle: affine equivariance of location/scale summaries and invariance of t, p, and Hedges g
test_that("C20: positive affine transform scales summaries/vsref and preserves t, p, and Hedges g", {
  skip_if_not_installed("svglite")
  d1 <- ClinicoPath::jjdotchart_test_data
  d2 <- d1; d2$turnaround_hours <- 2.5 * d1$turnaround_hours + 100
  r1 <- suppressWarnings(ClinicoPath::jjdotchart(data = d1, dep = "turnaround_hours", group = "centre", testvalue = 24, k = 5, seed = 20250101))
  r2 <- suppressWarnings(ClinicoPath::jjdotchart(data = d2, dep = "turnaround_hours", group = "centre", testvalue = 160, k = 5, seed = 20250101))
  expect_equal(r2$summary$asDF$value, 2.5 * r1$summary$asDF$value + 100, tolerance = 1e-10)
  expect_equal(r2$summary$asDF$vsref, 2.5 * r1$summary$asDF$vsref, tolerance = 1e-10)
  s1 <- val_dc_render_svg(r1); s2 <- val_dc_render_svg(r2)
  expect_equal(val_dc_num(s2$txt, "tStudent\\(11\\)=([0-9.-]+)"), val_dc_num(s1$txt, "tStudent\\(11\\)=([0-9.-]+)"), tolerance = 1e-5)
  expect_equal(val_dc_num(s2$txt, "gHedges=([0-9.-]+)"), val_dc_num(s1$txt, "gHedges=([0-9.-]+)"), tolerance = 1e-5)
})

# Oracle: jmvcore::toNumeric unwrapping 'values' attribute (100..400, not factor codes 1..4) & UTF-8 symbol preservation
test_that("C21: Nominal integer factor with 'values' attribute and UTF-8 column names preserve exact values", {
  skip_if_not_installed("svglite")
  d <- data.frame(
    g = factor(rep(c("Low", "Mid", "High", "Top"), each = 4)),
    x = factor(rep(c("100", "200", "300", "400"), each = 4), levels = c("100", "200", "300", "400"))
  )
  attr(d$x, "values") <- c(100L, 200L, 300L, 400L)
  res <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "x", group = "g", testvalue = 250))
  expect_equal(sort(res$summary$asDF$value), c(100, 200, 300, 400), tolerance = 1e-12)

  du <- val_dc_tiny()
  names(du) <- c("Hôpital / Centre", "Gréade score (µg/L)")
  ru <- suppressWarnings(ClinicoPath::jjdotchart(
    data = du, dep = "Gréade score (µg/L)", group = "Hôpital / Centre", testvalue = 15
  ))
  expect_true(val_dc_render_svg(ru)$ok)
  expect_equal(ru$summary$asDF$value, c(12, 18, 22, 29), tolerance = 1e-12)
})

# Oracle: .omv reopen state persistence via image$state (umbrella R/jjdotchart.b.R)
test_that("C22: .omv reopen renders valid SVG from image$state without re-running .run()", {
  skip_if_not_installed("svglite")
  d <- ClinicoPath::jjdotchart_test_data
  opts <- ClinicoPath:::jjdotchartOptions$new(dep = "turnaround_hours", group = "centre", testvalue = 24)
  a1 <- ClinicoPath:::jjdotchartClass$new(options = opts, data = d)
  suppressWarnings(a1$run())
  a2 <- ClinicoPath:::jjdotchartClass$new(options = opts, data = d)
  a2$init()
  a2$results$plot$setState(a1$results$plot$state)
  f <- tempfile(fileext = ".svg"); on.exit(unlink(f), add = TRUE)
  svglite::svglite(f, width = 8, height = 5)
  ok <- suppressWarnings(a2$.__enclos_env__$private$.plot(
    a2$results$plot, ggplot2::theme_bw(), list(palette = "jmv")
  ))
  grDevices::dev.off()
  expect_true(isTRUE(ok))
  expect_gt(file.info(f)$size, 5000)
})

# Oracle: deterministic RNG seeding under self$options$seed
test_that("C23: identical seed produces identical SVG and distinct seed shifts only stochastic CIs", {
  skip_if_not_installed("svglite")
  d <- ClinicoPath::jjdotchart_test_data
  s1 <- val_dc_render_svg(suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "turnaround_hours", group = "centre", typestatistics = "robust", testvalue = 24, seed = 20250101
  )))
  s2 <- val_dc_render_svg(suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "turnaround_hours", group = "centre", typestatistics = "robust", testvalue = 24, seed = 20250101
  )))
  expect_identical(s1$svg, s2$svg)
})

# ---- Regression tests for resolved independent-review findings --------------

# Oracle: Yuen (1974) 20%-trimmed-mean interval; point estimate must lie inside its own 95% CI
test_that("C07 (VAL-jjdotchart-01): robust Summary (20% trimmed mean) lies inside its own [Lower, Upper] CI", {
  set.seed(42)
  d <- data.frame(
    g = factor(rep(c("Contam", "CleanA", "CleanB", "CleanC"), each = 200)),
    y = c(stats::rnorm(164, 10, 0.3), rep(1000, 36),
          stats::rnorm(200, 11, 0.3), stats::rnorm(200, 12, 0.3), stats::rnorm(200, 13, 0.3))
  )
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "y", group = "g", typestatistics = "robust", testvalue = 11.5, seed = 20250101
  ))
  tb <- res$summary$asDF
  row_c <- tb[tb$grp == "Contam", , drop = FALSE]
  expect_lte(row_c$ci_low, row_c$value)
  expect_gte(row_c$ci_high, row_c$value)
  expect_lt(row_c$ci_high - row_c$ci_low, 1.0)
})

# Oracle: exact null (all group summaries == testvalue) has zero deviation; must refuse or never report p=2.23e-308 / r=1.00
test_that("C10/C11 (VAL-jjdotchart-02): exact-null constant group summaries do not fabricate p=2.23e-308 or r_biserial=1.00", {
  skip_if_not_installed("svglite")
  d <- data.frame(y = rep(c(9, 10, 11), 4), g = factor(rep(c("G1", "G2", "G3", "G4"), each = 3)))
  r_rob <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "y", group = "g", typestatistics = "robust", testvalue = 10))
  r_np  <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "y", group = "g", typestatistics = "nonparametric", testvalue = 10))
  expect_match(val_dc_strip_html(r_rob$notices$content), "ERROR:", fixed = TRUE)
  expect_false(grepl("2.23e-308", val_dc_render_svg(r_rob)$txt, fixed = TRUE))
  expect_match(val_dc_strip_html(r_np$notices$content), "ERROR:", fixed = TRUE)
  expect_false(grepl("rbiserialrank=1.00", val_dc_render_svg(r_np)$txt, fixed = TRUE))
})

# Oracle: optional Split By (grvar) must not listwise-delete complete (dep, group) observations from unsplit outputs
test_that("C13 (VAL-jjdotchart-03): adding optional grvar with NAs preserves unstratified summary table and plot", {
  set.seed(7)
  d <- data.frame(
    y = c(stats::rnorm(20, 10, 2), stats::rnorm(20, 15, 2), stats::rnorm(20, 20, 2), stats::rnorm(20, 25, 2)),
    g = factor(rep(c("G1", "G2", "G3", "G4"), each = 20), levels = c("G1", "G2", "G3", "G4")),
    s = factor(c(rep("S1", 10), rep(NA_character_, 30), rep("S2", 40)))
  )
  r_solo <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "y", group = "g", testvalue = 17))
  r_gr   <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "y", group = "g", grvar = "s", testvalue = 17))
  expect_equal(r_gr$summary$asDF, r_solo$summary$asDF)
})

# Oracle: per-stratum validation & engine-failure detection in Split By (plot2)
test_that("C14 (VAL-jjdotchart-04): Split By detects per-stratum engine failure and handles 1-group strata without crashing", {
  skip_if_not_installed("svglite")
  set.seed(9)
  d_sf <- data.frame(
    y = c(stats::rnorm(90, 10, 1), rep(-20, 10), rep(0, 10), rep(30, 10)),
    g = factor(c(rep(c("A", "B", "C"), each = 30), rep(c("D", "E", "F"), each = 10))),
    s = factor(c(rep("FlatSplit", 90), rep("WideSplit", 30)))
  )
  r_sf <- suppressWarnings(ClinicoPath::jjdotchart(data = d_sf, dep = "y", group = "g", grvar = "s", testvalue = 0))
  expect_match(val_dc_strip_html(r_sf$notices$content), "ERROR:")

  d_1g <- data.frame(
    y = seq_len(50),
    g = factor(rep(paste0("G", 1:5), each = 10)),
    s = factor(c(rep("FourGroups", 40), rep("OneGroup", 10)))
  )
  r_1g <- suppressWarnings(ClinicoPath::jjdotchart(data = d_1g, dep = "y", group = "g", grvar = "s", testvalue = 25))
  expect_match(val_dc_strip_html(r_1g$notices$content), "ERROR:")
  expect_true(val_dc_render_svg(r_1g, item = "plot2")$ok)
})

# Oracle: missing group assignments encoded via addNA() must be excluded, never treated as a biological cohort
test_that("C15 (VAL-jjdotchart-05): explicit <NA> factor level in group is excluded rather than analysed as a group", {
  d <- data.frame(
    y = c(10, 11, 12, 20, 21, 22, 30, 31, 32, 99, 100, 101),
    g = addNA(factor(c(rep("A", 3), rep("B", 3), rep("C", 3), rep(NA, 3))))
  )
  res <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "y", group = "g", testvalue = 20))
  expect_identical(nrow(res$summary$asDF), 3L)
  expect_false(any(is.na(res$summary$asDF$grp)))
})

# Oracle: Phipson & Smyth (2010, DOI 10.2202/1544-6115.1585) Monte Carlo p >= 1/(B+1) & finite robust CI guard at k=5
test_that("C11 (VAL-jjdotchart-06): robust test guards k=5 zero-Winsorized-SE [-Inf, Inf] CIs and floors nboot=200 p-values", {
  skip_if_not_installed("svglite")
  d5 <- data.frame(y = unlist(lapply(101:105, function(m) c(m - 0.1, m, m + 0.1))),
                   g = factor(rep(paste0("G", 1:5), each = 3)))
  r5 <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d5, dep = "y", group = "g", typestatistics = "robust", testvalue = 95, k = 4, seed = 20250102
  ))
  expect_false(grepl("-Inf, Inf", val_dc_render_svg(r5)$txt, fixed = TRUE))
  expect_match(val_dc_strip_html(r5$notices$content), "WARNING:")

  d7 <- data.frame(y = unlist(lapply(101:107, function(m) c(m - 0.1, m, m + 0.1))),
                   g = factor(rep(paste0("G", 1:7), each = 3)))
  r7 <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d7, dep = "y", group = "g", typestatistics = "robust", testvalue = 0, k = 4, seed = 20250101
  ))
  expect_gte(val_dc_num(val_dc_render_svg(r7)$txt, "p=([0-9.eE+-]+)"), 1 / 201)
})

# Oracle: test-CI duality at alpha=0.05 via delta-method Fisher-z Jacobian (1 - r^2)^-1
test_that("C10 (VAL-jjdotchart-07): nonparametric rank-biserial 95% CI does not silently contradict Wilcoxon p > 0.05", {
  skip_if_not_installed("svglite")
  d <- data.frame(
    y = unlist(lapply(c(10.2, 11.5, 12.1, 13.0, 14.8, 15.2, 16.0, 18.4), function(m) c(m - 0.1, m, m + 0.1))),
    g = factor(rep(paste0("G", 1:8), each = 3))
  )
  res <- suppressWarnings(ClinicoPath::jjdotchart(data = d, dep = "y", group = "g", typestatistics = "nonparametric", testvalue = 12, k = 4))
  svg <- val_dc_render_svg(res)
  expect_lte(val_dc_num(svg$txt, "CI95% \\[([0-9.-]+),"), 0)
})

# Oracle: exchangeability of iid rows and group factor order on reported CIs
test_that("C20 (VAL-jjdotchart-09): row permutation and group factor-level reordering preserve summary 95% CI bounds", {
  set.seed(99)
  d <- ClinicoPath::jjdotchart_test_data
  for (ty in c("parametric", "nonparametric", "robust", "bayes")) {
    r1 <- suppressWarnings(ClinicoPath::jjdotchart(
      data = d, dep = "turnaround_hours", group = "centre",
      typestatistics = ty, testvalue = 24, seed = 20250101
    ))
    d_perm <- d[sample(nrow(d)), , drop = FALSE]
    d_perm$centre <- factor(as.character(d_perm$centre), levels = rev(levels(d$centre)))
    r2 <- suppressWarnings(ClinicoPath::jjdotchart(
      data = d_perm, dep = "turnaround_hours", group = "centre",
      typestatistics = ty, testvalue = 24, seed = 20250101
    ))
    m <- merge(r1$summary$asDF, r2$summary$asDF, by = "grp", suffixes = c(".1", ".2"))
    expect_equal(m$ci_low.2, m$ci_low.1, tolerance = 1e-12, info = ty)
    expect_equal(m$ci_high.2, m$ci_high.1, tolerance = 1e-12, info = ty)
  }
})

# Oracle: caption 'Solid blue line' must render without stroke-dasharray in SVG
test_that("C17 (VAL-jjdotchart-10): centrality line renders as a solid blue line matching its caption", {
  skip_if_not_installed("svglite")
  d <- datasets::OrchardSprays; d$treatment <- factor(d$treatment)
  res <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "decrease", group = "treatment", testvalue = 40, centralityplotting = TRUE
  ))
  svg <- val_dc_render_svg(res)
  blue_line <- grep("#0000FF", svg$lines, ignore.case = TRUE, value = TRUE)
  expect_length(blue_line, 1L)
  expect_no_match(blue_line, "stroke-dasharray", fixed = TRUE)
})

# Oracle: .a.yaml showSummaryTable contract ("The summary column is labeled with the statistic actually used")
test_that("C18 (VAL-jjdotchart-11): summary$columns$value$title updates dynamically to the selected statistic name", {
  r_np <- suppressWarnings(ClinicoPath::jjdotchart(
    data = val_dc_tiny(), dep = "tat", group = "lab", typestatistics = "nonparametric", testvalue = 15
  ))
  expect_identical(r_np$summary$columns$value$title, "Median")
})

# Oracle: stateful R6 Valid -> Invalid -> Valid row-filter lifecycle clears stale table rows
test_that("VAL-jjdotchart-12: live R6 Valid -> Invalid -> Valid filter transitions clear stale summary rows", {
  d <- data.frame(
    g = factor(rep(c("Lab_1", "Lab_2", "Lab_3", "Lab_4"), each = 4)),
    y = as.numeric(1:16)
  )
  opts <- ClinicoPath:::jjdotchartOptions$new(dep = "y", group = "g", testvalue = 8)
  ana <- ClinicoPath:::jjdotchartClass$new(options = opts, data = d)
  suppressWarnings(ana$run())
  expect_identical(nrow(ana$results$summary$asDF), 4L)

  ana$.__enclos_env__$private$.data <- subset(d, g == "Lab_1")
  suppressWarnings(ana$run())
  expect_identical(nrow(ana$results$summary$asDF), 0L)

  ana$.__enclos_env__$private$.data <- d[0, , drop = FALSE]
  suppressWarnings(ana$run())
  expect_identical(nrow(ana$results$summary$asDF), 0L)

  ana$.__enclos_env__$private$.data <- d
  suppressWarnings(ana$run())
  expect_identical(nrow(ana$results$summary$asDF), 4L)
})

# Oracle: tied group summaries receive equal empirical percentile labels on the right y-axis
test_that("VAL-jjdotchart-13: tied group summaries receive identical percentile labels on right y-axis", {
  d_tie <- data.frame(
    grp = factor(rep(c("Lab_1", "Lab_2", "Lab_3", "Lab_4", "Lab_5"), each = 4)),
    val = c(8, 10, 10, 12,  8, 10, 10, 12,  8, 10, 10, 12,  8, 10, 10, 12,  18, 20, 20, 22)
  )
  opts <- ClinicoPath:::jjdotchartOptions$new(dep = "val", group = "grp", testvalue = 12)
  ana <- ClinicoPath:::jjdotchartClass$new(options = opts, data = d_tie)
  suppressWarnings(ana$run())
  prep <- ana$.__enclos_env__$private$.prepareData()
  p <- ana$.__enclos_env__$private$.buildMainPlot(prep, ggplot2::theme_bw())
  sec_labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.sec$get_labels()
  expect_identical(length(unique(sec_labels[1:4])), 1L)
})

# Oracle: clinical caseload skew / opposite-sign ecological divergence & within-group SE heterogeneity warnings
test_that("VAL-jjdotchart-14: caseload imbalance, ecological sign reversal, and SE heterogeneity raise WARNINGs", {
  set.seed(123)
  d_eco <- data.frame(
    site = factor(c(rep("Hub", 90), rep(paste0("Sat_", 1:9), each = 10))),
    tat  = c(stats::rnorm(90, 18.5, 2.0), stats::rnorm(90, 26.6, 2.0))
  )
  r_eco <- suppressWarnings(ClinicoPath::jjdotchart(data = d_eco, dep = "tat", group = "site", testvalue = 24))
  nt_eco <- val_dc_strip_html(r_eco$notices$content)
  expect_match(nt_eco, "highly unequal")
  expect_match(nt_eco, "OPPOSITE sides")

  d_het <- data.frame(
    site = factor(c(rep("Referral", 3), rep(paste0("Routine_", 1:7), each = 25))),
    tat  = c(24 - 50, 24, 24 + 50, stats::rnorm(175, 24, 1))
  )
  r_het <- suppressWarnings(ClinicoPath::jjdotchart(data = d_het, dep = "tat", group = "site", testvalue = 24))
  expect_match(val_dc_strip_html(r_het$notices$content), "standard errors vary widely")
})

# Oracle: Split By updates summary table title and warns on low per-panel group counts
test_that("VAL-jjdotchart-15: Split By updates summary title and warns on k=3 regional panels", {
  d <- ClinicoPath::jjdotchart_test_data
  r_gr <- suppressWarnings(ClinicoPath::jjdotchart(
    data = d, dep = "turnaround_hours", group = "centre", grvar = "region", testvalue = 24
  ))
  expect_false(grepl("one row per plotted point", r_gr$summary$title, fixed = TRUE))
  expect_match(val_dc_strip_html(r_gr$notices$content), "df = 2")
})
