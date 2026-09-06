# Release review of statsplot2.
#
# The arithmetic is delegated to ggstatsplot and is sound. The risk was in what
# the analysis reported about its own inputs: an inflated N, a "successful"
# one-group comparison, a silently reclassified constant outcome, and - worst -
# statistics computed on a random subsample that the panel never mentioned.

sp_notices <- function(res)
  # Do NOT strip "<...>" here: the notices output is Preformatted PLAIN TEXT, and
  # a tag-stripping regex eats everything between a "<" and the next ">" - e.g.
  # "recommended for n<30 ... Required: >=2 valid values" collapses into
  # "recommended for n=2 valid values", merging two notices.
  gsub("[[:space:]]+", " ", paste(as.character(res$notices$content), collapse = " "))

sp_big <- function(n = 30000, seed = 1) {
  set.seed(seed)
  data.frame(y = rnorm(n), g = factor(sample(c("A", "B"), n, TRUE)))
}


# ---- the subsample must announce itself ------------------------------------

test_that("random subsampling is disclosed, not left looking like missing data", {
  # The explanation went to message(), i.e. the R console, which a jamovi user
  # never sees. The panel said only "Observations used: 5,000 of 30,000", which
  # reads as NA exclusion. It matters because every statistic below is computed
  # on the subsample: measured over 300 replicates at d = 0.05 with n = 30,000,
  # full-data power is 99.7% (median p ~ 0.0000) against 45.3% on the 5,000-row
  # draw (median p = 0.0716) - a near-certain detection becomes a coin flip.
  n <- sp_notices(statsplot2(data = sp_big(), dep = "y", group = "g", sampleLarge = TRUE))
  expect_match(n, "random subsample", ignore.case = TRUE)
  expect_match(n, "drawn at RANDOM")
  expect_match(n, "lowers power")
  expect_match(n, "RANDOM SUBSAMPLE - see warning above")
})

test_that("no subsample warning when the full data is used", {
  n <- sp_notices(statsplot2(data = sp_big(), dep = "y", group = "g", sampleLarge = FALSE))
  expect_false(grepl("random subsample", n, ignore.case = TRUE))
  expect_match(n, "Observations used: 30,000 of 30,000")
})

test_that("the sampling threshold and size are under user control", {
  # Both were hard-coded at 10,000 / 5,000, so a user who wanted to keep 20,000
  # rows had only the all-or-nothing switch.
  skip_if_not(all(c("sampleThreshold", "sampleSize") %in% names(formals(statsplot2))),
              "new options not compiled yet - run jmvtools::prepare()")
  d <- sp_big()
  expect_match(sp_notices(statsplot2(data = d, dep = "y", group = "g",
                                     sampleLarge = TRUE)),
               "Observations used: 5,000")                       # defaults unchanged
  expect_match(sp_notices(statsplot2(data = d, dep = "y", group = "g",
                                     sampleLarge = TRUE, sampleSize = 20000)),
               "Observations used: 20,000")
  expect_match(sp_notices(statsplot2(data = d, dep = "y", group = "g",
                                     sampleLarge = TRUE, sampleThreshold = 50000)),
               "Observations used: 30,000 of 30,000")
  # asking to keep more rows than exist must not error or over-report
  expect_match(sp_notices(statsplot2(data = d, dep = "y", group = "g",
                                     sampleLarge = TRUE, sampleSize = 999999)),
               "Observations used: 30,000 of 30,000")
})


# ---- the reported N ---------------------------------------------------------

test_that("N counts usable observations, not rows", {
  # With `Exclude missing values` off (the default) missing values stay in the
  # frame and ggstatsplot drops them, so nrow() over-reported: 180 rows with 155
  # usable outcomes was announced as "Observations used: 180 of 180".
  data(statsplot2_test)
  d <- statsplot2_test
  d$tumor_reduction[1:20] <- NA
  usable <- sum(stats::complete.cases(d[, c("tumor_reduction", "treatment")]))
  n <- sp_notices(statsplot2(data = d, dep = "tumor_reduction", group = "treatment"))
  expect_match(n, sprintf("Observations used: %d of %d", usable, nrow(d)))
  expect_match(n, "omitted from the statistics")
})


# ---- setups that cannot mean anything ---------------------------------------

test_that("a one-group comparison is rejected, not called successful", {
  data(statsplot2_test)
  d <- statsplot2_test
  d$treatment <- "Placebo"
  n <- sp_notices(statsplot2(data = d, dep = "tumor_reduction", group = "treatment"))
  expect_match(n, "Only one group to compare")
  expect_false(grepl("completed successfully", n, fixed = TRUE))
})

test_that("a constant outcome is flagged, including the analysis-type switch", {
  # A constant numeric has one unique value, so the automatic plot selection
  # reads it as a FACTOR: the analysis silently changes from
  # independent_factor_continuous to independent_factor_factor.
  data(statsplot2_test)
  d <- statsplot2_test
  d$tumor_reduction <- 50
  n <- sp_notices(statsplot2(data = d, dep = "tumor_reduction", group = "treatment"))
  expect_match(n, "Outcome has no variation")
  expect_match(n, "changes the analysis type")
  expect_false(grepl("completed successfully", n, fixed = TRUE))
})

test_that("an ordinary analysis still reports success", {
  data(statsplot2_test)
  n <- sp_notices(statsplot2(data = statsplot2_test, dep = "tumor_reduction",
                             group = "treatment"))
  expect_match(n, "completed successfully")
  expect_false(grepl("Only one group", n, fixed = TRUE))
  expect_false(grepl("no variation", n, fixed = TRUE))
})


# ---- shipped data must load by its own name ---------------------------------

test_that("statsplot2 datasets load under the names they are documented by", {
  # data(foo) loads data/foo.rda and creates whatever objects are inside it. When
  # those differ, data(foo) succeeds and `foo` still does not exist:
  # statsplot2_repeated.rda held `repeated_measures_data` and
  # statsplot2_clinical.rda held `clinical_trial_data`.
  for (n in c("statsplot2_test", "statsplot2_repeated", "statsplot2_clinical",
              "statsplot2_skewed", "statsplot2_outliers")) {
    e <- new.env()
    data(list = n, package = "ClinicoPath", envir = e)
    expect_true(exists(n, envir = e), info = n)
  }
})


# ---- fix-function pass (2026-09-06) ------------------------------------------

test_that("a numeric with few distinct whole numbers is reported when read as categorical", {
  # Node counts 0-8 became a chi-square bar chart with a clean success summary.
  set.seed(4)
  d <- data.frame(nodes = sample(0:8, 40, TRUE), grp = factor(sample(c("A", "B"), 40, TRUE)))
  n <- sp_notices(statsplot2(data = d, dep = "nodes", group = "grp"))
  expect_match(n, "'nodes' has 9 distinct whole-number values and is analysed as categorical")
  expect_match(n, "Plot type: independent_factor_factor")
})

test_that("forceContinuous restores the continuous comparison", {
  skip_if_not("forceContinuous" %in% names(formals(statsplot2)),
              "option not compiled yet - run jmvtools::prepare()")
  set.seed(4)
  d <- data.frame(nodes = sample(0:8, 40, TRUE), grp = factor(sample(c("A", "B"), 40, TRUE)))
  n <- sp_notices(statsplot2(data = d, dep = "nodes", group = "grp", forceContinuous = TRUE))
  expect_match(n, "Plot type: independent_factor_continuous")
  expect_false(grepl("analysed as categorical", n))
})

test_that("a split level with fewer than two complete rows is announced, not dropped silently", {
  set.seed(5)
  d <- data.frame(y = rnorm(41), g = factor(sample(c("A", "B"), 41, TRUE)),
                  site = factor(c(rep("S1", 20), rep("S2", 20), "S3")))
  n <- sp_notices(statsplot2(data = d, dep = "y", group = "g", grvar = "site"))
  expect_match(n, "Split panel\\(s\\) omitted")
  expect_match(n, "'S3'")
})

test_that("NA in the split variable neither becomes a panel nor leaks rows into other panels", {
  # `NA == level` selected all-NA rows into every panel and drew an 'NA' stratum.
  set.seed(6)
  w <- data.frame(t0 = factor(sample(c("I", "II"), 60, TRUE)),
                  t1 = factor(sample(c("I", "II"), 60, TRUE)),
                  site = factor(sample(c("A", "B"), 60, TRUE)))
  w$site[1:5] <- NA
  a <- statsplot2Class$new(
    options = statsplot2Options$new(dep = "t1", group = "t0", grvar = "site", direction = "repeated"),
    data = w)
  a$run()
  p <- a$.__enclos_env__$private
  ai <- p$.detectAnalysisType(TRUE)
  g <- p$.plotGrouped(ai, p$.prepareDataForPlot(ai))
  for (i in 1:2) {
    tally <- g[[i]]$data
    expect_false(anyNA(tally$dp))
    expect_false(anyNA(tally$gr))
  }
  expect_null(tryCatch(g[[3]], error = function(e) NULL))
})

test_that("outliers are screened within groups, not against the pooled spread", {
  set.seed(7)
  d <- data.frame(y = c(rnorm(30, 0, 1), rnorm(30, 50, 1)), g = factor(rep(c("A", "B"), each = 30)))
  d$y[1] <- 20   # far outside group A, well inside the pooled range
  n <- sp_notices(statsplot2(data = d, dep = "y", group = "g"))
  expect_match(n, "Extreme Outliers Detected")
})

test_that("an empty dataset posts an ERROR notice, not only an HTML box", {
  d <- data.frame(y = numeric(0), g = factor(character(0), levels = c("A", "B")))
  n <- sp_notices(statsplot2(data = d, dep = "y", group = "g"))
  expect_match(n, "ERROR: No data available")
})

test_that("the summary does not echo a statistical approach for categorical comparisons", {
  set.seed(8)
  w <- data.frame(a = factor(sample(c("x", "y"), 50, TRUE)), b = factor(sample(c("p", "q", "r"), 50, TRUE)))
  n <- sp_notices(statsplot2(data = w, dep = "a", group = "b", distribution = "np"))
  expect_match(n, "Statistical approach: not applicable")
})

test_that("missing values in an alluvial diagram are described as an NA stratum", {
  set.seed(9)
  w <- data.frame(t0 = factor(sample(c("I", "II"), 40, TRUE)), t1 = factor(sample(c("I", "II"), 40, TRUE)))
  w$t1[1:3] <- NA
  n <- sp_notices(statsplot2(data = w, dep = "t1", group = "t0", direction = "repeated"))
  expect_match(n, "appear as an 'NA' stratum")
  expect_false(grepl("omitted from the statistics", n))
})


# ---- review pass (2026-09-06) ------------------------------------------------

sp_html <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = " ")))
sp_empty <- function(x) is.null(x) || !nzchar(paste(x, collapse = ""))
sp_fmt <- function(x) format(signif(x, 3), scientific = FALSE, trim = TRUE, big.mark = ",")

test_that("the result sentence is off by default and copy-ready when enabled", {
  skip_if_not("showSummary" %in% names(formals(statsplot2)), "option not compiled yet - run jmvtools::prepare()")
  set.seed(31)
  d <- data.frame(y = rnorm(60, 10, 2), g = factor(rep(c("A", "B", "C"), 20)))
  expect_true(sp_empty(statsplot2(data = d, dep = "y", group = "g")$summary$content))
  on <- sp_html(statsplot2(data = d, dep = "y", group = "g", showSummary = TRUE)$summary$content)
  expect_match(on, "y by g: One-way analysis of means")
  expect_match(on, "test statistic = [0-9.]+ \\(df = 2, [0-9.]+\\), p [=<] ")
  expect_match(on, "Omega2 = .* \\(95% CI .* to .*\\); n = 60\\.")
})

test_that("the result sentence quotes the same numbers as base R", {
  skip_if_not("showSummary" %in% names(formals(statsplot2)), "option not compiled yet")
  set.seed(32)
  d <- data.frame(y = rnorm(50), g = factor(rep(c("A", "B"), 25)))
  s <- sp_html(statsplot2(data = d, dep = "y", group = "g", showSummary = TRUE)$summary$content)
  r <- t.test(y ~ g, d)
  expect_match(s, "Welch Two Sample t-test", fixed = TRUE)
  expect_match(s, paste0("test statistic = ", sp_fmt(unname(r$statistic))), fixed = TRUE)
  expect_match(s, paste0("p = ", formatC(r$p.value, format = "f", digits = 3)), fixed = TRUE)
})

test_that("split-by gives one sentence per panel, labelled by level", {
  skip_if_not("showSummary" %in% names(formals(statsplot2)), "option not compiled yet")
  set.seed(33)
  d <- data.frame(y = rnorm(80), g = factor(rep(c("A", "B"), 40)), s = factor(rep(c("F", "M"), each = 40)))
  s <- sp_html(statsplot2(data = d, dep = "y", group = "g", grvar = "s", showSummary = TRUE)$summary$content)
  expect_match(s, "y by g, F: ", fixed = TRUE)
  expect_match(s, "y by g, M: ", fixed = TRUE)
})

test_that("a figure without a test says so instead of quoting nothing", {
  skip_if_not("showSummary" %in% names(formals(statsplot2)), "option not compiled yet")
  set.seed(34)
  w <- data.frame(t0 = factor(sample(c("I", "II"), 40, TRUE)), t1 = factor(sample(c("I", "II"), 40, TRUE)))
  s <- sp_html(statsplot2(data = w, dep = "t1", group = "t0", direction = "repeated", showSummary = TRUE)$summary$content)
  expect_match(s, "descriptive; no statistical test applies", fixed = TRUE)
})

test_that("a test that fails inside ggstatsplot is reported, not hidden", {
  # ggstatsplot swallows the error and draws the figure with no subtitle.
  skip_if_not("showSummary" %in% names(formals(statsplot2)), "option not compiled yet")
  set.seed(35)
  d <- data.frame(y = rnorm(90), g = factor(sample(c("A", "B", "C"), 90, TRUE)))
  fails <- inherits(try(statsExpressions::oneway_anova(d, g, y, type = "bayes"), silent = TRUE), "try-error")
  skip_if_not(fails, "Bayesian one-way test succeeds in this environment; nothing to surface")
  res <- statsplot2(data = d, dep = "y", group = "g", distribution = "bf", showSummary = TRUE)
  expect_match(sp_notices(res), "Statistics could not be computed", fixed = TRUE)
  expect_match(sp_html(res$summary$content), "Bayesian test could not be computed", fixed = TRUE)
})

test_that("the Bayesian sentence and interpretation state the prior", {
  skip_if_not("showSummary" %in% names(formals(statsplot2)), "option not compiled yet")
  set.seed(36)
  d <- data.frame(y = rnorm(50), g = factor(rep(c("A", "B"), 25)))
  s <- sp_html(statsplot2(data = d, dep = "y", group = "g", distribution = "bf", showSummary = TRUE)$summary$content)
  expect_match(s, "BF10 = .* \\(prior: cauchy, scale 0\\.707\\)")
  e <- sp_html(statsplot2(data = d, dep = "y", group = "g", distribution = "bf", showExplanations = TRUE)$ExplanationMessage$content)
  expect_match(e, "Bayes factors depend on the prior", fixed = TRUE)
})

test_that("fewer than 10 observations escalates the sample-size notice", {
  set.seed(37)
  d <- data.frame(y = rnorm(8), g = factor(rep(c("A", "B"), 4)))
  n <- sp_notices(statsplot2(data = d, dep = "y", group = "g"))
  expect_match(n, "Very Small Sample", fixed = TRUE)
  expect_match(n, "report descriptive statistics only", fixed = TRUE)
})

test_that("the explanation panel is opt-in", {
  skip_if_not("showExplanations" %in% names(formals(statsplot2)), "option not compiled yet")
  set.seed(38)
  d <- data.frame(y = rnorm(40), g = factor(rep(c("A", "B"), 20)))
  expect_true(sp_empty(statsplot2(data = d, dep = "y", group = "g")$ExplanationMessage$content))
  on <- sp_html(statsplot2(data = d, dep = "y", group = "g", showExplanations = TRUE)$ExplanationMessage$content)
  expect_match(on, "Plot Selection Summary", fixed = TRUE)
  expect_match(on, "Clinical Interpretation", fixed = TRUE)
})


# ---- release review (2026-09-06) ---------------------------------------------

test_that("an identifier-like grouping variable is rejected before the slow, doomed comparison", {
  # 200 one-row levels ran ggstatsplot's pairwise tests for 25 s and then died
  # with "not enough observations".
  d <- data.frame(y = rnorm(120), id = factor(sprintf("P%03d", 1:120)))
  expect_error(statsplot2(data = d, dep = "y", group = "id"), "looks like an identifier")
})

test_that("many groups are warned about, not blocked", {
  set.seed(51)
  d <- data.frame(y = rnorm(300), site = factor(rep(sprintf("S%02d", 1:25), 12)))
  n <- sp_notices(statsplot2(data = d, dep = "y", group = "site"))
  expect_match(n, "Many groups", fixed = TRUE)
  expect_match(n, "300 pairwise tests", fixed = TRUE)   # 25 * 24 / 2
  expect_match(n, "Analysis completed successfully", fixed = TRUE)
})
