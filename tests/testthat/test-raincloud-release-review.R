# Release-review regression tests for raincloud.
#
# Each block below is numbered to match one repair made in the release review of
# R/raincloud.b.R. Every expectation asserts what the USER SEES in an Html result
# item (todo / statistics / outliers / normality / comparison / interpretation),
# not an internal value, and every one was observed FAILING on the pre-repair code
# (`git show HEAD:R/raincloud.b.R`) before it was written.
#
# HTML-STRIPPING NOTE: the content deliberately carries the entity "&lt;" (a bare
# "<" would be eaten by an HTML parser). The "Result:" row used to be the one
# place that still emitted a literal "p < 0.05" - that was a defect (a browser
# parses "< 0.05</td>" as a single bogus element and the Result cell renders
# EMPTY) and it now uses the entity as well. rc_text() still strips only things
# that actually look like tags (`<` followed by a letter, `/` or `!`) and decodes
# entities afterwards, so assertions written against the stripped text keep
# seeing "p < 0.05". Assertions specifically ABOUT the entity are made against
# the RAW html instead.

library(testthat)

# ---------------------------------------------------------------- harness ----

rc_run <- function(data, ...) {
    opts <- do.call(ClinicoPath:::raincloudOptions$new,
                    utils::modifyList(list(dep_var = "y", group_var = "g"), list(...)))
    a <- ClinicoPath:::raincloudClass$new(options = opts, data = data)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}

rc_text <- function(html) {
    if (is.null(html) || !nzchar(html)) return("")
    t <- gsub("<[a-zA-Z/!][^>]*>", " ", html)   # real tags only - see note above
    t <- gsub("&lt;", "<", t, fixed = TRUE)
    t <- gsub("&gt;", ">", t, fixed = TRUE)
    t <- gsub("&amp;", "&", t, fixed = TRUE)
    trimws(gsub("\\s+", " ", t))
}

rc_comparison <- function(a) rc_text(a$results$comparison$content)

# stats::oneway.test is broken session-wide by formula.tools (loaded via logistf):
# its as.character.formula returns one string where base R returns three, so
# oneway.test rejects every formula with "a two-sided formula is required". The
# module shields its own call the same way; reference values must be too.
rc_welch_ref <- function(formula, data)
    ClinicoPath:::withBaseFormulaChar(stats::oneway.test(formula, data = data, var.equal = FALSE))

# Null data: three groups with IDENTICAL means, very unequal variances AND very
# unequal group sizes (n = 6/6/60, sd = 8/8/1) - the configuration under which
# ordinary ANOVA is badly anti-conservative. At seed 25 aov reports p = 0.0016
# while Welch reports p = 0.15. All three groups pass Shapiro-Wilk, so AUTO
# reaches the parametric branch and the variance check is what has to save it.
rc_null_unequal_var <- function(seed = 25) {
    set.seed(seed)
    data.frame(
        y = c(stats::rnorm(6, 50, 8), stats::rnorm(6, 50, 8), stats::rnorm(60, 50, 1)),
        g = factor(rep(c("A", "B", "C"), times = c(6, 6, 60))))
}

rc_3group <- function(seed = 11) {
    set.seed(seed)
    data.frame(y = c(stats::rnorm(20, 0), stats::rnorm(20, 1.5), stats::rnorm(20, 3)),
               g = factor(rep(c("A", "B", "C"), each = 20)))
}


# == 1. AUTO picks Welch's ANOVA when variances are unequal ====================

test_that("1a. AUTO runs Welch's ANOVA, not aov, when Bartlett rejects equal variances", {
    d <- rc_null_unequal_var()

    # The trap being guarded against, stated as a fact about this dataset.
    aov_p <- summary(stats::aov(y ~ g, data = d))[[1]][["Pr(>F)"]][1]
    welch <- rc_welch_ref(y ~ g, d)
    expect_lt(aov_p, 0.05)          # ordinary ANOVA calls null data significant
    expect_gt(welch$p.value, 0.05)  # Welch does not

    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "auto"))

    expect_match(txt, "Welch's ANOVA", fixed = TRUE)
    expect_false(grepl("one-way ANOVA", txt, fixed = TRUE))
    expect_match(txt, "not significant at the 0.05 level", fixed = TRUE)
    # Independently verifiable: reported F, df and p are oneway.test's.
    expect_match(txt, sprintf("F(%g,%s) = %s", round(welch$parameter[1]),
                              format(round(welch$parameter[2], 2)),
                              format(round(welch$statistic, 4))), fixed = TRUE)
    expect_match(txt, sprintf("P-value: %s", formatC(welch$p.value, format = "f", digits = 3)),
                 fixed = TRUE)
    # The copy-ready sentence must agree with the test that was actually run.
    expect_match(txt, "did not find a statistically significant difference", fixed = TRUE)
})

test_that("1b. forcing ANOVA still runs aov but warns that equal variances are doubtful", {
    d <- rc_null_unequal_var()
    aov_p <- summary(stats::aov(y ~ g, data = d))[[1]][["Pr(>F)"]][1]

    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "anova"))

    expect_match(txt, "one-way ANOVA", fixed = TRUE)
    expect_match(txt, "Equal variances are doubtful", fixed = TRUE)
    expect_match(txt, "Bartlett p", fixed = TRUE)
    expect_match(txt, "Use Welch's ANOVA", fixed = TRUE)
    expect_match(txt, sprintf("P-value: %s", formatC(aov_p, format = "f", digits = 3)),
                 fixed = TRUE)
})

test_that("1c. AUTO still picks ordinary ANOVA when variances ARE equal", {
    set.seed(4)
    d <- data.frame(y = c(stats::rnorm(25, 0), stats::rnorm(25, 0.4), stats::rnorm(25, 0.9)),
                    g = factor(rep(c("A", "B", "C"), each = 25)))
    skip_if(stats::bartlett.test(d$y, d$g)$p.value < 0.05, "seed produced unequal variances")

    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "auto"))
    expect_match(txt, "one-way ANOVA", fixed = TRUE)
    expect_false(grepl("Welch's ANOVA", txt, fixed = TRUE))
    expect_false(grepl("Equal variances are doubtful", txt, fixed = TRUE))

    fit <- summary(stats::aov(y ~ g, data = d))[[1]]
    expect_match(txt, sprintf("F(%d,%d) = %s", fit$Df[1], fit$Df[2],
                              format(round(fit[["F value"]][1], 4))), fixed = TRUE)
})

test_that("1d. non-normal groups still route to Kruskal-Wallis, verified against kruskal.test", {
    set.seed(6)
    d <- data.frame(y = c(stats::rexp(30, 1), stats::rexp(30, 0.5), stats::rexp(30, 0.25)),
                    g = factor(rep(c("A", "B", "C"), each = 30)))
    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "auto"))

    expect_match(txt, "Kruskal-Wallis test", fixed = TRUE)
    kw <- stats::kruskal.test(y ~ g, data = d)
    expect_match(txt, format(round(unname(kw$statistic), 4)), fixed = TRUE)
    # Kruskal-Wallis speaks to distributions, not means.
    expect_match(txt, "difference in distributions", fixed = TRUE)
})


# == 2. P-value Adjustment now runs real post-hoc pairwise tests ===============

test_that("2a. >2 groups: adjustment produces a pairwise table matching pairwise.t.test", {
    d <- rc_3group()
    a <- rc_run(d, comparison_test = TRUE, comparison_method = "anova",
                adjust_method = "bonferroni")
    txt <- rc_comparison(a)

    expect_match(txt, "Pairwise comparisons (bonferroni-adjusted)", fixed = TRUE)
    expect_match(txt, "B vs A", fixed = TRUE)
    expect_match(txt, "C vs A", fixed = TRUE)
    expect_match(txt, "C vs B", fixed = TRUE)
    # The old no-op row must be gone.
    expect_false(grepl("Adjusted (bonferroni):", txt, fixed = TRUE))

    ref <- stats::pairwise.t.test(d$y, d$g, p.adjust.method = "bonferroni",
                                  pool.sd = TRUE)$p.value
    expect_true(all(ref[!is.na(ref)] < 0.001))    # all three are "&lt; 0.001" here
    # 3 pairwise cells + the omnibus P-value cell + the "Result:" row + the
    # copy-ready sentence. This was 5 while the "Result:" row still emitted a
    # BARE "p < 0.001"; that row now uses the entity like every other p in the
    # block, so it is counted here too.
    expect_equal(lengths(regmatches(a$results$comparison$content,
                                    gregexpr("&lt; 0\\.001", a$results$comparison$content)))[[1]],
                 6L)
})

test_that("2b. adjusted pairwise p really differs from the unadjusted one", {
    # Effects small enough that bonferroni moves the number into view.
    set.seed(21)
    d <- data.frame(y = c(stats::rnorm(30, 0), stats::rnorm(30, 0.55), stats::rnorm(30, 0.75)),
                    g = factor(rep(c("A", "B", "C"), each = 30)))

    raw <- stats::pairwise.t.test(d$y, d$g, p.adjust.method = "none", pool.sd = TRUE)$p.value
    adj <- stats::pairwise.t.test(d$y, d$g, p.adjust.method = "bonferroni", pool.sd = TRUE)$p.value
    skip_if(all(is.na(raw)) || min(adj, na.rm = TRUE) < 0.001,
            "seed gave no adjusted p in printable range")

    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "anova",
                                adjust_method = "bonferroni"))
    # Each printed cell is the ADJUSTED value, and at least one of them is
    # genuinely larger than its raw counterpart (i.e. a correction happened).
    printed <- formatC(adj[!is.na(adj)], format = "f", digits = 3)
    for (p in printed) expect_match(txt, p, fixed = TRUE)
    expect_true(any(adj[!is.na(adj)] > raw[!is.na(raw)]))
})

test_that("2c. Kruskal route uses pairwise.wilcox.test", {
    set.seed(13)
    d <- data.frame(y = c(stats::rexp(25, 1), stats::rexp(25, 0.4), stats::rexp(25, 0.2)),
                    g = factor(rep(c("A", "B", "C"), each = 25)))
    html <- rc_run(d, comparison_test = TRUE, comparison_method = "kruskal",
                   adjust_method = "holm")$results$comparison$content
    expect_match(rc_text(html), "Pairwise comparisons (holm-adjusted)", fixed = TRUE)

    # Every cell of the printed table is pairwise.wilcox.test's holm-adjusted p,
    # formatted the way the module formats p-values.
    ref <- suppressWarnings(stats::pairwise.wilcox.test(
        d$y, d$g, p.adjust.method = "holm", exact = FALSE)$p.value)
    ref <- ref[!is.na(ref)]
    expect_gt(length(ref), 0)
    for (p in ref) {
        shown <- if (p < 0.001) "&lt; 0.001" else formatC(p, format = "f", digits = 3)
        expect_match(html, shown, fixed = TRUE)
    }
})

test_that("2d. exactly 2 groups: says there is nothing to adjust, prints no fake adjusted row", {
    set.seed(15)
    d <- data.frame(y = c(stats::rnorm(20, 0), stats::rnorm(20, 1)),
                    g = factor(rep(c("A", "B"), each = 20)))
    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "ttest",
                                adjust_method = "holm"))

    expect_match(txt, "only one comparison is being made, so there is nothing to adjust",
                 fixed = TRUE)
    expect_false(grepl("Adjusted (holm):", txt, fixed = TRUE))
    expect_false(grepl("Pairwise comparisons", txt, fixed = TRUE))
})

test_that("2e. adjust_method = none prints neither a pairwise table nor an adjustment note", {
    txt <- rc_comparison(rc_run(rc_3group(), comparison_test = TRUE,
                                comparison_method = "anova", adjust_method = "none"))
    expect_false(grepl("Pairwise comparisons", txt, fixed = TRUE))
    expect_false(grepl("nothing to adjust", txt, fixed = TRUE))
})


# == 3. A group with n < 2 no longer crashes the analysis =====================

test_that("3a. a singleton group yields a named message instead of a t.test error", {
    d <- data.frame(y = c(1, 2, 3, 4, 5, 9), g = factor(c("A", "A", "A", "B", "B", "C")))
    a <- expect_error(rc_run(d, comparison_test = TRUE), NA)   # must not throw
    txt <- rc_comparison(a)

    expect_match(txt, "needs at least two observations in every group", fixed = TRUE)
    expect_match(txt, "C (n=1)", fixed = TRUE)
    expect_false(grepl("not enough", txt, fixed = TRUE))
    # The rest of the output survives.
    expect_match(rc_text(a$results$interpretation$content),
                 "Raincloud Plot Interpretation Guide", fixed = TRUE)
})

test_that("3b. every offending group is named, not just the first", {
    d <- data.frame(y = c(1, 2, 3, 4, 7, 9),
                    g = factor(c("A", "A", "A", "B", "C", "D")))
    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "kruskal"))
    for (g in c("B (n=1)", "C (n=1)", "D (n=1)")) expect_match(txt, g, fixed = TRUE)
})


# == 4. p-values print as the entity "&lt; 0.001", never "0.0000" ==============

test_that("4a. a vanishing p renders as the HTML entity, not 0.0000 and not p = 0.000", {
    set.seed(5)
    d <- data.frame(y = c(stats::rnorm(30, 0, 1), stats::rnorm(30, 10, 1)),
                    g = factor(rep(c("A", "B"), each = 30)))
    html <- rc_run(d, comparison_test = TRUE, comparison_method = "ttest"
                   )$results$comparison$content

    expect_lt(stats::t.test(d$y[d$g == "A"], d$y[d$g == "B"])$p.value, 1e-30)

    expect_match(html, "&lt; 0.001", fixed = TRUE)     # entity, not a bare "<"
    expect_false(grepl("0.0000", html, fixed = TRUE))
    expect_false(grepl("p = 0.000", html, fixed = TRUE))
    # In the copy-ready sentence too.
    expect_match(html, "(p &lt; 0.001)", fixed = TRUE)
})

test_that("4b. an ordinary p prints to three decimals", {
    set.seed(6)   # gives p ~ 0.123, comfortably inside the printable range
    d <- data.frame(y = c(stats::rnorm(20, 0), stats::rnorm(20, 0.7)),
                    g = factor(rep(c("A", "B"), each = 20)))
    tt <- stats::t.test(d$y[d$g == "A"], d$y[d$g == "B"])
    expect_gt(tt$p.value, 0.001)
    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "ttest"))

    expect_match(txt, sprintf("P-value: %s", formatC(tt$p.value, format = "f", digits = 3)),
                 fixed = TRUE)
    expect_match(txt, sprintf("(p = %s)", formatC(tt$p.value, format = "f", digits = 3)),
                 fixed = TRUE)
})

test_that("4c. stripping real tags leaves the decoded p-value visible to a reader", {
    set.seed(5)
    d <- data.frame(y = c(stats::rnorm(30, 0, 1), stats::rnorm(30, 10, 1)),
                    g = factor(rep(c("A", "B"), each = 30)))
    # The entity survives tag removal and decodes to a readable "< 0.001".
    expect_match(rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "ttest")),
                 "P-value: < 0.001", fixed = TRUE)
})


# == 5. Cohen's d uses the UNPOOLED SD, matching the Welch t reported ==========

test_that("5a. Cohen's d equals the unpooled-SD estimate, and is labelled as such", {
    set.seed(31)
    # Deliberately unequal n AND unequal sd so pooled and unpooled differ clearly.
    d <- data.frame(y = c(stats::rnorm(12, 0, 3), stats::rnorm(48, 1.5, 1)),
                    g = factor(rep(c("A", "B"), times = c(12, 48))))
    x1 <- d$y[d$g == "A"]; x2 <- d$y[d$g == "B"]

    unpooled <- sqrt((stats::var(x1) + stats::var(x2)) / 2)
    pooled   <- sqrt(((length(x1) - 1) * stats::var(x1) + (length(x2) - 1) * stats::var(x2)) /
                     (length(x1) + length(x2) - 2))
    d_unpooled <- (mean(x1) - mean(x2)) / unpooled
    d_pooled   <- (mean(x1) - mean(x2)) / pooled
    expect_gt(abs(d_unpooled - d_pooled), 0.05)   # the two really do differ here

    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "ttest",
                                effect_size = TRUE))
    expect_match(txt, "Cohen's d (unpooled SD, matching Welch's t)", fixed = TRUE)
    expect_match(txt, sprintf("%.3f", d_unpooled), fixed = TRUE)
    expect_false(grepl(sprintf("%.3f", d_pooled), txt, fixed = TRUE))

    # And the t it is paired with is Welch's (fractional df), as reported.
    tt <- stats::t.test(x1, x2)
    expect_match(txt, sprintf("t = %s, df = %s", format(round(unname(tt$statistic), 4)),
                              format(round(unname(tt$parameter), 1))), fixed = TRUE)
})

test_that("5b. zero-variance groups report d as not estimable rather than Inf/NaN", {
    d <- data.frame(y = c(rep(2, 6), rep(5, 6)), g = factor(rep(c("A", "B"), each = 6)))
    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "ttest",
                                effect_size = TRUE))
    expect_false(grepl("NaN", txt, fixed = TRUE))
    expect_false(grepl("Inf", txt, fixed = TRUE))
})

test_that("5c. Wilcoxon refuses to report Cohen's d", {
    set.seed(17)
    d <- data.frame(y = c(stats::rexp(20, 1), stats::rexp(20, 0.3)),
                    g = factor(rep(c("A", "B"), each = 20)))
    txt <- rc_comparison(rc_run(d, comparison_test = TRUE, comparison_method = "wilcoxon",
                                effect_size = TRUE))
    expect_match(txt, "only reported for the parametric t-test", fixed = TRUE)
})


# == 6. a factor dependent variable no longer becomes level indices ============

# A jamovi nominal column arrives as a FACTOR carrying an integer `values`
# attribute, and jmvcore::canBeNumeric() accepts exactly that as "numeric" - so
# `permitted: numeric` does NOT keep it out. The old as.numeric(factor) then
# returned level INDICES (1,2,3,4) and every statistic below was computed on rank
# codes: the mean of 10/20/30/40 came out as 2.5 instead of 25.
test_that("6a. a jamovi-style numeric-labelled factor is read by its labels, not its codes", {
    y <- factor(c("10", "20", "30", "40", "10", "20", "30", "40"))
    attr(y, "values") <- c(10L, 20L, 30L, 40L)     # what jamovi attaches
    d <- data.frame(g = factor(rep(c("A", "B"), each = 4)))
    d$y <- y

    txt <- rc_text(rc_run(d, show_statistics = TRUE)$results$statistics$content)

    expect_match(txt, "25", fixed = TRUE)          # true mean of both groups
    expect_match(txt, "10 - 40", fixed = TRUE)     # true range
    expect_false(grepl("2.5 2.5", txt, fixed = TRUE))   # the old index-coded mean
    expect_false(grepl("1 - 4", txt, fixed = TRUE))     # the old index-coded range
})

test_that("6b. label values drive the group comparison too", {
    y <- factor(rep(c("100", "200", "300", "400"), 4))
    attr(y, "values") <- c(100L, 200L, 300L, 400L)
    d <- data.frame(g = factor(rep(c("A", "B"), each = 8)))
    d$y <- y
    a <- rc_run(d, show_statistics = TRUE)
    txt <- rc_text(a$results$statistics$content)
    expect_match(txt, "250", fixed = TRUE)          # mean of 100..400
    expect_match(txt, "100 - 400", fixed = TRUE)
})


# == 7. Inf is filtered and the removal is disclosed ===========================

test_that("7a. an infinite value is dropped and counted in the data-summary panel", {
    d <- data.frame(y = c(1, 2, 3, 4, 5, Inf, 6, 7, 8, 9, 10, 11),
                    g = factor(rep(c("A", "B"), each = 6)))
    a <- rc_run(d, show_statistics = TRUE)

    todo <- rc_text(a$results$todo$content)
    expect_match(todo, "non-finite", fixed = TRUE)
    expect_match(todo, "1 further row(s) removed", fixed = TRUE)
    expect_match(todo, "11 complete rows", fixed = TRUE)
    expect_match(todo, "A (n=5)", fixed = TRUE)

    # And no Inf reached the statistics.
    stats_txt <- rc_text(a$results$statistics$content)
    expect_false(grepl("Inf", stats_txt, fixed = TRUE))
    expect_false(grepl("NaN", stats_txt, fixed = TRUE))
    expect_match(stats_txt, "1 - 5", fixed = TRUE)   # group A range, Inf excluded
})

test_that("7b. -Inf is filtered the same way", {
    d <- data.frame(y = c(-Inf, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                    g = factor(rep(c("A", "B"), each = 5)))
    a <- rc_run(d, show_statistics = TRUE)
    expect_match(rc_text(a$results$todo$content), "1 further row(s) removed", fixed = TRUE)
    expect_false(grepl("-Inf", rc_text(a$results$statistics$content), fixed = TRUE))
})

test_that("7c. no removal note appears when the data are clean", {
    d <- data.frame(y = as.numeric(1:10), g = factor(rep(c("A", "B"), each = 5)))
    expect_false(grepl("non-finite", rc_text(rc_run(d)$results$todo$content), fixed = TRUE))
})

test_that("7d. an all-Inf dependent variable is rejected with a clear message, not a crash", {
    d <- data.frame(y = c(Inf, Inf, Inf, Inf), g = factor(rep(c("A", "B"), each = 2)))
    expect_error(rc_run(d), "No usable numeric values remain")
})


# == 8. Shapiro-Wilk p > 0.05 is not "Normal" =================================

test_that("8a. a non-significant Shapiro-Wilk reads as an absence of evidence", {
    set.seed(9)
    d <- data.frame(y = c(stats::rnorm(30), stats::rnorm(30)),
                    g = factor(rep(c("A", "B"), each = 30)))
    txt <- rc_text(rc_run(d, normality_test = TRUE)$results$normality$content)

    expect_match(txt, "No evidence against normality", fixed = TRUE)
    # The old verdict claimed the null.
    expect_false(grepl(">Normal<", rc_run(d, normality_test = TRUE)$results$normality$content,
                       fixed = TRUE))
    # The footnote must say the same thing.
    expect_match(txt, "does not establish that the data ARE normal", fixed = TRUE)
    expect_match(txt, "little power at small n", fixed = TRUE)
})

test_that("8b. a significant Shapiro-Wilk reads as a departure, and matches shapiro.test", {
    set.seed(2)
    d <- data.frame(y = c(stats::rexp(40, 1) * 10, stats::rnorm(40)),
                    g = factor(rep(c("A", "B"), each = 40)))
    txt <- rc_text(rc_run(d, normality_test = TRUE)$results$normality$content)

    expect_match(txt, "Departs from normality", fixed = TRUE)
    expect_false(grepl("Non-normal", txt, fixed = TRUE))
    sw <- stats::shapiro.test(d$y[d$g == "A"])
    expect_match(txt, format(round(unname(sw$statistic), 4)), fixed = TRUE)
})

test_that("8c. an untestable group says so", {
    d <- data.frame(y = c(rep(3, 8), 1:8), g = factor(rep(c("A", "B"), each = 8)))
    txt <- rc_text(rc_run(d, normality_test = TRUE)$results$normality$content)
    expect_match(txt, "Not testable (constant values)", fixed = TRUE)
})


# == 9. zero-SD / zero-MAD groups report "not testable", not "0 outliers" ======

test_that("9a. z-score on a constant group is reported as not testable", {
    d <- data.frame(y = c(rep(5, 10), 1:10), g = factor(rep(c("A", "B"), each = 10)))
    txt <- rc_text(rc_run(d, show_outliers = TRUE,
                          outlier_method = "zscore")$results$outliers$content)

    expect_match(txt, "A: not testable", fixed = TRUE)
    expect_match(txt, "the spread used by this method is zero", fixed = TRUE)
    expect_match(txt, "try the IQR method", fixed = TRUE)
    expect_false(grepl("A: 0 outliers detected", txt, fixed = TRUE))
    expect_match(txt, "B: 0 outliers detected", fixed = TRUE)   # the other group still runs
})

test_that("9b. modified z-score on a zero-MAD group (majority tied) is not testable", {
    # MAD is 0 whenever more than half the values are identical - common in
    # rounded lab data - even though the group is NOT constant and has an
    # obvious outlier at 20.
    d <- data.frame(y = c(5, 5, 5, 5, 5, 5, 5, 6, 7, 20, 1:10),
                    g = factor(rep(c("A", "B"), each = 10)))
    expect_equal(stats::mad(d$y[d$g == "A"]), 0)

    txt <- rc_text(rc_run(d, show_outliers = TRUE,
                          outlier_method = "modified_zscore")$results$outliers$content)
    expect_match(txt, "A: not testable", fixed = TRUE)
    expect_false(grepl("A: 0 outliers detected", txt, fixed = TRUE))
})

test_that("9c. IQR method is unaffected and still counts outliers", {
    d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100, 1:10),
                    g = factor(rep(c("A", "B"), each = 10)))
    txt <- rc_text(rc_run(d, show_outliers = TRUE,
                          outlier_method = "iqr")$results$outliers$content)
    expect_match(txt, "A: 1 outliers detected", fixed = TRUE)
    expect_false(grepl("not testable", txt, fixed = TRUE))
})


# == 10. the selectable "--- GraphPad Prism ---" separators no longer fall through

# NOTE: of the two separators only the THEME one changed behaviour. The palette
# separator already fell through to `scales::hue_pal()`, which is what "Default"
# also returns, so this is a contract test that pins the now-explicit mapping.
# The theme separator previously fell through to theme_minimal(), NOT clinical -
# 10b is the one that fails on the pre-repair code.
test_that("10a. selecting the palette separator falls back to the default palette", {
    set.seed(9)
    d <- data.frame(y = c(stats::rnorm(30), stats::rnorm(30)),
                    g = factor(rep(c("A", "B"), each = 30)))

    sep <- rc_run(d, color_palette = "separator_prism")$.__enclos_env__$private$.get_color_palette(3)
    def <- rc_run(d, color_palette = "default")$.__enclos_env__$private$.get_color_palette(3)
    vir <- rc_run(d, color_palette = "viridis")$.__enclos_env__$private$.get_color_palette(3)

    expect_equal(sep, def)
    expect_false(identical(sep, vir))
    expect_length(sep, 3)
    expect_true(all(grepl("^#", sep)))          # real colours, not NA/NULL
    expect_false(any(is.na(sep)))
})

test_that("10b. selecting the theme separator falls back to the clinical theme", {
    set.seed(9)
    d <- data.frame(y = c(stats::rnorm(30), stats::rnorm(30)),
                    g = factor(rep(c("A", "B"), each = 30)))

    sep <- rc_run(d, plot_theme = "separator_theme")$.__enclos_env__$private$.get_plot_theme()
    cli <- rc_run(d, plot_theme = "clinical")$.__enclos_env__$private$.get_plot_theme()
    mini <- rc_run(d, plot_theme = "minimal")$.__enclos_env__$private$.get_plot_theme()

    expect_s3_class(sep, "theme")
    expect_true(isTRUE(all.equal(sep, cli)))
    expect_false(isTRUE(all.equal(sep, mini)))
})

test_that("10c. a plot with both separators selected still renders", {
    set.seed(9)
    d <- data.frame(y = c(stats::rnorm(30), stats::rnorm(30)),
                    g = factor(rep(c("A", "B"), each = 30)))
    a <- rc_run(d, color_palette = "separator_prism", plot_theme = "separator_theme")

    f <- tempfile(fileext = ".png")
    grDevices::png(f, 600, 400)
    on.exit({try(grDevices::dev.off(), silent = TRUE); unlink(f)}, add = TRUE)
    expect_error(
        print(a$.__enclos_env__$private$.plot(a$results$plot,
                                              ggtheme = ggplot2::theme_bw(), theme = NULL)),
        NA)
})
