# Release review of jjpiestats.
#
# The dangerous surface here is not the arithmetic - the chi-square matches base
# R exactly. It is the PROSE: this analysis emits a copy-ready Methods sentence
# for manuscripts, an interpretation guide, and an assumptions panel, and each of
# those named a test or a threshold that did not correspond to what was computed.

pie_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x %||% "")))
pie_notices <- function(res) pie_txt(paste(as.character(res$notices$content), collapse = " "))

# 2x2 with one expected count below 5, chosen because Pearson and Fisher land on
# OPPOSITE sides of 0.05 - which is precisely why naming the wrong test matters:
#          A   B      expected 7.38 8.62 / 4.62 5.38
#   No    10   6      Pearson chi2(1) = 4.4726, p = 0.0344   <- what the chart shows
#   Yes    2   8      Fisher exact             p = 0.0511    <- the valid test
pie_sparse <- function() {
    data.frame(resp = factor(rep(c("Yes", "No", "Yes", "No"), times = c(2, 10, 8, 6))),
               arm  = factor(rep(c("A", "B"), times = c(12, 14))))
}
pie_wide <- function() {
    set.seed(11)
    data.frame(resp = factor(sample(c("CR", "PR", "SD", "PD"), 200, TRUE)),
               arm  = factor(sample(c("A", "B"), 200, TRUE)))
}


# ---- the method the report claims was run -----------------------------------

test_that("the copy-ready Methods sentence names the test that actually ran", {
    # ggpiestats returns the SAME uncorrected Pearson chi-square for parametric,
    # nonparametric and robust - statsExpressions routes every frequentist type
    # to chi-square and offers no exact test. The report nonetheless printed
    # "using Fisher's exact test" for nonparametric, i.e. Methods text asserting
    # a test that was never performed, and whose p-value (0.0511) is on the other
    # side of 0.05 from the one displayed (0.0344).
    for (ty in c("parametric", "nonparametric", "robust")) {
        t <- pie_txt(jjpiestats(data = pie_sparse(), dep = "resp", group = "arm",
                                typestatistics = ty, showexplanations = TRUE)$report$content)
        expect_match(t, "Pearson's chi-squared test", fixed = TRUE, info = ty)
        expect_false(grepl("Fisher", t, fixed = TRUE), info = ty)
    }
    t <- pie_txt(jjpiestats(data = pie_sparse(), dep = "resp", group = "arm",
                            typestatistics = "bayes", showexplanations = TRUE)$report$content)
    expect_match(t, "Bayesian", fixed = TRUE)
})

test_that("the three frequentist types really are the same test", {
    # This is the measurement the fix rests on; if ggstatsplot ever gains an exact
    # option this test fails and the wording above must be revisited.
    d <- pie_sparse()
    sub <- function(ty) paste(deparse(ggstatsplot::ggpiestats(
        data = d, x = resp, y = arm, type = ty, results.subtitle = TRUE)$labels$subtitle),
        collapse = "")
    expect_identical(sub("parametric"), sub("nonparametric"))
    expect_identical(sub("parametric"), sub("robust"))
    expect_false(identical(sub("parametric"), sub("bayes")))
})

test_that("the interpretation guide no longer promises an exact test", {
    t <- pie_txt(jjpiestats(data = pie_sparse(), dep = "resp", group = "arm",
                            typestatistics = "nonparametric",
                            showInterpretation = TRUE)$interpretation$content)
    expect_false(grepl("Fisher's exact test provides precise", t, fixed = TRUE))
    expect_match(t, "same Pearson chi-square as Parametric")
})


# ---- the assumptions panel hands over the real exact p ----------------------

test_that("a sparse table gets the exact test, not a redirect to a dead option", {
    d <- pie_sparse()
    oracle <- stats::fisher.test(table(d$resp, d$arm))$p.value
    expect_equal(round(oracle, 4), 0.0511, tolerance = 1e-4)          # pins the fixture

    t <- pie_txt(jjpiestats(data = d, dep = "resp", group = "arm",
                            showAssumptions = TRUE)$assumptions$content)
    expect_match(t, "1 of 4 expected cell counts are below 5")
    # For a 2x2 the exact test now replaces the chart subtitle, so the panel
    # points at the figure rather than repeating the number; the p-value itself
    # is asserted against fisher.test in the plot test further down.
    expect_match(t, "subtitle therefore reports Fisher's exact test")
    expect_false(grepl("nonparametric option", t, fixed = TRUE))
})

test_that("an adequately powered table raises no assumption warning", {
    t <- pie_txt(jjpiestats(data = pie_wide(), dep = "resp", group = "arm",
                            showAssumptions = TRUE)$assumptions$content)
    expect_false(grepl("Chi-square is unreliable", t, fixed = TRUE))
})


# ---- expected proportions were validated in the wrong lifecycle phase -------

test_that("every malformed expected-proportions entry is reported", {
    # The guards ran inside .plot1/.plot2/.plot4, i.e. during .plot(), where
    # jamovi discards notices - so the proportion test silently used equal
    # proportions and said nothing. Four distinct failure modes, all silent.
    d <- pie_wide()   # resp has 4 levels
    cases <- list(
        list(r = "0.5,0.5,0.5,0.5", want = "sums to"),
        list(r = "0.5,0.5",         want = "level"),
        list(r = "abc,1,2,3",       want = "not a list of numbers"),
        list(r = "-1,1,1,1",        want = "zero or negative")
    )
    for (cs in cases) {
        n <- pie_notices(jjpiestats(data = d, dep = "resp", proportiontest = TRUE, ratio = cs$r))
        expect_match(n, "Expected proportions ignored", info = cs$r)
        expect_match(n, cs$want, info = cs$r)
    }
})

test_that("a valid expected-proportions entry is accepted silently", {
    n <- pie_notices(jjpiestats(data = pie_wide(), dep = "resp",
                                proportiontest = TRUE, ratio = "0.25,0.25,0.25,0.25"))
    expect_false(grepl("Expected proportions ignored", n, fixed = TRUE))
})


# ---- setups that cannot mean anything ---------------------------------------

test_that("crossing a variable with itself is called out", {
    # The contingency table is diagonal by construction, so the association test
    # returns p ~ 0 from the setup rather than from the data. It ran silently.
    set.seed(7)
    d <- data.frame(a = factor(sample(c("X", "Y", "Z"), 90, TRUE)))
    n <- pie_notices(jjpiestats(data = d, dep = "a", group = "a"))
    expect_match(n, "Variable compared with itself")
    expect_match(n, "diagonal by construction")
})

test_that("paired mode explains the missing single-variable chart", {
    # plot1 has no grouping variable, so `paired` makes it undrawable. The
    # explanation was raised from .plot1 and therefore never displayed.
    d <- pie_sparse()
    n <- pie_notices(jjpiestats(data = d, dep = "resp", group = "arm", paired = TRUE))
    expect_match(n, "Single-variable pie chart not shown")
})

test_that("a pie chart with too many slices is flagged", {
    set.seed(3)
    d <- data.frame(g = factor(paste0("L", sample(1:12, 240, TRUE))))
    expect_match(pie_notices(jjpiestats(data = d, dep = "g")), "Too many slices to read")
    d2 <- data.frame(g = factor(sample(c("A", "B", "C"), 90, TRUE)))
    expect_false(grepl("Too many slices", pie_notices(jjpiestats(data = d2, dep = "g")), fixed = TRUE))
})


# ---- statistics -------------------------------------------------------------

test_that("the reported chi-square matches stats::chisq.test", {
    d <- pie_wide()
    tb <- table(d$resp, d$arm)
    ref <- suppressWarnings(stats::chisq.test(tb, correct = FALSE))
    sub <- paste(deparse(ggstatsplot::ggpiestats(data = d, x = resp, y = arm,
                                                 results.subtitle = TRUE)$labels$subtitle),
                 collapse = "")
    # NB: the statistic and n are quoted in the plotmath expression but df is
    # interpolated bare, as `* 3 *`.
    expect_match(sub, sprintf('"%s"', round(ref$statistic, 2)), fixed = TRUE)
    expect_match(sub, sprintf('* %d *', unname(ref$parameter)), fixed = TRUE)
    expect_match(sub, sprintf('"%d"', sum(tb)), fixed = TRUE)
})

test_that("paired mode really runs McNemar, matching stats::mcnemar.test", {
    set.seed(5)
    n <- 60
    pre  <- sample(c("Pos", "Neg"), n, TRUE, prob = c(.4, .6))
    post <- ifelse(pre == "Neg" & runif(n) < .35, "Pos",
                   ifelse(pre == "Pos" & runif(n) < .1, "Neg", pre))
    d <- data.frame(pre = factor(pre), post = factor(post))
    ref <- stats::mcnemar.test(table(d$pre, d$post), correct = FALSE)
    sub <- paste(deparse(ggstatsplot::ggpiestats(data = d, x = pre, y = post, paired = TRUE,
                                                 results.subtitle = TRUE)$labels$subtitle),
                 collapse = "")
    expect_match(sub, "McNemar", fixed = TRUE)
    expect_match(sub, sprintf('"%s"', round(ref$statistic, 2)), fixed = TRUE)
})

test_that("the weighted counts path equals the expanded raw data", {
    d <- pie_wide()
    agg <- as.data.frame(table(d$resp, d$arm)); names(agg) <- c("resp", "arm", "n")
    raw <- suppressWarnings(stats::chisq.test(table(d$resp, d$arm), correct = FALSE))
    wtd <- suppressWarnings(stats::chisq.test(stats::xtabs(n ~ resp + arm, agg), correct = FALSE))
    expect_equal(unname(wtd$statistic), unname(raw$statistic))
    expect_no_error(jjpiestats(data = agg, dep = "resp", group = "arm", counts = "n"))
})

test_that("the effect-size guide says which Cramer's V is on screen", {
    # ggstatsplot shows the BIAS-CORRECTED V (Bergsma 2013); Cohen's 0.10/0.30/0.50
    # landmarks describe the classical one. Measured on a 4x2 table with n = 200:
    # classical V = 0.15 ("small"), corrected V = 0.08, which those thresholds
    # would dismiss as negligible.
    t <- pie_txt(jjpiestats(data = pie_wide(), dep = "resp", group = "arm",
                            showInterpretation = TRUE)$interpretation$content)
    expect_match(t, "bias-corrected")
    expect_match(t, "Bergsma")
})


# ---- generated syntax -------------------------------------------------------

test_that("the syntax pane emits valid, non-duplicated R", {
    # .asArgs() wrote variable names as bare symbols, so a column called
    # `Tumor Grade ("high")` produced dep = Tumor Grade ("high") - a parse error
    # for anyone who copied the syntax out of jamovi.
    set.seed(4)
    d <- data.frame(a = factor(sample(c("X", "Y"), 40, TRUE)),
                    b = factor(sample(c("P", "Q"), 40, TRUE)))
    names(d) <- c('Tumor Grade ("high")', "Arm")
    a <- ClinicoPath:::jjpiestatsClass$new(
        options = ClinicoPath:::jjpiestatsOptions$new(dep = 'Tumor Grade ("high")', group = "Arm"),
        data = d)
    src <- a$asSource()
    expect_silent(parse(text = src))
    expect_equal(lengths(regmatches(src, gregexpr("dep =", src)))[[1]], 1L)
    expect_equal(lengths(regmatches(src, gregexpr("group =", src)))[[1]], 1L)
})


# ---- the chart itself now carries the valid test ----------------------------

pie_plot_txt <- function(res, item = "plot2") {
    f <- tempfile(fileext = ".svg"); svglite::svglite(f, 8, 6)
    on.exit(unlink(f), add = TRUE)
    print(res[[item]])
    grDevices::dev.off()          # MUST close before reading: svglite buffers,
    x <- readLines(f, warn = FALSE)   # so an unflushed file loses the subtitle
    paste(gsub("^>|</text>$", "", regmatches(x, regexpr(">[^<]*</text>", x))), collapse = " ")
}

test_that("a sparse 2x2 chart reports Fisher's exact test, not chi-square", {
    # Previously the figure showed chi2(1) = 4.47, p = 0.03 and the Assumptions
    # panel had to tell the reader to disregard it. A chart gets pasted into a
    # slide deck without its panel, so the wrong number travelled alone.
    d <- pie_sparse()
    ft <- stats::fisher.test(table(d$resp, d$arm))
    t <- pie_plot_txt(jjpiestats(data = d, dep = "resp", group = "arm",
                                 resultssubtitle = TRUE))
    expect_match(t, "Fisher")
    expect_match(t, formatC(ft$p.value, format = "f", digits = 3), fixed = TRUE)
    expect_match(t, formatC(unname(ft$estimate), format = "f", digits = 2), fixed = TRUE)
    expect_false(grepl("Pearson", t, fixed = TRUE))
})

test_that("an adequately powered table keeps the chi-square subtitle", {
    t <- pie_plot_txt(jjpiestats(data = pie_wide(), dep = "resp", group = "arm",
                                 resultssubtitle = TRUE))
    expect_match(t, "Pearson")
    expect_false(grepl("Fisher", t, fixed = TRUE))
})

test_that("paired and Bayesian analyses are left alone", {
    # McNemar is already the right test for paired data, and the Bayesian
    # subtitle is not a chi-square - neither should be overwritten.
    set.seed(5)
    n <- 60
    pre  <- sample(c("Pos", "Neg"), n, TRUE, prob = c(.4, .6))
    post <- ifelse(pre == "Neg" & runif(n) < .35, "Pos",
                   ifelse(pre == "Pos" & runif(n) < .1, "Neg", pre))
    dp <- data.frame(pre = factor(pre), post = factor(post))
    expect_match(pie_plot_txt(jjpiestats(data = dp, dep = "pre", group = "post",
                                         paired = TRUE, resultssubtitle = TRUE)),
                 "McNemar")

    tb <- pie_plot_txt(jjpiestats(data = pie_sparse(), dep = "resp", group = "arm",
                                  typestatistics = "bayes", resultssubtitle = TRUE))
    expect_false(grepl("Fisher", tb, fixed = TRUE))
})

test_that("the assumptions panel agrees with whatever the chart shows", {
    # The panel used to assert unconditionally that the subtitle was a chi-square.
    # Once the subtitle can be swapped, saying so regardless would recreate the
    # contradiction this whole review is about.
    swapped <- pie_txt(jjpiestats(data = pie_sparse(), dep = "resp", group = "arm",
                                  showAssumptions = TRUE)$assumptions$content)
    expect_match(swapped, "subtitle therefore reports Fisher's exact test")
    expect_false(grepl("quote that value, not the subtitle", swapped, fixed = TRUE))

    # A sparse table that is NOT 2x2 gets no swap, so the panel must still hand
    # over the exact p-value and say the subtitle is unchanged.
    d3 <- data.frame(resp = factor(rep(c("A", "B", "C"), times = c(3, 12, 12))),
                     arm  = factor(rep(c("X", "Y"), times = c(13, 14))))
    not_swapped <- pie_txt(jjpiestats(data = d3, dep = "resp", group = "arm",
                                      showAssumptions = TRUE)$assumptions$content)
    expect_match(not_swapped, "still reports an uncorrected Pearson chi-square")
    expect_match(not_swapped, "quote that value, not the subtitle")
})

test_that("p-values are formatted without a stray equals sign", {
    d3 <- data.frame(resp = factor(rep(c("A", "B", "C"), times = c(3, 12, 12))),
                     arm  = factor(rep(c("X", "Y"), times = c(13, 14))))
    t <- as.character(jjpiestats(data = d3, dep = "resp", group = "arm",
                                 showAssumptions = TRUE)$assumptions$content)
    expect_false(grepl("p = &lt;", t, fixed = TRUE))
    expect_match(t, "p &lt; 0.001", fixed = TRUE)
})

test_that("Split By panels get their own exact p-values", {
    # grouped_ggpiestats returns a combined patchwork whose per-panel subtitles
    # cannot be swapped the way plot2's single subtitle is, so the exact values
    # are surfaced in the notices instead.
    set.seed(2)
    n <- 60
    ds <- data.frame(resp = factor(sample(c("Yes", "No"), n, TRUE, c(.15, .85))),
                     arm  = factor(rep(c("A", "B"), each = n / 2)),
                     site = factor(rep(c("S1", "S2"), n / 2)))
    n_txt <- pie_notices(jjpiestats(data = ds, dep = "resp", group = "arm", grvar = "site"))
    expect_match(n_txt, "Split By panels")
    expect_match(n_txt, "S1: p =")
    expect_match(n_txt, "S2: p =")
})

test_that("a well-powered Split By raises no per-panel notice", {
    set.seed(12)
    ds <- data.frame(resp = factor(sample(c("Yes", "No"), 400, TRUE)),
                     arm  = factor(sample(c("A", "B"), 400, TRUE)),
                     site = factor(rep(c("S1", "S2"), 200)))
    expect_false(grepl("Split By panels",
                       pie_notices(jjpiestats(data = ds, dep = "resp", group = "arm",
                                              grvar = "site")), fixed = TRUE))
})
