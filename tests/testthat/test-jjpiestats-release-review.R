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
                            showAssumptions = TRUE, resultssubtitle = TRUE)$assumptions$content)
    expect_match(t, "1 of 4 expected cell counts are below 5")
    # For a 2x2 the exact test now replaces the chart subtitle, so the panel
    # points at the figure rather than repeating the number; the p-value itself
    # is asserted against fisher.test in the plot test further down.
    expect_match(t, "subtitle therefore reports Fisher's exact test")
    expect_false(grepl("nonparametric option", t, fixed = TRUE))
})

test_that("an adequately powered table raises no assumption warning", {
    t <- pie_txt(jjpiestats(data = pie_wide(), dep = "resp", group = "arm",
                            showAssumptions = TRUE, resultssubtitle = TRUE)$assumptions$content)
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
    # McNemar is the right test for paired data and the Bayesian subtitle is not
    # a chi-square - neither may be overwritten. This paired table is not sparse;
    # a sparse paired one (Bentur, smallest expected count 1.3) is checked in
    # "the paired chart keeps McNemar's asymptotic test at any b + c".
    dp <- data.frame(
        pre  = factor(rep(c("Neg", "Neg", "Pos", "Pos"), times = c(40, 30, 25, 40))),
        post = factor(rep(c("Neg", "Pos", "Neg", "Pos"), times = c(40, 30, 25, 40))))
    t_paired <- pie_plot_txt(jjpiestats(data = dp, dep = "pre", group = "post",
                                        paired = TRUE, resultssubtitle = TRUE))
    expect_match(t_paired, "McNemar")
    expect_false(grepl("exact", t_paired, fixed = TRUE))

    tb <- pie_plot_txt(jjpiestats(data = pie_sparse(), dep = "resp", group = "arm",
                                  typestatistics = "bayes", resultssubtitle = TRUE))
    expect_false(grepl("Fisher", tb, fixed = TRUE))
})

test_that("the assumptions panel agrees with whatever the chart shows", {
    # The panel used to assert unconditionally that the subtitle was a chi-square.
    # Once the subtitle can be swapped, saying so regardless would recreate the
    # contradiction this whole review is about.
    swapped <- pie_txt(jjpiestats(data = pie_sparse(), dep = "resp", group = "arm",
                                  showAssumptions = TRUE, resultssubtitle = TRUE)$assumptions$content)
    expect_match(swapped, "subtitle therefore reports Fisher's exact test")
    expect_false(grepl("quote that value, not the subtitle", swapped, fixed = TRUE))

    # A sparse table that is NOT 2x2 gets no swap, so the panel must still hand
    # over the exact p-value and say the subtitle is unchanged.
    d3 <- data.frame(resp = factor(rep(c("A", "B", "C"), times = c(3, 12, 12))),
                     arm  = factor(rep(c("X", "Y"), times = c(13, 14))))
    not_swapped <- pie_txt(jjpiestats(data = d3, dep = "resp", group = "arm",
                                      showAssumptions = TRUE, resultssubtitle = TRUE)$assumptions$content)
    expect_match(not_swapped, "still reports an uncorrected Pearson chi-square")
    expect_match(not_swapped, "quote that value, not the subtitle")
})

test_that("p-values are formatted without a stray equals sign", {
    d3 <- data.frame(resp = factor(rep(c("A", "B", "C"), times = c(3, 12, 12))),
                     arm  = factor(rep(c("X", "Y"), times = c(13, 14))))
    t <- as.character(jjpiestats(data = d3, dep = "resp", group = "arm",
                                 showAssumptions = TRUE, resultssubtitle = TRUE)$assumptions$content)
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


# ---- check-function pass, 2026-09-06 ----------------------------------------

test_that("the biomarker interpretation sentence is not truncated by the translator", {
    # jmvcore's .() treats ' [ctx]' as a msgctxt marker and drops everything from
    # the space-bracket on when no catalog entry matches, so
    # 'OR = 4.3 [2.1-8.7]) describes ...' lost its CI and its whole second half.
    # Raw content, not pie_txt(): the panel legitimately contains "p < 0.001",
    # and the helper's tag-stripper would eat everything after that "<".
    t <- as.character(jjpiestats(data = pie_wide(), dep = "resp", group = "arm",
                                 clinicalpreset = "biomarker",
                                 showInterpretation = TRUE)$interpretation$content)
    expect_match(t, "independent cohort", fixed = TRUE)
    expect_match(t, "95% CI", fixed = TRUE)
})

test_that("the weighted donut chart accepts names with spaces and punctuation", {
    # as.data.frame(xtabs()) runs make.names() on the dimension names, so the
    # aggregated table came back as 'Tumor.Grade' / 'Arm..x.' and
    # ggdonutchart(label = dep) could not find 'Tumor Grade'.
    d <- data.frame(g = factor(c("low", "high", "low", "high")),
                    a = factor(c("A", "A", "B", "B")),
                    n = c(3, 5, 2, 6))
    names(d) <- c("Tumor Grade", "Arm (x)", "n counts")
    res <- jjpiestats(data = d, dep = "Tumor Grade", group = "Arm (x)",
                      counts = "n counts", addGGPubrDonut = TRUE)
    t <- pie_plot_txt(res, "donutPlot")
    expect_match(t, "low", fixed = TRUE)
    expect_match(t, "high", fixed = TRUE)
})

test_that("a paired request on a table that is not 2x2 says why nothing was drawn", {
    # .validatePairedData ran only inside .plot2/.plot4, i.e. during .plot(),
    # where its notice is discarded - the chart was blank and silent.
    d3 <- data.frame(resp = factor(rep(c("A", "B", "C"), times = c(10, 12, 12))),
                     arm  = factor(rep(c("X", "Y"), times = c(17, 17))))
    n <- pie_notices(jjpiestats(data = d3, dep = "resp", group = "arm", paired = TRUE))
    expect_match(n, "Paired analysis not shown", fixed = TRUE)
    expect_match(n, "3\u00d72", fixed = TRUE)
})

test_that("the single-variable pie chart shows its goodness-of-fit subtitle", {
    # `opts$ratio` partial-matched the sibling key `ratio_raw`, so an empty
    # Expected-proportions box sent ratio = "" into ggpiestats(); the
    # goodness-of-fit test errored inside statsExpressions, ggstatsplot
    # swallowed the error, and plot1 rendered with no subtitle at all - which
    # made digits, conflevel, bfmessage and proportiontest silent no-ops on
    # that chart. The unpatched chart text was "62% 38% resp No Yes".
    d <- data.frame(resp = factor(rep(c("Yes", "No"), c(60, 100))))
    t <- pie_plot_txt(jjpiestats(data = d, dep = "resp",
                                 resultssubtitle = TRUE), "plot1")
    expect_match(t, "gof", fixed = TRUE)
    expect_match(t, "160", fixed = TRUE)
    t4 <- pie_plot_txt(jjpiestats(data = d, dep = "resp",
                                  resultssubtitle = TRUE, digits = 4), "plot1")
    expect_false(identical(t, t4))
})

# ---- McNemar: the recommended test at any number of discordant pairs ---------

# The chart keeps the plotting package's asymptotic McNemar test without
# continuity correction, (b - c)^2 / (b + c) on 1 df. Fagerland, Lydersen & Laake
# (BMC Med Res Methodol 2013;13:91) recommend it (and the mid-p test) and advise
# against the exact binomial and continuity-corrected tests "in any situation".
# The old code swapped in the exact binomial below 25 discordant pairs, which
# turned Fagerland's Table 6 examples non-significant: Bentur (b = 1, c = 7)
# asymptotic p = 0.0339 vs exact 0.0703; Cavo (6, 16) 0.0330 vs 0.0525.
# Retiring a rule: assert its phrases are gone, not just that a notice exists.
pie_mcn_retired <- c("exact binomial", "anti-conservative", "below about 25",
                     "quote that value", "McNemar's exact test", "exact counterpart")

# Aggregated paired 2x2: a = (Neg, Neg), b = (Neg -> Pos), c = (Pos -> Neg), d = (Pos, Pos).
pie_mcn_tab <- function(a, b, c, d) {
    tb <- matrix(c(a, c, b, d), 2, 2,
                 dimnames = list(Pre = c("Neg", "Pos"), Post = c("Neg", "Pos")))
    out <- as.data.frame(as.table(tb))
    names(out) <- c("Pre", "Post", "n")
    out
}
pie_mcn <- function(v, ...) jjpiestats(data = pie_mcn_tab(v[1], v[2], v[3], v[4]),
                                       dep = "Pre", group = "Post", counts = "n",
                                       paired = TRUE, ...)

test_that("the paired chart keeps McNemar's asymptotic test at any b + c", {
    # n = 206 with 6 discordant pairs: chi2 = 2.67, p = 0.1025; the retired exact
    # swap showed p = 0.219 instead.
    for (cs in list(list(v = c(2, 1, 7, 11),    chi = "4.50", old = "0.070"),   # Bentur
                    list(v = c(59, 6, 16, 80),  chi = "4.55", old = "0.052"),   # Cavo
                    list(v = c(100, 1, 5, 100), chi = "2.67", old = "0.219"))) {
        ref <- stats::mcnemar.test(matrix(cs$v[c(1, 3, 2, 4)], 2), correct = FALSE)
        expect_identical(sprintf("%.2f", unname(ref$statistic)), cs$chi)
        t <- pie_plot_txt(pie_mcn(cs$v, resultssubtitle = TRUE), "plot2")
        expect_match(t, "McNemar", fixed = TRUE, info = cs$chi)
        expect_match(t, cs$chi, fixed = TRUE)
        expect_false(grepl("exact", t, fixed = TRUE), info = cs$chi)
        expect_false(grepl(cs$old, t, fixed = TRUE), info = cs$chi)
    }
})

test_that("the notices say how much b + c can show, never that the test is invalid", {
    notes <- function(v) pie_notices(pie_mcn(v))
    n6 <- notes(c(100, 1, 5, 100))
    expect_match(n6, "Only 6 discordant pairs, so McNemar's test has little power", fixed = TRUE)
    for (r in pie_mcn_retired) expect_false(grepl(r, n6, fixed = TRUE), info = r)
    # smallest asymptotic p with m pairs is P(chi2(1) > m): 0.083 at m = 3, 0.0455 at m = 4
    expect_match(notes(c(20, 3, 0, 20)), "Only 3 discordant pairs: with so few, McNemar's test cannot reach p < 0.05", fixed = TRUE)
    n4 <- notes(c(20, 4, 0, 20))
    expect_false(grepl("cannot reach", n4, fixed = TRUE))
    expect_match(n4, "Only 4 discordant pairs, so McNemar's test has little power", fixed = TRUE)
    expect_match(notes(c(20, 1, 0, 20)), "Only 1 discordant pair: McNemar's test cannot reach p < 0.05", fixed = TRUE)
    expect_false(grepl("discordant", notes(c(20, 9, 21, 20)), fixed = TRUE))   # m = 30
    expect_false(grepl("discordant", notes(c(20, 5, 5, 20)), fixed = TRUE))    # m = 10, tie
})

test_that("no discordant pairs is said, not left as a silent blank subtitle", {
    r <- pie_mcn(c(20, 0, 0, 20), resultssubtitle = TRUE)
    expect_match(pie_notices(r), "No discordant pairs", fixed = TRUE)
    expect_match(pie_notices(r), "p is 1 by convention", fixed = TRUE)
    expect_false(grepl("McNemar", pie_plot_txt(r, "plot2"), fixed = TRUE))
})

test_that("Methods and Assumptions name the asymptotic test without the retired rule", {
    r <- pie_mcn(c(100, 1, 5, 100), resultssubtitle = TRUE, showexplanations = TRUE)
    rep_txt <- pie_txt(r$report$content)
    expect_match(rep_txt, "McNemar's test without continuity correction", fixed = TRUE)
    expect_false(grepl("exact", rep_txt, fixed = TRUE))
    a <- pie_txt(r$assumptions$content)
    expect_match(a, "Fagerland et al. (2013)", fixed = TRUE)
    expect_match(a, "Pairs must be independent", fixed = TRUE)
    for (p in c("anti-conservative", "below about 25", "quote that value"))
        expect_false(grepl(p, a, fixed = TRUE), info = p)
    # The Interpretation panel used to send paired runs to "the exact p-value
    # reported in the Assumptions panel", which no longer exists for paired data.
    for (ty in c("parametric", "nonparametric", "robust")) {
        g <- pie_txt(pie_mcn(c(30, 6, 1, 3), typestatistics = ty,
                             showInterpretation = TRUE)$interpretation$content)
        expect_match(g, "McNemar's test compares the pairs", fixed = TRUE, info = ty)
        for (p in c("exact p-value reported", "Pearson chi-square"))
            expect_false(grepl(p, g, fixed = TRUE), info = paste(ty, p))
    }
})

test_that("a zero margin is analysed, and fewer than 10 pairs are refused without exact advice", {
    # zero row margin (b = 5, c = 0): chi2 = 5.00, p = 0.0253 - only b and c matter
    rz <- pie_mcn(c(10, 5, 0, 0), resultssubtitle = TRUE)
    expect_false(grepl("Paired analysis not shown", pie_notices(rz), fixed = TRUE))
    expect_match(pie_plot_txt(rz, "plot2"), "5.00", fixed = TRUE)
    # n = 8: below the 10 to 100 pairs Fagerland et al. evaluated
    n8 <- pie_notices(pie_mcn(c(2, 3, 1, 2)))
    expect_match(n8, "not shown for fewer than 10 paired observations; your data has 8", fixed = TRUE)
    for (r in pie_mcn_retired) expect_false(grepl(r, n8, fixed = TRUE), info = r)
})

test_that("a Bayesian paired run is not labelled McNemar", {
    # statsExpressions ignores `paired` for type = "bayes": the chart is an
    # independence Bayes factor.
    r <- pie_mcn(c(20, 2, 8, 20), typestatistics = "bayes", showexplanations = TRUE)
    expect_match(pie_notices(r), "Pairing not used by the Bayesian analysis", fixed = TRUE)
    expect_false(grepl("McNemar", pie_txt(r$summary$content), fixed = TRUE))
    rep_txt <- pie_txt(r$report$content)
    expect_match(rep_txt, "Bayesian", fixed = TRUE)
    expect_false(grepl("McNemar", rep_txt, fixed = TRUE))
})

test_that("option changes on a reused analysis instance are not served from a stale cache", {
    # .prepareOptions() cached without an invalidation key, so on the SAME R6
    # instance - which is what jamovi reuses across option changes - it returned
    # the first run's options forever and every renderer ignored later edits.
    # The public wrapper builds a fresh analysis each call, so only the GUI saw it.
    set.seed(1)
    d <- data.frame(resp = factor(sample(c("Yes", "No"), 120, TRUE)),
                    arm  = factor(sample(c("A", "B"), 120, TRUE)))
    o <- ClinicoPath:::jjpiestatsOptions$new(dep = "resp", group = "arm",
                                             digits = 2, typestatistics = "parametric")
    a <- ClinicoPath:::jjpiestatsClass$new(options = o, data = d)
    a$run()
    priv <- a$.__enclos_env__$private
    expect_equal(priv$.prepareOptions()$digits, 2)

    for (op in o$.__enclos_env__$private$.options) {
        if (op$name == "digits") op$value <- 5L
        if (op$name == "typestatistics") op$value <- "bayes"
    }
    a$run()
    expect_equal(priv$.prepareOptions()$digits, 5)
    expect_equal(priv$.prepareOptions()$typestatistics, "bayes")
})
