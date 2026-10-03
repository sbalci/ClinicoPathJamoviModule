# Release review of jjbarstats.
#
# The defect this file guards: on a 2x2 table with low expected counts the
# backend used to flip `type` to "nonparametric" and announce "Automatically
# switched to Fisher's Exact Test". ggbarstats has no exact-test option, so the
# flip bought nothing - every non-Bayesian `type` returns the SAME uncorrected
# Pearson chi-squared - and the clinician was told an exact test had been run
# while the plot still showed the invalid one.

jbs_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x %||% "")))
jbs_assump <- function(res) jbs_txt(res$assumptions$content)

# 2x2 with one expected count below 5. Chosen because Pearson and Fisher fall on
# OPPOSITE sides of 0.05 here, which is the whole reason the old text was unsafe:
#          A   B          expected: 7.38 8.62 / 4.62 5.38
#   No    10   6          Pearson chi2(1) = 4.4726, p = 0.0344   <- what the plot shows
#   Yes    2   8          Fisher exact             p = 0.0511    <- the valid test
jbs_2x2 <- function() {
    data.frame(resp = factor(rep(c("Yes", "No", "Yes", "No"), times = c(2, 10, 8, 6))),
               arm  = factor(rep(c("A", "B"), times = c(12, 14))))
}


test_that("the sparse-2x2 panel reports the real Fisher p-value", {
    d <- jbs_2x2()
    oracle <- stats::fisher.test(table(d$resp, d$arm))$p.value
    expect_equal(round(oracle, 4), 0.0511, tolerance = 1e-4)   # pins the fixture

    t <- jbs_assump(jjbarstats(data = d, dep = "resp", group = "arm"))
    expect_match(t, "Chi-square Assumption Violated")
    expect_match(t, "1 of 4 cells")
    # For a 2x2 the exact test now REPLACES the chart subtitle, so the panel
    # points at the figure rather than repeating the number. The p-value itself
    # is asserted against fisher.test in the plot test further down.
    expect_match(t, "subtitle reports Fisher's exact test")
})

test_that("the panel no longer claims an exact test was substituted", {
    t <- jbs_assump(jjbarstats(data = jbs_2x2(), dep = "resp", group = "arm"))
    expect_false(grepl("Automatically switched", t, fixed = TRUE))
    expect_false(grepl("Auto-Selected", t, fixed = TRUE))
    expect_false(grepl("Using Fisher's Exact Test", t, fixed = TRUE))
    # and it states what the chart now shows instead
    expect_match(t, "Exact test used")
})

test_that("a table with adequate expected counts raises no assumption warning", {
    set.seed(7)
    d <- data.frame(resp = factor(sample(c("Yes", "No"), 200, TRUE)),
                    arm  = factor(sample(c("A", "B"), 200, TRUE)))
    expect_true(all(stats::chisq.test(table(d$resp, d$arm))$expected >= 5))
    expect_false(grepl("Assumption Violated", jbs_assump(jjbarstats(data = d, dep = "resp", group = "arm")),
                       fixed = TRUE))
})

test_that("a sparse table larger than 2x2 says an exact test is not offered", {
    set.seed(11)
    d <- data.frame(resp = factor(rep(c("I", "II", "III"), times = c(3, 12, 12))),
                    arm  = factor(rep(c("A", "B", "C"), times = c(9, 9, 9))))
    t <- jbs_assump(jjbarstats(data = d, dep = "resp", group = "arm"))
    expect_match(t, "Assumption Violated")
    expect_match(t, "only available for 2")     # "...for 2x2 tables"
    expect_false(grepl("Automatically switched", t, fixed = TRUE))
})

test_that("the removed frequentist aliases are rejected, not silently mapped", {
    # "nonparametric" and "robust" produced the very same Pearson chi-squared as
    # "parametric" on a contingency table, so the list now offers only the
    # frequentist test and the Bayesian analysis. An old value is an error, not
    # a silent fallback, and the frequentist choice is passed through untouched.
    d <- jbs_2x2()
    expect_error(jjbarstats(data = d, dep = "resp", group = "arm", typestatistics = "nonparametric"),
                 "must be one of")
    expect_error(jjbarstats(data = d, dep = "resp", group = "arm", typestatistics = "robust"),
                 "must be one of")
    p <- jjbarstats(data = d, dep = "resp", group = "arm", typestatistics = "parametric")$plot
    f <- tempfile(fileext = ".svg"); svglite::svglite(f, 8, 6)
    ok <- tryCatch({ print(p); TRUE }, error = function(e) conditionMessage(e))
    grDevices::dev.off(); unlink(f)
    expect_true(isTRUE(ok))
})

test_that("multiple dependent variables each get their own assumption block", {
    # .checkStatisticalAssumptions used to be written from the plot loop, so the
    # last dependent variable overwrote the panel for the others.
    d <- jbs_2x2()
    d$resp2 <- factor(rep(c("Yes", "No"), times = c(13, 13)))
    t <- jbs_assump(jjbarstats(data = d, dep = c("resp", "resp2"), group = "arm"))
    expect_match(t, "resp")
    expect_equal(lengths(regmatches(t, gregexpr("Chi-square Assumption Violated", t)))[[1]], 1L)
})


# ---- the chart itself now carries the valid test ----------------------------
#
# Ported from jjpiestats. The earlier pass mitigated the invalid subtitle by
# DISCLOSURE only - the assumptions panel reported the real Fisher p and told the
# reader to disregard the figure. That is weaker than it needs to be, because a
# chart gets pasted into a slide deck without its panel, so the wrong number
# travelled alone.

jbs_plot_txt <- function(res, item = "plot") {
    f <- tempfile(fileext = ".svg"); svglite::svglite(f, 8, 6)
    on.exit(unlink(f), add = TRUE)
    print(res[[item]])
    grDevices::dev.off()          # MUST close before reading: svglite buffers,
    x <- readLines(f, warn = FALSE)   # so an unflushed file loses the subtitle
    paste(gsub("^>|</text>$", "", regmatches(x, regexpr(">[^<]*</text>", x))), collapse = " ")
}

test_that("a sparse 2x2 chart reports Fisher's exact test, not chi-squared", {
    d <- jbs_2x2()
    ft <- stats::fisher.test(table(d$resp, d$arm))
    t <- jbs_plot_txt(jjbarstats(data = d, dep = "resp", group = "arm",
                                 resultssubtitle = TRUE))
    expect_match(t, "Fisher")
    expect_match(t, formatC(ft$p.value, format = "f", digits = 3), fixed = TRUE)
    expect_match(t, formatC(unname(ft$estimate), format = "f", digits = 2), fixed = TRUE)
    expect_false(grepl("Pearson", t, fixed = TRUE))
})

test_that("an adequately powered table keeps the chi-squared subtitle", {
    set.seed(11)
    d <- data.frame(resp = factor(sample(c("Yes", "No"), 200, TRUE)),
                    arm  = factor(sample(c("A", "B"), 200, TRUE)))
    expect_true(all(stats::chisq.test(table(d$resp, d$arm))$expected >= 5))
    t <- jbs_plot_txt(jjbarstats(data = d, dep = "resp", group = "arm",
                                 resultssubtitle = TRUE))
    expect_match(t, "Pearson")
    expect_false(grepl("Fisher", t, fixed = TRUE))
})

test_that("the assumptions panel agrees with whatever the chart shows", {
    # Once the subtitle can be swapped, telling the reader to disregard it would
    # recreate the very contradiction this fix removes.
    swapped <- jbs_assump(jjbarstats(data = jbs_2x2(), dep = "resp", group = "arm"))
    expect_match(swapped, "Exact test used")
    expect_match(swapped, "subtitle reports Fisher's exact test")
    expect_false(grepl("not the plot subtitle", swapped, fixed = TRUE))

    # A sparse table that is NOT 2x2 gets no swap, so the panel must still say the
    # subtitle is a chi-squared and that an exact test is unavailable for it.
    set.seed(11)
    d3 <- data.frame(resp = factor(rep(c("I", "II", "III"), times = c(3, 12, 12))),
                     arm  = factor(rep(c("A", "B", "C"), times = c(9, 9, 9))))
    not_swapped <- jbs_assump(jjbarstats(data = d3, dep = "resp", group = "arm"))
    expect_match(not_swapped, "only available for 2")
    expect_false(grepl("Exact test used", not_swapped, fixed = TRUE))
})

test_that("paired and Bayesian analyses are left alone", {
    # McNemar is already correct for paired data, and a Bayesian subtitle is not a
    # chi-squared - neither should be overwritten.
    tb <- jbs_plot_txt(jjbarstats(data = jbs_2x2(), dep = "resp", group = "arm",
                                  typestatistics = "bayes", resultssubtitle = TRUE))
    expect_false(grepl("Fisher", tb, fixed = TRUE))
})

test_that("the split-by chart keeps the disclosure route", {
    # grouped_ggbarstats returns a combined patchwork whose per-panel subtitles
    # cannot be replaced the way the single chart's is, so that path must still
    # tell the user to quote the exact p-value instead.
    d <- jbs_2x2()
    d$site <- factor(rep(c("S1", "S2"), length.out = nrow(d)))
    res <- jjbarstats(data = d, dep = "resp", group = "arm", grvar = "site")
    expect_no_error(res$plot2)
})

test_that("with several dependent variables the decision is per-table", {
    # jjbarstats draws one panel per dependent variable, and the exact-test
    # decision belongs to each variable's own contingency table - not to the
    # analysis as a whole. A sparse variable and a well-powered one must be able
    # to carry different subtitles in the same figure.
    set.seed(1)
    d <- data.frame(
        resp1 = factor(rep(c("Yes", "No", "Yes", "No"), times = c(2, 10, 8, 6))),  # sparse
        resp2 = factor(rep(c("Yes", "No"), 13)),                                    # balanced
        arm   = factor(rep(c("A", "B"), times = c(12, 14))))

    f <- tempfile(fileext = ".svg"); svglite::svglite(f, 12, 6)
    on.exit(unlink(f), add = TRUE)
    print(jjbarstats(data = d, dep = c("resp1", "resp2"), group = "arm",
                     resultssubtitle = TRUE)$plot)
    grDevices::dev.off()
    x <- readLines(f, warn = FALSE)
    t <- paste(gsub("^>|</text>$", "", regmatches(x, regexpr(">[^<]*</text>", x))), collapse = " ")

    expect_match(t, "Fisher")    # the sparse variable
    expect_match(t, "Pearson")   # the well-powered one
})


# ---- 2026-09-06 release review: regression cover for the day's fixes -----------------

test_that("subtitle statistics agree with base R and effectsize on the bundled data", {
    data(jjbarstats_test)
    d <- as.data.frame(jjbarstats_test); tb <- table(d$response, d$treatment)
    t <- jbs_plot_txt(jjbarstats(data = d, dep = "response", group = "treatment",
                                 resultssubtitle = TRUE))
    ct <- stats::chisq.test(tb, correct = FALSE)
    expect_match(t, formatC(unname(ct$statistic), format = "f", digits = 2), fixed = TRUE)
    V <- effectsize::cramers_v(tb, adjust = TRUE)
    expect_match(t, formatC(V$Cramers_v_adjusted, format = "f", digits = 2), fixed = TRUE)
})

test_that("the interpretation guide scales Cramer's V cut-offs to the table", {
    data(jjbarstats_test)
    d <- as.data.frame(jjbarstats_test)
    strip <- function(h) gsub("\\s+", " ", gsub("<[^>]*>", " ", as.character(h)))
    g3 <- strip(jjbarstats(data = d, dep = "response", group = "treatment",
                           showInterpretation = TRUE)$interpretation$content)
    expect_match(g3, "bias-corrected")
    expect_match(g3, "0.07 (small), 0.21 (medium), 0.35 (large)", fixed = TRUE)   # df* = 2
    d2 <- droplevels(subset(d, response != "Partial Response" & treatment != "Low Dose"))
    g2 <- strip(jjbarstats(data = d2, dep = "response", group = "treatment",
                           showInterpretation = TRUE)$interpretation$content)
    expect_match(g2, "0.1 (small), 0.3 (medium), 0.5 (large)", fixed = TRUE)      # df* = 1
})

# The chart shows the asymptotic McNemar test without continuity correction
# (statsExpressions: mcnemar.test(correct = FALSE)), which Fagerland, Lydersen &
# Laake (BMC Med Res Methodol 2013;13:91) recommend; they advise against the
# exact binomial test "in any situation". The notices therefore say how much
# b + c can show (cannot reach p < 0.05 at m <= 3, little power below 10) and
# never advise the exact test. A retired rule is guarded by asserting its
# phrases are gone: the new low-power notice reuses the title "Few discordant
# pairs", so a title-only check passed under the old rule and the new one alike.
bar_mcn_retired <- c("exact binomial", "anti-conservative", "below 25", "unreliable")
bar_mcn <- function(v, ...) {
    ab <- c("Y", "N", "Y", "N")      # pairs: (Y,Y) a, (N,Y) b, (Y,N) c, (N,N) d
    d <- data.frame(a = factor(rep(ab, v)), b = factor(rep(c("Y", "Y", "N", "N"), v)))
    jjbarstats(data = d, dep = "a", group = "b", paired = TRUE, ...)
}

test_that("McNemar notices follow the discordant pairs without the retired cut-off", {
    notes <- function(v, ...) jbs_txt(paste(as.character(bar_mcn(v, ...)$notices$content), collapse = " "))
    n8 <- notes(c(40, 5, 3, 40))                                         # b = 5, c = 3
    expect_match(n8, "Only 8 discordant pairs (5 vs 3) for 'a', so McNemar's test has little power", fixed = TRUE)
    for (r in bar_mcn_retired) expect_false(grepl(r, n8, fixed = TRUE), info = r)
    n3 <- notes(c(20, 3, 0, 20))
    expect_match(n3, "Only 3 discordant pairs (3 vs 0) for 'a': McNemar's test cannot reach p < 0.05", fixed = TRUE)
    expect_false(grepl("Zero-Count Cells", n3, fixed = TRUE))            # a paired 2x2 cannot be collapsed
    expect_match(notes(c(20, 1, 0, 20)), "Only 1 discordant pair (1 vs 0)", fixed = TRUE)
    expect_false(grepl("discordant pair", notes(c(20, 15, 15, 20)), fixed = TRUE))   # m = 30
    # statsExpressions runs mcnemar.test(correct = FALSE); the narrative must say which
    expect_match(as.character(bar_mcn(c(40, 5, 3, 40))$summary$content),
                 "without continuity correction", fixed = TRUE)
})

test_that("a sparse paired table gets no expected-count rule and no Fisher advice", {
    # (b, c) = (6, 1), n = 40, one expected count 0.9: Fisher's exact test (an
    # independent-samples test) gives p = 0.030, the McNemar test shown 0.059.
    a <- jbs_assump(bar_mcn(c(30, 6, 1, 3), showAssumptions = TRUE))
    expect_match(a, "Paired observations (matched subjects)", fixed = TRUE)
    for (p in c("Fisher's exact test on this table", "quote that value", "Pearson",
                "Assumption Violated", "Expected cell counts"))
        expect_false(grepl(p, a, fixed = TRUE), info = p)
})

test_that("the paired chart shows McNemar's asymptotic statistic", {
    # (b, c) = (7, 17): chi2 = 100 / 24 = 4.17, p = 0.0412; the exact binomial
    # (0.0639) is not what is shown
    ref <- stats::mcnemar.test(matrix(c(20, 17, 7, 20), 2), correct = FALSE)
    expect_identical(sprintf("%.2f", unname(ref$statistic)), "4.17")
    t <- jbs_plot_txt(bar_mcn(c(20, 7, 17, 20), resultssubtitle = TRUE))
    expect_match(t, "McNemar", fixed = TRUE)
    expect_match(t, "4.17", fixed = TRUE)
})

test_that("fewer than 10 pairs are refused without exact-test advice", {
    err <- tryCatch(bar_mcn(c(2, 3, 1, 2)), error = function(e) conditionMessage(e))
    expect_type(err, "character")
    expect_match(err, "McNemar's test is not run for fewer than 10 pairs", fixed = TRUE)
    for (r in bar_mcn_retired) expect_false(grepl(r, err, fixed = TRUE), info = r)
})

test_that("a Bayesian paired run is not labelled McNemar", {
    r <- bar_mcn(c(20, 2, 8, 20), typestatistics = "bayes", showSummary = TRUE, showAssumptions = TRUE)
    s <- jbs_txt(r$summary$content)
    expect_match(s, "Bayesian contingency table analysis", fixed = TRUE)
    expect_false(grepl("McNemar", s, fixed = TRUE))
    expect_false(grepl("McNemar", jbs_assump(r), fixed = TRUE))
    expect_false(grepl("McNemar", jbs_txt(r$todo$content), fixed = TRUE))
    n <- jbs_txt(paste(as.character(r$notices$content), collapse = " "))
    expect_match(n, "Pairing not used by the Bayesian analysis", fixed = TRUE)
    expect_false(grepl("compatible with McNemar", n, fixed = TRUE))
})

test_that("the palette option reaches ggbarstats in package::palette form", {
    data(jjbarstats_test)
    d <- as.data.frame(jjbarstats_test)
    fills <- function(pal) {
        p <- jjbarstats(data = d, dep = "response", group = "treatment", palette = pal)$plot
        f <- tempfile(fileext = ".svg"); svglite::svglite(f, 8, 6)
        w <- character()
        withCallingHandlers(print(p), warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
        grDevices::dev.off()
        x <- paste(readLines(f, warn = FALSE), collapse = ""); unlink(f)
        list(svg = x, warnings = w)
    }
    a <- fills("Dark2"); b <- fills("gdoc")
    expect_false(identical(a$svg, b$svg))
    expect_false(any(grepl("package::palette", c(a$warnings, b$warnings))))
})

test_that("removed options are rejected rather than silently ignored", {
    data(jjbarstats_test)
    for (arg in list(list(pairwisecomparisons = TRUE), list(padjustmethod = "holm"), list(excl = TRUE)))
        expect_error(do.call(jjbarstats, c(list(data = jjbarstats_test, dep = "response", group = "treatment"), arg)),
                     "unused argument")
})
