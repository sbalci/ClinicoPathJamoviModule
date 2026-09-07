# Dot Chart (Summary vs Reference Value) - jjdotchart
#
# The analysis collapses each group to ONE point and runs a one-sample test of
# those k points against a reference value. Almost every risk here is a
# misreading risk, so the tests assert what the user is shown, not internals.
#
# NOTE: these require R/jjdotchart.h.R, which is generated. Run
#   Rscript -e 'Sys.unsetenv("ELECTRON_RUN_AS_NODE"); jmvtools::prepare()'
# before running this file for the first time.

dc_skew <- function() {
    set.seed(3)
    data.frame(
        v = c(c(rnorm(27, 10, 1), 200, 250, 300), rnorm(30, 12, 1), rnorm(30, 14, 1)),
        g = factor(rep(c("A", "B", "C"), each = 30)),
        s = factor(rep(c("M", "F"), 45))
    )
}
dc_flat <- function() {
    set.seed(9)
    data.frame(v = rnorm(90, 10, 1), g = factor(rep(c("A", "B", "C"), each = 30)))
}
dc_many <- function() {
    set.seed(11)
    data.frame(v = rnorm(200, 10, 2), g = factor(rep(paste0("L", 1:10), each = 20)))
}

dc_notices <- function(res) gsub("<[^>]*>", "", res$notices$content %||% "")
dc_render <- function(res, item = "plot") {
    f <- tempfile(fileext = ".svg")
    svglite::svglite(f, width = 9, height = 6)
    ok <- tryCatch({ print(res[[item]]); TRUE }, error = function(e) conditionMessage(e))
    grDevices::dev.off()
    x <- readLines(f, warn = FALSE); unlink(f)
    list(ok = ok, svg = paste(x, collapse = ""),
         txt = paste(gsub("^>|</text>$", "",
                          regmatches(x, regexpr(">[^<]*</text>", x))), collapse = ""))
}
`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- what is actually tested -----------------------------------------------

test_that("the panel states that n is the number of groups, not observations", {
    res <- jjdotchart(data = dc_skew(), dep = "v", group = "g", testvalue = 12)
    n <- dc_notices(res)
    expect_match(n, "3 groups is reduced to one point")
    expect_match(n, "3 group summaries")
    expect_match(n, "90 observations")
    expect_match(n, "does not compare the groups with each other")
})

test_that("the plotted summary follows the selected test, and the table agrees", {
    d <- dc_skew()
    # Group A is heavily skewed (three injected outliers), so mean and median
    # differ sharply - if the table were hard-coded to the mean it would
    # contradict the figure on three of the four settings.
    for (ty in c("parametric", "nonparametric", "robust")) {
        res <- jjdotchart(data = d, dep = "v", group = "g",
                          testvalue = 12, typestatistics = ty)
        tab <- res$summary$asDF
        expected <- switch(ty,
            parametric    = mean(d$v[d$g == "A"]),
            nonparametric = stats::median(d$v[d$g == "A"]),
            robust        = mean(d$v[d$g == "A"], trim = 0.2))
        expect_equal(tab$value[tab$grp == "A"], expected, tolerance = 1e-9, info = ty)
    }
})

test_that("every table row matches the point and error bar actually drawn", {
    # The block above pins the ARITHMETIC but compares the table only against a
    # formula written here, so it cannot see the table drifting away from the
    # figure - and it skips bayes entirely. It did drift: the table reported the
    # mean (33.77) under a column headed "MAP estimate" while the chart plotted
    # the MAP (9.19), and the interval was a t interval on the mean rather than
    # the type-specific bootstrap the error bars use.
    #
    # Oracle is the upstream figure itself. Its interval is bootstrapped, so it
    # has to be built under the same seed the analysis pins internally.
    d <- dc_skew()
    for (ty in c("parametric", "nonparametric", "robust", "bayes")) {
        tab <- jjdotchart(data = d, dep = "v", group = "g",
                          testvalue = 12, typestatistics = ty)$summary$asDF
        fig <- as.data.frame(withr::with_seed(20250101L,
            ggstatsplot::ggdotplotstats(data = d, x = v, y = g, type = ty,
                                        test.value = 12, results.subtitle = FALSE,
                                        conf.level = 0.95, digits = 2))$data)
        m <- merge(tab, fig, by.x = "grp", by.y = "g")
        expect_equal(nrow(m), 3L, info = ty)
        expect_equal(m$value,   m$v,         tolerance = 1e-9, info = ty)
        expect_equal(m$ci_low,  m$conf.low,  tolerance = 1e-9, info = ty)
        expect_equal(m$ci_high, m$conf.high, tolerance = 1e-9, info = ty)
    }
})

test_that("the summary column is labelled with the statistic actually used", {
    d <- dc_skew()
    note <- function(ty) {
        res <- jjdotchart(data = d, dep = "v", group = "g", typestatistics = ty, testvalue = 12)
        nt <- res$summary$notes
        paste(vapply(nt, function(x) if (is.character(x)) x else x$note %||% "", character(1)),
              collapse = " ")
    }
    expect_match(note("parametric"), "mean", ignore.case = TRUE)
    expect_match(note("nonparametric"), "median", ignore.case = TRUE)
})

test_that("the table reports n per group, making the aggregation auditable", {
    res <- jjdotchart(data = dc_skew(), dep = "v", group = "g", testvalue = 12)
    tab <- res$summary$asDF
    expect_equal(nrow(tab), 3L)
    expect_equal(sum(tab$n), 90L)
    expect_true(all(tab$n == 30L))
})


# ---- the silent-failure trap ------------------------------------------------

test_that("a reference value that kills the statistics is reported, not hidden", {
    # ggdotplotstats swallows the engine failure and returns a plot with no
    # subtitle at all. Measured: group means 9.89-10.06 with the package default
    # reference of 0 produce "function cannot be evaluated at initial parameters".
    res <- jjdotchart(data = dc_flat(), dep = "v", group = "g", testvalue = 0)
    n <- dc_notices(res)
    expect_match(n, "could not be computed")
    expect_match(n, "Reference Value is far from")
})

test_that("a sensible reference value computes and raises no failure notice", {
    res <- jjdotchart(data = dc_flat(), dep = "v", group = "g", testvalue = 10)
    expect_false(grepl("could not be computed", dc_notices(res), fixed = TRUE))
    r <- dc_render(res)
    expect_true(isTRUE(r$ok))
    expect_match(r$txt, "t", fixed = TRUE)
})

test_that("a reference value outside the range of the summaries is flagged", {
    res <- jjdotchart(data = dc_many(), dep = "v", group = "g", testvalue = 1000)
    expect_match(dc_notices(res), "outside the range of the group")
})


# ---- the reference line -----------------------------------------------------

test_that("the reference line is drawn at the reference value", {
    d <- dc_many()
    r12 <- dc_render(jjdotchart(data = d, dep = "v", group = "g", testvalue = 9))
    r20 <- dc_render(jjdotchart(data = d, dep = "v", group = "g", testvalue = 11))
    expect_false(identical(r12$svg, r20$svg))
    expect_match(r12$txt, "Reference Value = 9", fixed = TRUE)
})

test_that("the optional centrality line is named so it cannot be misread", {
    d <- dc_many()
    off <- dc_render(jjdotchart(data = d, dep = "v", group = "g",
                                testvalue = 10, centralityplotting = FALSE))
    on  <- dc_render(jjdotchart(data = d, dep = "v", group = "g",
                                testvalue = 10, centralityplotting = TRUE))
    expect_false(grepl("not the reference", off$txt, fixed = TRUE))
    expect_match(on$txt, "not the reference", fixed = TRUE)
})


# ---- data handling ----------------------------------------------------------

test_that("infinite values are excluded and disclosed", {
    d <- dc_many(); d$v[1] <- Inf
    res <- jjdotchart(data = d, dep = "v", group = "g", testvalue = 10)
    expect_match(dc_notices(res), "infinite value")
    tab <- res$summary$asDF
    expect_true(all(is.finite(tab$value)))
    expect_equal(sum(tab$n), 199L)
})

test_that("fewer than three groups is warned about, and one group is rejected", {
    d2 <- droplevels(subset(dc_skew(), g %in% c("A", "B")))
    expect_match(dc_notices(jjdotchart(data = d2, dep = "v", group = "g", testvalue = 12)),
                 "inference from their summaries is imprecise")

    d1 <- droplevels(subset(dc_skew(), g == "A"))
    n1 <- dc_notices(jjdotchart(data = d1, dep = "v", group = "g", testvalue = 12))
    expect_match(n1, "at least two groups")
})

test_that("empty factor levels do not become phantom points", {
    d <- dc_many()
    levels(d$g) <- c(levels(d$g), "NeverUsed")
    tab <- jjdotchart(data = d, dep = "v", group = "g", testvalue = 10)$summary$asDF
    expect_equal(nrow(tab), 10L)
    expect_false("NeverUsed" %in% tab$grp)
})

test_that("results are reproducible across identical runs", {
    # The error bars come from an unseeded datawizard bootstrap upstream.
    d <- dc_many()
    a <- dc_render(jjdotchart(data = d, dep = "v", group = "g", testvalue = 10))
    b <- dc_render(jjdotchart(data = d, dep = "v", group = "g", testvalue = 10))
    expect_identical(a$svg, b$svg)
})


test_that("variable names with spaces and punctuation survive the whole pipeline", {
    # Names reach rlang::sym(), statsExpressions' returned tibble columns and
    # the axis labels. as.data.frame() on that tibble must NOT make.names() the
    # group/measurement columns, or cd[[grp]] would look up NULL.
    d <- dc_many()
    names(d) <- c("Ki-67 %", "Tumor Grade")
    res <- jjdotchart(data = d, dep = "Ki-67 %", group = "Tumor Grade", testvalue = 10)

    tab <- res$summary$asDF
    expect_equal(nrow(tab), 10L)
    expect_false(any(is.na(tab$value)))
    expect_true(all(tab$n > 0))

    r <- dc_render(res)
    expect_true(isTRUE(r$ok))
    expect_false(grepl("could not be drawn", r$txt, fixed = TRUE))
    # The measurement name is the default x-axis label.
    expect_match(r$txt, "Ki-67", fixed = TRUE)
})

test_that("infinite rows are not also counted as missing", {
    # n_dropped is computed after BOTH filters, so it used to include the Inf
    # rows: 3 Inf values and zero NAs reported "3 row(s) with missing values
    # were excluded" alongside "3 row(s) had an infinite value".
    d <- dc_many()
    d$v[1:3] <- Inf
    n <- dc_notices(jjdotchart(data = d, dep = "v", group = "g", testvalue = 10))
    expect_match(n, "3 row\\(s\\) had an infinite value")
    expect_false(grepl("row(s) with missing values", n, fixed = TRUE))

    # 4 genuinely missing + 3 infinite must be reported as 4 and 3, not 7 and 3.
    d2 <- dc_many()
    d2$v[1:3] <- Inf
    d2$v[10:13] <- NA
    n2 <- dc_notices(jjdotchart(data = d2, dep = "v", group = "g", testvalue = 10))
    expect_match(n2, "4 row\\(s\\) with missing values")
    expect_match(n2, "3 row\\(s\\) had an infinite value")
})

test_that("notices are ordered ERROR first, whatever order they were raised in", {
    # The engine-failure ERROR is raised last in .run(), after the INFO and
    # WARNING from .validate(); it must still render at the top of the panel.
    d <- dc_flat()
    n <- dc_notices(jjdotchart(data = d, dep = "v", group = "g", testvalue = 0))
    pos <- function(tag) regexpr(tag, n, fixed = TRUE)
    if (pos("ERROR:") > 0) {
        expect_true(pos("ERROR:") < pos("INFO:"))
        if (pos("WARNING:") > 0) expect_true(pos("ERROR:") < pos("WARNING:"))
    }
    # WARNING always precedes INFO even though the INFO is raised first.
    d2 <- dc_many()
    n2 <- dc_notices(jjdotchart(data = d2, dep = "v", group = "g", testvalue = 0))
    expect_true(regexpr("WARNING:", n2, fixed = TRUE) <
                regexpr("INFO:", n2, fixed = TRUE))
})

test_that("the reference-line caption is never lost to a Bayes factor message", {
    # bfmessage was removed: ggstatsplot honours bf.message only for
    # type="parametric" and the Bayes factor REPLACES the caption, deleting the
    # only text naming the reference line. Every test type must keep it.
    d <- dc_many()
    for (ty in c("parametric", "nonparametric", "robust", "bayes")) {
        r <- dc_render(jjdotchart(data = d, dep = "v", group = "g",
                                  testvalue = 10, typestatistics = ty))
        expect_true(isTRUE(r$ok))
        expect_match(r$txt, "Dashed red line", fixed = TRUE,
                     info = paste("test type:", ty))
    }
})

test_that("tiny and highly unequal groups are flagged", {
    # Every group contributes ONE equally-weighted point whatever its n, and a
    # one-observation group is drawn with a zero-width interval - the least
    # certain estimate rendered as the most precise point on the chart.
    d <- data.frame(
        v = c(rnorm(40, 10, 1), rnorm(40, 12, 1), 99),
        g = factor(rep(c("A", "B", "Rare"), c(40, 40, 1)))
    )
    n <- dc_notices(jjdotchart(data = d, dep = "v", group = "g", testvalue = 10))
    expect_match(n, "fewer than 3 observations")
    expect_match(n, "zero-width interval")
    expect_match(n, "highly unequal")
    expect_match(n, "40 observations")

    # Balanced groups of adequate size must raise neither notice.
    n2 <- dc_notices(jjdotchart(data = dc_many(), dep = "v", group = "g", testvalue = 10))
    expect_false(grepl("fewer than 3 observations", n2, fixed = TRUE))
    expect_false(grepl("highly unequal", n2, fixed = TRUE))
})

test_that("the robust summary in the table equals the plotted point", {
    # .plotArgs pins tr = 0.2 rather than trusting ggdotplotstats' default; if
    # upstream ever changes it the table and the figure would disagree.
    d <- dc_skew()
    tab <- jjdotchart(data = d, dep = "v", group = "g", testvalue = 12,
                      typestatistics = "robust")$summary$asDF
    p <- withr::with_seed(20250101, ggstatsplot::ggdotplotstats(
        data = d, x = v, y = g, type = "robust", tr = 0.2,
        test.value = 12, results.subtitle = FALSE))
    plotted <- p$data$v[match(tab$grp, as.character(p$data$g))]
    expect_equal(tab$value, plotted, tolerance = 1e-8)
})

test_that("the nonparametric p-value approximation is disclosed", {
    # statsExpressions uses wilcox.test(exact = FALSE, correct = TRUE); base R
    # defaults to the exact distribution below n = 50. At k = 10 that is
    # p = 0.05279 vs 0.04883 - opposite sides of 0.05.
    d <- dc_many()
    n <- dc_notices(jjdotchart(data = d, dep = "v", group = "g",
                               testvalue = 9.5, typestatistics = "nonparametric"))
    expect_match(n, "normal approximation with a continuity correction")

    # Not claimed for the other test types.
    for (ty in c("parametric", "robust", "bayes")) {
        n2 <- dc_notices(jjdotchart(data = d, dep = "v", group = "g",
                                    testvalue = 9.5, typestatistics = ty))
        expect_false(grepl("continuity correction", n2, fixed = TRUE),
                     info = paste("test type:", ty))
    }
})

test_that("the parametric test equals a hand stats::t.test on the k summaries", {
    # The headline claim of this analysis: a one-sample test of the k group
    # summaries. Verify the reported statistic IS that test, with df = k - 1.
    d <- dc_many()
    tab <- jjdotchart(data = d, dep = "v", group = "g", testvalue = 9.5)$summary$asDF
    ht <- stats::t.test(tab$value, mu = 9.5)
    expect_equal(unname(ht$parameter), nrow(tab) - 1)
    ost <- withr::with_seed(20250101, statsExpressions::one_sample_test(
        data = data.frame(.v = tab$value), x = .v, type = "parametric",
        test.value = 9.5, conf.level = 0.95, digits = 5))
    expect_equal(unname(ht$statistic), as.data.frame(ost)$statistic, tolerance = 1e-9)
    expect_equal(ht$p.value, as.data.frame(ost)$p.value, tolerance = 1e-9)
})

# ---- split by ---------------------------------------------------------------

test_that("the Split By chart renders, with and without a title", {
    d <- dc_skew()
    r <- dc_render(jjdotchart(data = d, dep = "v", group = "g", grvar = "s",
                              testvalue = 12), item = "plot2")
    expect_true(isTRUE(r$ok))
    expect_false(grepl("could not be drawn", r$txt, fixed = TRUE))

    # grouped_ggdotplotstats titles each panel itself, so a `title` passed
    # through ... collides; the overall title must go via annotation.args.
    rt <- dc_render(jjdotchart(data = d, dep = "v", group = "g", grvar = "s",
                               testvalue = 12, mytitle = "By sex"), item = "plot2")
    expect_true(isTRUE(rt$ok))
    expect_match(rt$txt, "By sex", fixed = TRUE)
})


# ---- entry conditions -------------------------------------------------------

test_that("no variables selected shows the welcome text and warns about the design", {
    # No `data =` on purpose. jmvcore::select(data, character(0)) builds a
    # zero-column frame and dies in `row.names<-` with "invalid 'row.names'
    # length" BEFORE any module code runs, so `jjdotchart(data = d, dep = NULL)`
    # cannot be called from R at all. Verified identical for jjdotplotstats and
    # jjbetweenstats, i.e. it is jmvcore-wide and not a defect here; the jamovi
    # GUI reaches .run() by another route and shows this welcome panel.
    res <- jjdotchart(dep = NULL, group = NULL)
    expect_match(res$todo$content, "Cleveland dot chart")
    expect_match(res$todo$content, "not the number of patients")
    expect_equal(dc_notices(res), "")
})

test_that("all four statistical types render", {
    d <- dc_many()
    for (ty in c("parametric", "nonparametric", "robust", "bayes")) {
        r <- dc_render(jjdotchart(data = d, dep = "v", group = "g",
                                  testvalue = 10, typestatistics = ty))
        expect_true(isTRUE(r$ok), info = ty)
        expect_false(grepl("could not be drawn", r$txt, fixed = TRUE), info = ty)
    }
})
