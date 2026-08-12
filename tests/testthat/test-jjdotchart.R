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
    expect_match(n, "2 degrees of freedom")
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
                 "very little power")

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
