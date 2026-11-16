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
    # Inspect the actual ggplot object built by jjdotchartClass across all four
    # statistical modes so any drift between the Group Summaries table and the
    # rendered points / horizontal error bars is caught immediately.
    opts_cls <- if (exists("jjdotchartOptions", inherits = TRUE)) get("jjdotchartOptions", inherits = TRUE) else ClinicoPath:::jjdotchartOptions
    ana_cls  <- if (exists("jjdotchartClass", inherits = TRUE)) get("jjdotchartClass", inherits = TRUE) else ClinicoPath:::jjdotchartClass
    d <- dc_skew()
    for (ty in c("parametric", "nonparametric", "robust", "bayes")) {
        opts <- opts_cls$new(
            dep = "v", group = "g", testvalue = 12,
            typestatistics = ty, conflevel = 0.95, k = 2
        )
        ana <- ana_cls$new(options = opts, data = d)
        suppressWarnings(ana$run())
        tab <- ana$results$summary$asDF

        prep <- ana$.__enclos_env__$private$.prepareData()
        p <- ana$.__enclos_env__$private$.buildMainPlot(prep, ggplot2::theme_bw())
        gb <- ggplot2::ggplot_build(p)
        err_layer <- gb$data[[1]]
        pt_layer  <- gb$data[[2]]
        y_labels  <- gb$layout$panel_params[[1]]$y$get_labels()

        drawn <- data.frame(
            grp      = as.character(y_labels[pt_layer$y]),
            x_drawn  = as.numeric(pt_layer$x),
            lo_drawn = as.numeric(err_layer$xmin),
            hi_drawn = as.numeric(err_layer$xmax),
            stringsAsFactors = FALSE
        )
        m <- merge(tab, drawn, by = "grp")
        expect_equal(nrow(m), 3L, info = ty)
        expect_equal(m$value,   m$x_drawn,  tolerance = 1e-12, info = ty)
        expect_equal(m$ci_low,  m$lo_drawn, tolerance = 1e-12, info = ty)
        expect_equal(m$ci_high, m$hi_drawn, tolerance = 1e-12, info = ty)
    }
})

test_that("the summary column is labelled with the statistic actually used", {
    d <- dc_skew()
    expected_titles <- c(
        parametric    = "Mean",
        nonparametric = "Median",
        robust        = "Trimmed mean (20%)",
        bayes         = "MAP estimate"
    )
    for (ty in names(expected_titles)) {
        res <- jjdotchart(data = d, dep = "v", group = "g", typestatistics = ty, testvalue = 12)
        expect_identical(res$summary$columns$value$title, expected_titles[[ty]], info = ty)
        nt <- res$summary$notes
        note_txt <- paste(
            vapply(nt, function(x) if (is.character(x)) x else x$note %||% "", character(1)),
            collapse = " "
        )
        expect_match(note_txt, expected_titles[[ty]], fixed = TRUE, info = ty)
    }
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

test_that("haven_labelled value labels and SPSS na_values/na_range are honored", {
    d <- data.frame(
        y = structure(
            c(10, 12, 14, 999, 20, 22, 24, 30, 32, 34, 15, 16),
            na_values = 999,
            class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double")
        ),
        g = structure(
            c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 99, 99),
            labels = c("Site Alpha" = 1, "Site Beta" = 2, "Site Gamma" = 3, "Missing Site" = 99),
            na_values = 99,
            class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double")
        )
    )
    opts_cls <- if (exists("jjdotchartOptions", inherits = TRUE)) get("jjdotchartOptions", inherits = TRUE) else ClinicoPath:::jjdotchartOptions
    ana_cls  <- if (exists("jjdotchartClass", inherits = TRUE)) get("jjdotchartClass", inherits = TRUE) else ClinicoPath:::jjdotchartClass
    opts <- opts_cls$new(dep = "y", group = "g", testvalue = 20)
    ana <- ana_cls$new(options = opts, data = d)
    suppressWarnings(ana$run())
    tab <- ana$results$summary$asDF
    expect_identical(sort(tab$grp), c("Site Alpha", "Site Beta", "Site Gamma"))
    expect_equal(tab$n[tab$grp == "Site Alpha"], 3L)
    expect_equal(tab$value[tab$grp == "Site Alpha"], 12, tolerance = 1e-12)
})

test_that("variables and group levels with spaces, regex metacharacters, HTML, and Unicode are safe", {
    d <- data.frame(
        `ALT (U/L) [A+B] % #` = c(10, 12, 14, Inf, 20, 22, 24, 30, 32, 34),
        `Hospital & Clinic <Unit>` = factor(c(
            rep("Lab <A+B>", 4),
            rep("Şişli — Merkez", 3),
            rep("Site (Gamma) [3]", 3)
        )),
        `Region + Cohort` = factor(rep(c("North <1>", "South & West"), length.out = 10)),
        check.names = FALSE
    )
    res <- jjdotchart(
        data = d,
        dep = "ALT (U/L) [A+B] % #",
        group = "Hospital & Clinic <Unit>",
        grvar = "Region + Cohort",
        testvalue = 20
    )
    expect_equal(nrow(res$summary$asDF), 3L)
    expect_match(res$notices$content, "ALT \\(U/L\\) \\[A\\+B\\] % #")
    expect_false(grepl("<Unit>", res$notices$content, fixed = TRUE))
    r1 <- dc_render(res, "plot")
    expect_true(isTRUE(r1$ok))
})

# ---- release-review lifecycle, serialization, and scale regressions ---------

test_that("stateful A -> B recalculation on one R6 instance matches fresh B", {
    opts_cls <- if (exists("jjdotchartOptions", inherits = TRUE)) get("jjdotchartOptions", inherits = TRUE) else ClinicoPath:::jjdotchartOptions
    ana_cls  <- if (exists("jjdotchartClass", inherits = TRUE)) get("jjdotchartClass", inherits = TRUE) else ClinicoPath:::jjdotchartClass

    d <- dc_skew()
    opts_a <- opts_cls$new(
        dep = "v", group = "g", grvar = "s",
        typestatistics = "robust", testvalue = 999,
        conflevel = 0.90, centralityplotting = TRUE,
        centralitytype = "nonparametric", seed = 7L
    )
    inst <- ana_cls$new(options = opts_a, data = d)
    inst$run()

    opts_a$.__enclos_env__$private$..grvar$value <- NULL
    opts_a$.__enclos_env__$private$..typestatistics$value <- "nonparametric"
    opts_a$.__enclos_env__$private$..testvalue$value <- 12
    opts_a$.__enclos_env__$private$..conflevel$value <- 0.95
    opts_a$.__enclos_env__$private$..centralityplotting$value <- FALSE
    opts_a$.__enclos_env__$private$..seed$value <- 42L
    inst$run()

    fresh_b <- jjdotchart(
        data = d, dep = "v", group = "g", grvar = NULL,
        typestatistics = "nonparametric", testvalue = 12,
        conflevel = 0.95, centralityplotting = FALSE, seed = 42L
    )

    expect_equal(inst$results$summary$asDF, fresh_b$summary$asDF, tolerance = 1e-12)
    expect_identical(inst$results$notices$content, fresh_b$notices$content)
    expect_identical(inst$results$plot$state, fresh_b$plot$state)
    expect_identical(inst$results$plot2$state, fresh_b$plot2$state)
})

test_that(".omv reopen renders main and split plots from restored image$state without .run()", {
    opts_cls <- if (exists("jjdotchartOptions", inherits = TRUE)) get("jjdotchartOptions", inherits = TRUE) else ClinicoPath:::jjdotchartOptions
    ana_cls  <- if (exists("jjdotchartClass", inherits = TRUE)) get("jjdotchartClass", inherits = TRUE) else ClinicoPath:::jjdotchartClass

    d <- dc_skew()
    saved <- jjdotchart(
        data = d, dep = "v", group = "g", grvar = "s",
        typestatistics = "parametric", testvalue = 12,
        centralityplotting = TRUE
    )
    expect_true(is.logical(saved$plot$state) && length(saved$plot$state) == 1L)
    expect_true(is.logical(saved$plot2$state) && length(saved$plot2$state) == 1L)

    reopen_opts <- opts_cls$new(
        dep = "v", group = "g", grvar = "s",
        typestatistics = "parametric", testvalue = 12,
        centralityplotting = TRUE
    )
    reopened <- ana_cls$new(options = reopen_opts, data = d)
    reopened$results$plot$setState(saved$plot$state)
    reopened$results$plot2$setState(saved$plot2$state)

    tf <- tempfile(fileext = ".pdf")
    on.exit(unlink(tf), add = TRUE)
    grDevices::pdf(tf)
    ok1 <- reopened$.__enclos_env__$private$.plot(reopened$results$plot, ggplot2::theme_bw(), list())
    ok2 <- reopened$.__enclos_env__$private$.plot2(reopened$results$plot2, ggplot2::theme_bw(), list())
    grDevices::dev.off()

    expect_true(isTRUE(ok1))
    expect_true(isTRUE(ok2))
    expect_gt(file.info(tf)$size, 1000)
})

test_that("streaming bootstrap (n_g > 5000) and large numeric seeds avoid integer overflow", {
    opts_cls <- if (exists("jjdotchartOptions", inherits = TRUE)) get("jjdotchartOptions", inherits = TRUE) else ClinicoPath:::jjdotchartOptions
    ana_cls  <- if (exists("jjdotchartClass", inherits = TRUE)) get("jjdotchartClass", inherits = TRUE) else ClinicoPath:::jjdotchartClass

    set.seed(101)
    big_d <- data.frame(
        y = c(rnorm(5100, mean = 10, sd = 2), rnorm(40, mean = 13, sd = 2), rnorm(40, mean = 16, sd = 2)),
        g = factor(c(rep("BigRegistry", 5100), rep("ClinicB", 40), rep("ClinicC", 40)))
    )
    opts <- opts_cls$new(dep = "y", group = "g", typestatistics = "nonparametric", testvalue = 12)
    opts$.__enclos_env__$private$..seed$value <- 3e9
    ana <- ana_cls$new(options = opts, data = big_d)
    expect_no_warning(ana$run())
    tab <- ana$results$summary$asDF
    big_row <- tab[tab$grp == "BigRegistry", ]
    expect_equal(big_row$n, 5100L)
    expect_true(is.finite(big_row$ci_low) && is.finite(big_row$ci_high))
    expect_lte(big_row$ci_low, big_row$value)
    expect_gte(big_row$ci_high, big_row$value)
})

test_that("weightsensitivity table matches balanced, CR1 caseload-weighted, and DerSimonian-Laird oracles", {
    # Hidden by default
    d_bal <- dc_many()
    res_off <- jjdotchart(data = d_bal, dep = "v", group = "g", showWeightedTable = FALSE)
    expect_false(isTRUE(res_off$weightsensitivity$visible))

    # 1. Balanced caseloads: caseload-weighted equals unweighted to machine precision
    res_bal <- jjdotchart(
        data = d_bal, dep = "v", group = "g",
        typestatistics = "parametric", testvalue = 12, conflevel = 0.95,
        showWeightedTable = TRUE
    )
    expect_true(isTRUE(res_bal$weightsensitivity$visible))
    wt_bal <- res_bal$weightsensitivity$asDF
    expect_equal(nrow(wt_bal), 3L)
    expect_equal(wt_bal$estimate[2], wt_bal$estimate[1], tolerance = 1e-12)
    expect_equal(wt_bal$se[2],       wt_bal$se[1],       tolerance = 1e-12)
    expect_equal(wt_bal$ci_low[2],   wt_bal$ci_low[1],   tolerance = 1e-12)
    expect_equal(wt_bal$ci_high[2],  wt_bal$ci_high[1],  tolerance = 1e-12)
    expect_equal(wt_bal$pvalue[2],   wt_bal$pvalue[1],   tolerance = 1e-12)

    # 2. Skewed caseloads: exact CR1 sandwich + DerSimonian-Laird random-effects synthesis
    set.seed(2026)
    d_unbal <- data.frame(
        y = c(
            rnorm(120, mean = 18.0, sd = 2.0),
            rnorm(15,  mean = 9.0,  sd = 3.5),
            rnorm(12,  mean = 10.5, sd = 4.0),
            rnorm(10,  mean = 8.5,  sd = 3.0),
            rnorm(8,   mean = 11.0, sd = 4.5)
        ),
        grp = factor(c(
            rep("Hub", 120), rep("SiteB", 15), rep("SiteC", 12),
            rep("SiteD", 10), rep("SiteE", 8)
        ), levels = c("Hub", "SiteB", "SiteC", "SiteD", "SiteE"))
    )
    tv <- 13.0
    res_unbal <- jjdotchart(
        data = d_unbal, dep = "y", group = "grp",
        typestatistics = "parametric", testvalue = tv, conflevel = 0.90,
        showWeightedTable = TRUE
    )
    tab <- res_unbal$summary$asDF
    wt  <- res_unbal$weightsensitivity$asDF
    k   <- nrow(tab)
    tcrit <- qt(0.95, df = k - 1L)
    zcrit <- qnorm(0.95)

    # Parametric caseload-weighted mean equals grand observation mean
    expect_equal(wt$estimate[2], mean(d_unbal$y), tolerance = 1e-12)
    w_obs <- tab$n / sum(tab$n)
    cr1_se <- sqrt((k / (k - 1L)) * sum((w_obs^2) * ((tab$value - wt$estimate[2])^2)))
    expect_equal(wt$se[2],      cr1_se, tolerance = 1e-12)
    expect_equal(wt$ci_low[2],  wt$estimate[2] - tcrit * cr1_se, tolerance = 1e-12)
    expect_equal(wt$ci_high[2], wt$estimate[2] + tcrit * cr1_se, tolerance = 1e-12)
    expect_equal(wt$pvalue[2],  2 * pt(-abs((wt$estimate[2] - tv) / cr1_se), df = k - 1L), tolerance = 1e-12)

    # Independent DerSimonian-Laird (1986) reference calculation
    yi <- vapply(levels(d_unbal$grp), function(g) mean(d_unbal$y[d_unbal$grp == g]), numeric(1))
    vi <- vapply(levels(d_unbal$grp), function(g) {
        xg <- d_unbal$y[d_unbal$grp == g]
        var(xg) / length(xg)
    }, numeric(1))
    # Sort in table order
    ord_idx <- match(tab$grp, names(yi))
    yi <- unname(yi[ord_idx])
    vi <- unname(vi[ord_idx])
    wi <- 1 / vi
    mu_fe <- sum(wi * yi) / sum(wi)
    Q <- sum(wi * ((yi - mu_fe)^2))
    c_dl <- sum(wi) - sum(wi^2) / sum(wi)
    tau2 <- max(0, (Q - (k - 1L)) / c_dl)
    wi_re <- 1 / (vi + tau2)
    dl_est <- sum(wi_re * yi) / sum(wi_re)
    dl_se  <- sqrt(1 / sum(wi_re))

    expect_equal(wt$estimate[3], dl_est, tolerance = 1e-11)
    expect_equal(wt$se[3],       dl_se,  tolerance = 1e-11)
    expect_equal(wt$ci_low[3],   dl_est - zcrit * dl_se, tolerance = 1e-11)
    expect_equal(wt$ci_high[3],  dl_est + zcrit * dl_se, tolerance = 1e-11)
    expect_equal(wt$pvalue[3],   2 * pnorm(-abs((dl_est - tv) / dl_se)), tolerance = 1e-11)

    if (requireNamespace("metafor", quietly = TRUE)) {
        rma_fit <- metafor::rma.uni(yi = yi - tv, vi = vi, method = "DL", level = 90)
        expect_equal(wt$vsref[3],  as.numeric(rma_fit$b[1]), tolerance = 1e-10)
        expect_equal(wt$se[3],     as.numeric(rma_fit$se),   tolerance = 1e-10)
        expect_equal(wt$pvalue[3], as.numeric(rma_fit$pval), tolerance = 1e-10)
    }

    # 3. Single-observation groups gracefully excluded from inverse-variance row
    d_sing <- data.frame(
        y   = c(10, 12, 14, 20, 22),
        grp = factor(paste0("G", 1:5))
    )
    res_sing <- jjdotchart(
        data = d_sing, dep = "y", group = "grp",
        testvalue = 15, showWeightedTable = TRUE
    )
    wt_sing <- res_sing$weightsensitivity$asDF
    expect_true(is.finite(wt_sing$estimate[1]))
    expect_true(is.finite(wt_sing$estimate[2]))
    expect_true(is.na(wt_sing$estimate[3]))
    expect_match(wt_sing$units[3], "0 of 5")
})

test_that("configurable hyperparameters (trimprop, bfprior, nboot, ciresamples, mapresamples) match exact oracles", {
    d <- dc_skew()

    # 1. trimprop + nboot (robust) + robust centrality line
    r10 <- jjdotchart(
        data = d, dep = "v", group = "g",
        typestatistics = "robust", trimprop = 0.10, nboot = 300L,
        centralityplotting = TRUE, centralitytype = "robust",
        testvalue = 12, seed = 42L
    )
    r35 <- jjdotchart(
        data = d, dep = "v", group = "g",
        typestatistics = "robust", trimprop = 0.35, nboot = 300L,
        centralityplotting = TRUE, centralitytype = "robust",
        testvalue = 12, seed = 42L
    )
    t10 <- r10$summary$asDF
    t35 <- r35$summary$asDF
    for (i in seq_len(nrow(t10))) {
        xg <- d$v[d$g == t10$grp[i]]
        expect_equal(t10$value[i], mean(xg, trim = 0.10), tolerance = 1e-12)
    }
    for (i in seq_len(nrow(t35))) {
        xg <- d$v[d$g == t35$grp[i]]
        expect_equal(t35$value[i], mean(xg, trim = 0.35), tolerance = 1e-12)
    }
    expect_false(isTRUE(all.equal(t10$value, t35$value)))
    expect_match(r10$summary$columns$value$title, "10%")
    expect_match(r35$summary$columns$value$title, "35%")

    # 2. bfprior + mapresamples (Bayesian) against BayesFactor::ttestBF(rscale = ...)
    d_m <- dc_many()
    b_narrow <- jjdotchart(
        data = d_m, dep = "v", group = "g",
        typestatistics = "bayes", bfprior = 0.35, mapresamples = 200L,
        testvalue = 12, seed = 42L
    )
    b_wide <- jjdotchart(
        data = d_m, dep = "v", group = "g",
        typestatistics = "bayes", bfprior = 1.414, mapresamples = 200L,
        testvalue = 12, seed = 42L
    )
    modes_vec <- b_narrow$summary$asDF$value
    bf_n_ref <- -as.numeric(BayesFactor::ttestBF(x = modes_vec, mu = 12, rscale = 0.35)@bayesFactor$bf[1])
    bf_w_ref <- -as.numeric(BayesFactor::ttestBF(x = modes_vec, mu = 12, rscale = 1.414)@bayesFactor$bf[1])
    svg_n <- dc_render(b_narrow)
    svg_w <- dc_render(b_wide)
    expect_true(isTRUE(svg_n$ok) && isTRUE(svg_w$ok))
    expect_false(identical(svg_n$txt, svg_w$txt))
    expect_match(svg_n$txt, sprintf("%.2f", abs(bf_n_ref)), fixed = TRUE)
    expect_match(svg_w$txt, sprintf("%.2f", abs(bf_w_ref)), fixed = TRUE)
    expect_match(b_wide$notices$content, "B = 200", fixed = TRUE)
    expect_match(b_wide$notices$content, "r = 1.414", fixed = TRUE)

    # 3. ciresamples (nonparametric median bootstrap)
    np_small <- jjdotchart(
        data = d_m, dep = "v", group = "g",
        typestatistics = "nonparametric", ciresamples = 250L,
        testvalue = 12, seed = 42L
    )
    expect_match(np_small$summary$notes$agg$note, "B = 250", fixed = TRUE)
})

test_that("Turkish tr.po localization covers all jjdotchart strings with intact placeholders", {
    po_candidates <- c(
        "jamovi/i18n/tr.po",
        "../../jamovi/i18n/tr.po"
    )
    po_path <- po_candidates[file.exists(po_candidates)][1]
    skip_if(is.na(po_path), "tr.po catalog not found relative to test working directory")

    lines <- readLines(po_path, encoding = "UTF-8", warn = FALSE)
    blocks <- list()
    cur <- character(0)
    for (ln in lines) {
        if (nzchar(trimws(ln))) {
            cur <- c(cur, ln)
        } else if (length(cur) > 0L) {
            blocks[[length(blocks) + 1L]] <- cur
            cur <- character(0)
        }
    }
    if (length(cur) > 0L) blocks[[length(blocks) + 1L]] <- cur

    jj_blocks <- Filter(function(b) any(grepl("jjdotchart", b[grepl("^#:", b)], fixed = TRUE)), blocks)
    expect_gt(length(jj_blocks), 40L)

    unquo <- function(s) {
        s <- sub('^"[ ]*', "", s)
        sub('"$', "", s)
    }
    extract_field <- function(b, prefix) {
        idx <- which(startsWith(b, paste0(prefix, " ")))
        if (length(idx) == 0L) return("")
        parts <- sub(paste0("^", prefix, "\\s+"), "", b[idx[1]])
        k <- idx[1] + 1L
        while (k <= length(b) && startsWith(b[k], '"')) {
            parts <- c(parts, b[k])
            k <- k + 1L
        }
        paste(vapply(parts, unquo, character(1)), collapse = "")
    }
    ph_set <- function(s) {
        m <- gregexpr("\\{[A-Za-z0-9_]+\\}", s)
        vals <- regmatches(s, m)[[1]]
        sort(unique(vals))
    }

    for (b in jj_blocks) {
        expect_false(any(grepl("^#,.*fuzzy", b)), info = paste(b, collapse = "\n"))
        mid <- extract_field(b, "msgid")
        mstr <- extract_field(b, "msgstr")
        if (nzchar(mid)) {
            expect_true(nzchar(mstr), info = paste("Untranslated msgid:", mid))
            expect_identical(ph_set(mstr), ph_set(mid), info = paste("Placeholder mismatch for:", mid))
        }
    }
})

test_that("security hardening: syntax codegen, non-finite testvalue, self-referential column names, and clamped bounds", {
    d <- dc_many()
    pkg_ns <- environment(jjdotchart)

    # 1. Syntax pane codegen (.sourcifyOption) blocks quote/backtick/vector/omv injection
    bad_dep <- 'dep` + stop("CANARY") + "x'
    d_adv <- d
    names(d_adv)[names(d_adv) == "v"] <- bad_dep

    opts <- pkg_ns$jjdotchartOptions$new(
        dep = bad_dep,
        group = "g",
        mytitle = 'ok", testvalue = {stop("CANARY"); 5}, xtitle = "end',
        centralityplotting = TRUE
    )
    an <- pkg_ns$jjdotchartClass$new(options = opts, data = d_adv)

    # Simulate crafted .omv option payloads (multi-element vector, list, call, non-finite, undeclared)
    opts$.__enclos_env__$private$..xtitle$.__enclos_env__$private$.value <-
        c("ok", 'b", testvalue = {stop("CANARY"); 5}, ytitle = "c')
    opts$.__enclos_env__$private$..ytitle$.__enclos_env__$private$.value <-
        list(a = quote(stop("CANARY")))
    opts$.__enclos_env__$private$..typestatistics$.__enclos_env__$private$.value <-
        'parametric", testvalue = {stop("CANARY"); 5}, k = "2'
    opts$.__enclos_env__$private$..testvalue$.__enclos_env__$private$.value <- Inf

    fake_opt <- jmvcore::OptionString$new('evil = {stop("CANARY"); 1}, mytitle', value = "boom", default = "")
    expect_identical(an$.__enclos_env__$private$.sourcifyOption(fake_opt), "")

    call_opt <- jmvcore::OptionString$new("mytitle", value = "x", default = "")
    call_opt$.__enclos_env__$private$.value <- quote(stop("CANARY"))
    expect_identical(an$.__enclos_env__$private$.sourcifyOption(call_opt), "")

    src <- an$asSource()
    parsed <- parse(text = src)
    expect_length(parsed, 1L)
    ast_calls <- character(0)
    walk_calls <- function(expr) {
        if (is.call(expr)) {
            ast_calls <<- c(ast_calls, paste(deparse(expr[[1]]), collapse = ""))
            for (i in seq_along(expr)[-1]) walk_calls(expr[[i]])
        }
    }
    walk_calls(parsed[[1]])
    expect_false("stop" %in% ast_calls)
    mc <- match.call(jjdotchart, parsed[[1]])
    expect_true(all(names(mc)[-1] %in% names(formals(jjdotchart))))

    # 2. Non-finite testvalue (NA, NaN, Inf, -Inf) rejected cleanly with ERROR notice
    for (bad_tv in list(NA_real_, NaN, Inf, -Inf)) {
        res_tv <- jjdotchart(data = d, dep = "v", group = "g", testvalue = bad_tv)
        expect_equal(res_tv$summary$rowCount, 0L)
        expect_match(dc_notices(res_tv), "Reference value must be a finite number", fixed = TRUE)
        expect_false(grepl("zero discrepancy", dc_notices(res_tv), fixed = TRUE))
    }

    # 3. Self-referential {dep}/{group}/{grvar} + HTML tags do not hang format() or inject raw HTML
    d_brace <- data.frame(
        v = c(Inf, d$v[-1]),
        g = d$g,
        s = factor(rep(c("S1", "S2"), length.out = nrow(d))),
        check.names = FALSE
    )
    dep_brace <- "<img src=x onerror=alert(1)>{dep}"
    grp_brace <- "unit{group}"
    grv_brace <- "stratum{grvar}"
    names(d_brace) <- c(dep_brace, grp_brace, grv_brace)

    t0 <- proc.time()[["elapsed"]]
    res_brace <- jjdotchart(
        data = d_brace,
        dep = "<img src=x onerror=alert(1)>{dep}",
        group = "unit{group}",
        grvar = "stratum{grvar}",
        testvalue = 10
    )
    elapsed <- proc.time()[["elapsed"]] - t0
    expect_lt(elapsed, 10)
    expect_identical(res_brace$plot$title, "<img src=x onerror=alert(1)>(dep) by unit(group)")
    expect_identical(
        res_brace$plot2$title,
        "<img src=x onerror=alert(1)>(dep) by unit(group), split by stratum(grvar)"
    )
    expect_false(grepl("<img", res_brace$notices$content %||% "", fixed = TRUE))
    expect_match(res_brace$notices$content %||% "", "&lt;img", fixed = TRUE)

    # 4. Out-of-range options under noThrow = TRUE clamp cleanly without NaN or sprintf errors
    opts_clamp <- pkg_ns$jjdotchartOptions$new(
        dep = "v", group = "g", testvalue = 10,
        centralityplotting = TRUE, showWeightedTable = TRUE
    )
    opts_clamp$.__enclos_env__$private$..conflevel$.__enclos_env__$private$.value <- 1.5
    opts_clamp$.__enclos_env__$private$..k$.__enclos_env__$private$.value <- 500L
    opts_clamp$.__enclos_env__$private$..plotwidth$.__enclos_env__$private$.value <- -50L
    opts_clamp$.__enclos_env__$private$..plotheight$.__enclos_env__$private$.value <- 99999L
    an_clamp <- pkg_ns$jjdotchartClass$new(options = opts_clamp, data = d)
    warns <- character(0)
    withCallingHandlers({
        an_clamp$init(noThrow = TRUE)
        an_clamp$run(noThrow = TRUE)
    }, warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    expect_false(any(grepl("NaNs produced", warns, fixed = TRUE)))
    expect_equal(an_clamp$results$plot$width, 300L)
    expect_equal(an_clamp$results$plot$height, 800L)
    df_clamp <- an_clamp$results$summary$asDF
    expect_true(all(is.finite(df_clamp$ci_low)))
    expect_true(all(is.finite(df_clamp$ci_high)))
    svg_clamp <- dc_render(an_clamp$results)
    expect_true(isTRUE(svg_clamp$ok))
})

test_that("duplicate variable slots, per-item clearWith isolation, all-NA Split By state, and FALSE render guard", {
    pkg_ns <- environment(jjdotchart)
    d <- data.frame(
        score = rep(1:4, each = 10),
        grp = factor(rep(paste0("G", 1:4), each = 10)),
        spl = factor(rep(c("S1", "S2"), length.out = 40)),
        spl_na = factor(rep(NA_character_, 40), levels = c("S1", "S2"))
    )

    # 1. Duplicate variable slots emit a clean ERROR notice with zero R warnings
    warns <- character(0)
    withCallingHandlers({
        r_dup2 <- jjdotchart(data = d, dep = "score", group = "grp", grvar = "grp", testvalue = 2)
        an_dup1 <- pkg_ns$jjdotchartClass$new(
            options = pkg_ns$jjdotchartOptions$new(dep = "score", group = "score", testvalue = 2),
            data = d
        )
        an_dup1$init(noThrow = TRUE)
        an_dup1$run(noThrow = TRUE)
        r_dup1 <- an_dup1$results

        an_dup3 <- pkg_ns$jjdotchartClass$new(
            options = pkg_ns$jjdotchartOptions$new(dep = "score", group = "grp", grvar = "score", testvalue = 2),
            data = d
        )
        an_dup3$init(noThrow = TRUE)
        an_dup3$run(noThrow = TRUE)
        r_dup3 <- an_dup3$results
    }, warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    expect_length(warns, 0L)
    for (r_dup in list(r_dup1, r_dup2, r_dup3)) {
        expect_equal(r_dup$summary$rowCount, 0L)
        expect_match(
            dc_notices(r_dup),
            "Measurement, Groups, and Split By must be distinct variables.",
            fixed = TRUE
        )
    }

    # 2. Per-item clearWith isolates display toggles and cosmetic plot options
    res_cw <- jjdotchart(data = d, dep = "score", group = "grp", grvar = "spl", testvalue = 2)
    cw_of <- function(elem) unlist(elem$.__enclos_env__$private$.clearWith, use.names = FALSE)
    for (item_nm in c("summary", "weightsensitivity", "plot", "plot2")) {
        cw <- cw_of(res_cw[[item_nm]])
        expect_false("showSummaryTable" %in% cw, info = item_nm)
        expect_false("showWeightedTable" %in% cw, info = item_nm)
    }
    for (tbl_nm in c("summary", "weightsensitivity")) {
        cw <- cw_of(res_cw[[tbl_nm]])
        expect_false("mytitle" %in% cw, info = tbl_nm)
        expect_false("plotwidth" %in% cw, info = tbl_nm)
        expect_false("k" %in% cw, info = tbl_nm)
    }
    expect_false("grvar" %in% cw_of(res_cw$weightsensitivity))
    expect_false("grvar" %in% cw_of(res_cw$plot))
    expect_true("grvar" %in% cw_of(res_cw$plot2))

    # 3. All-NA Split By keeps main plot valid while marking plot2 invalid (FALSE)
    res_na_spl <- jjdotchart(data = d, dep = "score", group = "grp", grvar = "spl_na", testvalue = 2)
    expect_identical(res_na_spl$plot$state, TRUE)
    expect_identical(res_na_spl$plot2$state, FALSE)
    expect_match(dc_notices(res_na_spl), "No complete rows remain for Split By", fixed = TRUE)

    # 4. Early renderer exits return FALSE (not NULL)
    opts_inv <- pkg_ns$jjdotchartOptions$new(dep = "score", group = "grp", grvar = "spl", testvalue = 2)
    an_inv <- pkg_ns$jjdotchartClass$new(options = opts_inv, data = d)
    expect_identical(
        an_inv$.__enclos_env__$private$.plot(an_inv$results$plot, ggplot2::theme_bw(), list()),
        FALSE
    )
    expect_identical(
        an_inv$.__enclos_env__$private$.plot2(an_inv$results$plot2, ggplot2::theme_bw(), list()),
        FALSE
    )
})
