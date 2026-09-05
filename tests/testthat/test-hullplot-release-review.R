# Release-review regression tests for hullplot.
#
# The defects these cover share a root: rows and factor levels that the analysis
# had already discarded, or should have discarded, kept influencing what the
# user was told. Infinite values survived complete.cases() into the group
# statistics and the copy-ready summary; a group whose every row was dropped
# survived as an empty factor level and was still counted; excluded rows were
# never disclosed at all.
#
# Every expectation below was observed on the unfixed code before being written.

library(testthat)

hp_data <- function(seed = 3) {
    set.seed(seed)
    data.frame(
        x     = c(rnorm(20, 0), rnorm(20, 4)),
        y     = c(rnorm(20, 0), rnorm(20, 3)),
        group = factor(rep(c("A", "B"), each = 20)),
        col   = factor(rep(c("p", "q"), length.out = 40)),
        sz    = runif(40, 1, 5)
    )
}

hp_run <- function(data = hp_data(), ...) {
    opts <- do.call(
        ClinicoPath:::hullplotOptions$new,
        utils::modifyList(
            list(x_var = "x", y_var = "y", group_var = "group",
                 show_statistics = TRUE, outlier_detection = TRUE,
                 show_summary = TRUE),
            list(...)))
    a <- ClinicoPath:::hullplotClass$new(options = opts, data = data)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}
hp_plot_data <- function(a) a$.__enclos_env__$private$.prepared_data$data
hp_text <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", html))

# Render the plot and count non-white pixels. Used to show that an option that
# claims to change the figure actually changes it.
hp_ink <- function(a) {
    skip_if_not_installed("png")
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 800, 600, res = 96)
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    a$.__enclos_env__$private$.plot(a$results$plot,
                                    ggtheme = ggplot2::theme_bw(), theme = NULL)
    grDevices::dev.off(); on.exit()
    img <- png::readPNG(f)
    sum(apply(img[, , 1:3], c(1, 2), function(p) any(p < 0.98)))
}


test_that("infinite values are excluded rather than propagated into the results", {
    # complete.cases() treats only NA as missing, so Inf reached the group
    # statistics table as "Inf +/- NaN" and, because the centroid distance
    # became infinite, drove the discriminability index above every threshold -
    # printing "Groups appear well-separated" in text offered for publication.
    d <- hp_data()
    d$x[1:5] <- Inf
    a <- hp_run(d)

    expect_equal(nrow(hp_plot_data(a)), 35L)
    expect_true(all(is.finite(hp_plot_data(a)$x)))

    stats <- hp_text(a$results$statistics$content)
    expect_false(grepl("Inf", stats, fixed = TRUE))
    expect_false(grepl("NaN", stats, fixed = TRUE))
    expect_match(stats, "35 complete observations")

    # -Inf and NaN travel the same path
    for (bad in list(-Inf, NaN)) {
        d2 <- hp_data(); d2$y[1:4] <- bad
        expect_equal(nrow(hp_plot_data(hp_run(d2))), 36L)
    }
})


test_that("an infinite value cannot flip the separation verdict", {
    # Two groups deliberately placed on top of each other: the honest verdict is
    # "overlapping". A single Inf used to make the centroid distance infinite
    # and the verdict "well-separated".
    set.seed(11)
    d <- data.frame(x = rnorm(40), y = rnorm(40),
                    group = factor(rep(c("A", "B"), each = 20)))
    honest <- hp_text(hp_run(d)$results$summary$content)
    expect_match(honest, "Standardized centroid separation is described as overlapping")

    d$x[1] <- Inf
    poisoned <- hp_text(hp_run(d)$results$summary$content)
    expect_match(poisoned, "Standardized centroid separation is described as overlapping")
    expect_false(grepl("well-separated", poisoned, fixed = TRUE))
})


test_that("a group emptied by missing data stops being counted as a group", {
    # as.factor() on an existing factor preserves unused levels, so a group
    # whose every row was dropped survived in levels(): the panels reported
    # "3 groups" for a two-group plot and the outlier table carried a row for a
    # group that was not in the data.
    d <- hp_data()
    d$x[d$group == "B"] <- NA
    a <- hp_run(d)

    expect_equal(levels(hp_plot_data(a)$group), "A")
    expect_equal(nrow(hp_plot_data(a)), 20L)
    expect_match(hp_text(a$results$interpretation$content), "Groups: 1 group defined by")
    expect_false(grepl(" B:", hp_text(a$results$outliers$content), fixed = TRUE))

    # a colour variable emptied the same way must not keep a phantom level either
    d2 <- hp_data()
    d2$y[d2$col == "q"] <- NA
    a2 <- hp_run(d2, color_var = "col")
    cm <- a2$.__enclos_env__$private$.prepared_data$color_mapping
    expect_equal(levels(hp_plot_data(a2)[[cm]]), "p")
})


test_that("excluded rows are disclosed", {
    # The interpretation panel reported the surviving N ("Observations: 35 data
    # points") with nothing to say that five rows had been removed.
    d <- hp_data(); d$x[1:5] <- NA
    txt <- hp_text(hp_run(d)$results$interpretation$content)
    expect_match(txt, "5 of 40 rows", fixed = TRUE)
    expect_match(txt, "35 rows were plotted", fixed = TRUE)
    expect_match(txt, "Observations: 35 data points", fixed = TRUE)

    # infinite values are called out separately, because they signal a data
    # problem rather than an ordinary missing value
    d2 <- hp_data(); d2$x[1:3] <- Inf
    txt2 <- hp_text(hp_run(d2)$results$interpretation$content)
    expect_match(txt2, "3 infinite/undefined value", fixed = TRUE)

    # complete data says nothing
    expect_false(grepl("rows were plotted",
                       hp_text(hp_run()$results$interpretation$content), fixed = TRUE))
})


test_that("the grouping variable cannot be an axis variable", {
    # plot_data was built with the user's own column names, so choosing the X
    # variable as the grouping variable produced two columns named "x" and
    # `[[<-` converted the FIRST one - the X axis - to a factor. The plot then
    # drew categorical X data with no error and no warning.
    d <- hp_data()
    expect_error(hp_run(d, group_var = "x"), "Grouping Variable must differ")
    expect_error(hp_run(d, group_var = "y"), "Grouping Variable must differ")
    # the pre-existing x/y guard must stay
    expect_error(hp_run(d, x_var = "x", y_var = "x"), "must be different")
    # and the legitimate combination still runs
    expect_silent(invisible(hp_run(d)))
})


test_that("hull boundary expansion actually moves the boundary", {
    # `hull_expand` is declared 0-1 with a 0.05 default but was handed to
    # ggforce as millimetres, against its own default of unit(5, "mm"): the
    # entire option range grew the inked hull by 68 px out of 374,400 (0.018%).
    d <- hp_data()
    ink <- vapply(c(0, 0.2, 0.5), function(v) hp_ink(hp_run(d, hull_expand = v)), numeric(1))
    expect_true(all(diff(ink) > 0))
    # the full range must be a large, obvious change, not a rounding artefact
    expect_gt(ink[3] / ink[1], 2)

    b <- readLines("../../R/hullplot.b.R", warn = FALSE)
    expect_true(any(grepl('grid::unit(self$options$hull_expand, "npc")', b, fixed = TRUE)))
    expect_false(any(grepl('grid::unit(self$options$hull_expand, "mm")', b, fixed = TRUE)))
})


test_that("a single group does not get a comparative conclusion", {
    # The copy-ready paragraph is offered for use in manuscripts. With one group
    # it read "revealed 1 distinct groups ... Groups appear single cohort (no
    # comparison available) in the two-dimensional space", and the clinical
    # interpretation asserted "substantial overlap between categories" when
    # there was only one category.
    d <- hp_data(); d$group <- factor("only")
    txt <- hp_text(hp_run(d)$results$summary$content)

    expect_match(txt, "described a single group", fixed = TRUE)
    expect_match(txt, "No between-group comparison is possible", fixed = TRUE)
    expect_false(grepl("1 distinct groups", txt, fixed = TRUE))
    expect_false(grepl("substantial overlap between categories", txt, fixed = TRUE))
    expect_false(grepl("Groups appear single cohort", txt, fixed = TRUE))

    # two groups keep the comparative wording
    txt2 <- hp_text(hp_run()$results$summary$content)
    expect_match(txt2, "revealed 2 distinct groups", fixed = TRUE)
    expect_match(txt2, "Standardized centroid separation is described as ", fixed = TRUE)
})


test_that("an empty dataset explains itself instead of rendering nothing", {
    a <- hp_run(hp_data()[0, ])
    expect_match(hp_text(a$results$todo$content), "No data to plot")
    expect_match(hp_text(a$results$todo$content), "row filters")
    # and the plot declines rather than erroring
    expect_false(a$.__enclos_env__$private$.plot(
        a$results$plot, ggtheme = ggplot2::theme_bw(), theme = NULL))
})


test_that("group statistics agree with an independent computation", {
    d <- hp_data()
    a <- hp_run(d)
    txt <- hp_text(a$results$statistics$content)

    for (g in c("A", "B")) {
        sub <- d[d$group == g, ]
        expect_match(txt, sprintf("%s %d %s", g, nrow(sub),
                                  format(round(mean(sub$x), 2), nsmall = 2)),
                     fixed = TRUE)
    }
    # the reported N per group must be the plotted N, not the input N
    d2 <- d; d2$y[d2$group == "A"][1:6] <- NA
    expect_match(hp_text(hp_run(d2)$results$statistics$content), "A 14 ", fixed = TRUE)
})


test_that("outlier counts match the 1.5 x IQR rule", {
    d <- hp_data()
    # plant an unambiguous outlier in group A
    d$x[1] <- 1000
    a <- hp_run(d)
    sub <- d[d$group == "A", ]
    q <- stats::quantile(sub$x, c(0.25, 0.75))
    iqr <- q[2] - q[1]
    expect_true(sub$x[1] > q[2] + 1.5 * iqr)
    expect_match(hp_text(a$results$outliers$content), "A: 1 potential outliers")

    # groups too small for quartiles are reported as such, not as zero
    small <- rbind(d[d$group == "A", ][1:4, ], d[d$group == "B", ])
    small$group <- droplevels(small$group)
    expect_match(hp_text(hp_run(small)$results$outliers$content),
                 "A: n too small for reliable outlier detection")
})


test_that("the convex-hull fallback renders when concaveman is unavailable", {
    # ggforce::geom_mark_hull needs V8 + concaveman. Both are installed here, so
    # the fallback branch never ran in any test: it is only reachable on a
    # machine without them, which is exactly where a break would go unnoticed.
    # Mock the availability check to force it.
    real_rn <- base::requireNamespace
    fake_rn <- function(package, ..., quietly = FALSE)
        if (package %in% c("V8", "concaveman")) FALSE
        else real_rn(package, ..., quietly = quietly)

    render <- function(a) {
        f <- tempfile(fileext = ".png")
        grDevices::png(f, 700, 550)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        a$.__enclos_env__$private$.plot(a$results$plot,
                                        ggtheme = ggplot2::theme_bw(), theme = NULL)
        grDevices::dev.off(); on.exit()
        ggplot2::last_plot()
    }
    geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))

    # concave path: one ggforce layer, no caption
    p_concave <- render(hp_run())
    expect_true("GeomMarkHull" %in% geoms(p_concave))
    expect_null(p_concave$labels$caption)

    # convex fallback: chull polygons plus centroid labels, and the substitution
    # is disclosed in the caption rather than made silently
    p_convex <- testthat::with_mocked_bindings(
        render(hp_run()), requireNamespace = fake_rn, .package = "base")
    expect_false("GeomMarkHull" %in% geoms(p_convex))
    expect_true(all(c("GeomPolygon", "GeomText", "GeomPoint") %in% geoms(p_convex)))
    expect_match(p_convex$labels$caption, "showing convex hulls")

    # the label layer follows show_labels on this path too
    p_nolab <- testthat::with_mocked_bindings(
        render(hp_run(show_labels = FALSE)), requireNamespace = fake_rn, .package = "base")
    expect_false("GeomText" %in% geoms(p_nolab))

    # a group with fewer than 3 points has no hull; it must not break the render
    d <- hp_data()
    d <- rbind(d[d$group == "A", ][1:2, ], d[d$group == "B", ])
    d$group <- droplevels(d$group)
    expect_no_error(testthat::with_mocked_bindings(
        render(hp_run(d)), requireNamespace = fake_rn, .package = "base"))
})


test_that("variable arguments must be passed by value, not as bare symbols", {
    # jmvcore's generated wrappers use non-standard evaluation: a bare symbol
    # resolves to its own NAME, so `x_var = x_var` asks for a column literally
    # called "x_var". Every requested column is then absent, jmvcore::select()
    # builds a 0-column data frame, and copying the original row names onto it
    # dies with "invalid 'row.names' length" - an error that names neither the
    # option nor the column. This was the cause of the long-standing "flaky"
    # errors in test-hullplot-integration.R and test-jjcorrmat-integration.R.
    d <- hp_data()
    xv <- "x"; yv <- "y"; gv <- "group"

    expect_error(hullplot(data = d, x_var = xv, y_var = yv, group_var = gv),
                 "invalid 'row.names' length")
    # forcing evaluation is the fix
    expect_s3_class(hullplot(data = d, x_var = !!xv, y_var = !!yv, group_var = !!gv),
                    "hullplotResults")
    expect_s3_class(hullplot(data = d, x_var = "x", y_var = "y", group_var = "group"),
                    "hullplotResults")
})
