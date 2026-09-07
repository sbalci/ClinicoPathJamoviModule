# Release review of lollipop.
#
# NOTE ON READING RESULTS: .run() catches jmvcore::reject() and writes the message
# into the `todo` HTML panel rather than letting it propagate, so a rejection is
# NOT an R error and does NOT appear in `notices`. Assert against todo$content.

lol_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))
lol_todo <- function(res) lol_txt(res$todo$content)
lol_notices <- function(res) lol_txt(paste(as.character(res$notices$content), collapse = " "))
lol_stat <- function(res, name) {
    df <- res$summary$asDF
    v <- df$value[df$statistic == name]
    if (!length(v)) NA_character_ else v[1]
}


test_that("a grouping variable with one real level plus NAs is rejected", {
    # unique() counts NA as a value, so one real category plus any missing value
    # scored 2 and passed the "at least 2 categories" guard - leaving a
    # single-lollipop "comparison" after the complete-case filter.
    d <- data.frame(v = c(1, 2, 3, 4, 5), g = factor(c("a", "a", "a", NA, NA)))
    expect_match(lol_todo(lollipop(data = d, dep = "v", group = "g", highlight = NULL)),
                 "at least 2 different categories")
})

test_that("two real levels plus NAs still runs", {
    d <- data.frame(v = c(1, 2, 3, 4, 5, 6), g = factor(c("a", "a", "b", "b", NA, NA)))
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL, aggregation = "mean")
    expect_gt(nrow(res$summary$asDF), 0)
    expect_false(grepl("Analysis Error", lol_todo(res), fixed = TRUE))
})

test_that("infinite values are removed and disclosed", {
    # complete.cases() follows is.na(), which is FALSE for Inf, so an infinite
    # value survived into the axis range, the mean and the "highest group" claim.
    d <- data.frame(v = c(1, 2, Inf, 4, 5, 6),
                    g = factor(c("a", "a", "a", "b", "b", "b")))
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL, aggregation = "mean")
    expect_match(lol_notices(res), "infinite value")
    # and the surviving statistics are finite
    expect_false(grepl("Inf", lol_stat(res, "Mean Value"), fixed = TRUE))
})

test_that("aggregation reproduces stats::aggregate", {
    set.seed(3)
    d <- data.frame(v = rnorm(60), g = factor(rep(c("a", "b", "c"), each = 20)))
    for (m in c("mean", "median", "sum")) {
        res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL, aggregation = m)
        ref <- stats::aggregate(v ~ g, data = d, FUN = get(m))
        # one plotted point per group
        expect_equal(as.numeric(lol_stat(res, "Number of Groups")), 3, info = m)
        expect_equal(as.numeric(lol_stat(res, "Number of Plotted Points")), 3, info = m)
        # the highest/lowest group claims must match the reference aggregation
        expect_equal(lol_stat(res, "Highest Value Group"),
                     as.character(ref$g[which.max(ref$v)]), info = m)
        expect_equal(lol_stat(res, "Lowest Value Group"),
                     as.character(ref$g[which.min(ref$v)]), info = m)
    }
})

test_that("duplicate observations without aggregation are warned about", {
    # Without aggregation the chart over-plots several points per group, which
    # reads as a single value per group.
    d <- data.frame(v = c(1, 2, 3, 4), g = factor(c("a", "a", "b", "b")))
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL, aggregation = "none")
    expect_match(lol_notices(res), "Duplicate Groups Detected")
})

test_that("a highlight level that is absent is reported, not silently ignored", {
    d <- data.frame(v = c(1, 2, 3, 4), g = factor(c("a", "a", "b", "b")))
    res <- lollipop(data = d, dep = "v", group = "g",
                    useHighlight = TRUE, highlight = "zzz")
    expect_match(lol_notices(res), "does not occur in the grouping variable")
})

test_that("missing rows are removed and the count disclosed", {
    d <- data.frame(v = c(1, 2, NA, 4, 5, 6),
                    g = factor(c("a", "a", "a", "b", "b", "b")))
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL, aggregation = "mean")
    expect_match(lol_notices(res), "had a missing value")
})

test_that("every sort order and orientation renders", {
    set.seed(5)
    d <- data.frame(v = rnorm(30), g = factor(rep(c("a", "b", "c"), each = 10)))
    render <- function(...) {
        res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                        aggregation = "mean", ...)
        f <- tempfile(fileext = ".svg"); svglite::svglite(f, 7, 5)
        ok <- tryCatch({ print(res$plot); TRUE }, error = function(e) conditionMessage(e))
        grDevices::dev.off(); unlink(f)
        ok
    }
    for (s in c("original", "value_asc", "value_desc", "group_alpha"))
        expect_true(isTRUE(render(sortBy = s)), info = s)
    for (o in c("vertical", "horizontal"))
        expect_true(isTRUE(render(orientation = o)), info = o)
})

test_that("a constant dependent variable does not break the chart", {
    d <- data.frame(v = rep(5, 6), g = factor(rep(c("a", "b"), each = 3)))
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL, aggregation = "mean")
    expect_false(grepl("Analysis Error", lol_todo(res), fixed = TRUE))
})

test_that("horizontal sorting is not drawn upside down", {
    # ggplot lays a discrete y scale out BOTTOM-to-top, so using the sorted row
    # order verbatim rendered a horizontal "descending" chart ascending from the
    # top. The first sorted row must be the TOP lollipop in both orientations.
    d <- data.frame(v = c(5, 10, 15, 20), g = factor(c("c", "a", "d", "b")))

    vert <- lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                     aggregation = "mean", sortBy = "value_desc")
    expect_equal(levels(vert$plot$state$data$group), c("b", "d", "a", "c"))

    horiz <- lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                      aggregation = "mean", sortBy = "value_desc",
                      orientation = "horizontal")
    expect_equal(levels(horiz$plot$state$data$group), c("c", "a", "d", "b"))

    alpha <- lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                      aggregation = "mean", sortBy = "group_alpha",
                      orientation = "horizontal")
    expect_equal(levels(alpha$plot$state$data$group), c("d", "c", "b", "a"))
})

test_that("aggregated summary does not call group summaries observations", {
    # "Number of Observations = 4" for a 20-row dataset misreads as the sample
    # size; the row must name what it actually counts.
    d <- data.frame(v = rep(c(1, 2, 3, 4), each = 5),
                    g = factor(rep(c("a", "b", "c", "d"), each = 5)))

    # aggregation must be named: the default is "mean"
    raw <- lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                    aggregation = "none")
    expect_equal(raw$summary$asDF$statistic[1], "Number of Observations")
    expect_equal(raw$summary$asDF$value[1], "20")

    agg <- lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                    aggregation = "mean")
    expect_equal(agg$summary$asDF$statistic[1], "Number of Plotted Points")
    expect_equal(agg$summary$asDF$value[1], "4")
})

test_that("the mean reference line discloses that it averages group summaries", {
    # Under aggregation the plotted points ARE the group summaries, so the line
    # is an unweighted mean of means: 30/3/3 observations at 10/50/52 put it at
    # 37.3 while the grand mean of the raw data is 16.8.
    d <- data.frame(g = factor(c(rep("A", 30), rep("B", 3), rep("C", 3))),
                    v = c(rep(10, 30), rep(50, 3), rep(52, 3)))
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                    aggregation = "mean", showMean = TRUE)
    expect_equal(mean(res$plot$state$data$dependent), 37 + 1/3, tolerance = 1e-6)
    expect_false(isTRUE(all.equal(mean(res$plot$state$data$dependent), mean(d$v))))
    expect_match(lol_notices(res), "unweighted mean of the 3 plotted group summaries")

    # ...and only when the two can actually diverge
    balanced <- data.frame(g = factor(rep(c("A", "B", "C"), each = 4)), v = 1:12)
    expect_false(grepl("unweighted mean",
        lol_notices(lollipop(data = balanced, dep = "v", group = "g", highlight = NULL,
                             aggregation = "mean", showMean = TRUE))))
})

test_that("baseline misuse is disclosed but a diverging zero baseline is not", {
    d <- data.frame(g = factor(paste0("Pt", 1:8)),
                    v = c(13.1, 13.2, 13.25, 13.3, 13.35, 13.4, 13.5, 13.57))
    # baseline 0 leaves the differences occupying a few percent of each stem
    expect_match(lol_notices(lollipop(data = d, dep = "v", group = "g", highlight = NULL)),
                 "Baseline Far From the Data")
    # a typed baseline inside the range silently flips stem direction
    expect_match(lol_notices(lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                                      baseline = 13.3)),
                 "sits inside the range")
    # but a zero baseline on data that straddles zero is the canonical
    # diverging lollipop and must stay quiet
    z <- data.frame(g = factor(paste0("G", 1:6)), v = c(-3, -2, -1, 1, 2, 3))
    expect_false(grepl("Baseline", lol_notices(lollipop(data = z, dep = "v", group = "g",
                                                        highlight = NULL))))
})

test_that("aggregation discloses the within-group spread it discards", {
    set.seed(4)
    d <- data.frame(g = factor(rep(c("A", "B"), each = 20)),
                    v = c(rnorm(20, 10, 0.1), rnorm(20, 10.2, 8)))
    expect_match(lol_notices(lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                                      aggregation = "mean")),
                 "shows less disagreement than the data contains")
})

test_that("the two recalibrated heuristics no longer fire on correct data", {
    # One value per category is the canonical lollipop input, not a reason to
    # "use a different visualization".
    for (k in c(12, 25, 40)) {
        d <- data.frame(g = factor(sprintf("Cat%02d", 1:k)), v = seq(10, 30, length.out = k))
        expect_false(grepl("Thinly Replicated",
                           lol_notices(lollipop(data = d, dep = "v", group = "g",
                                                highlight = NULL))),
                     info = paste("k =", k))
    }
    # range > 5*SD is not scale free - it fires on clean normal data purely for
    # being large. The far-out rule must not.
    set.seed(11)
    for (k in c(150, 400)) {
        d <- data.frame(g = factor(rep(sprintf("G%03d", 1:(k / 5)), each = 5)),
                        v = rnorm(k, 100, 10))
        expect_false(grepl("Extreme Values",
                           lol_notices(lollipop(data = d, dep = "v", group = "g",
                                                highlight = NULL, aggregation = "mean"))),
                     info = paste("n =", k))
    }
    # a genuine far-out point still gets flagged
    d <- data.frame(g = factor(paste0("G", 1:12)), v = c(1:11, 900))
    expect_match(lol_notices(lollipop(data = d, dep = "v", group = "g", highlight = NULL)),
                 "three interquartile ranges")
})

test_that("asSource() escapes text options so the syntax pane parses", {
    # jmvcore's default sourcify wraps a String option's value in quotes without
    # escaping it, so a chart titled  Hb ("g/dL")  emitted
    #   title = "Hb ("g/dL")"
    # which does not parse. The deparse branch tests OptionString (not
    # OptionVariable, which merely inherits from it) so variable names, factor
    # Levels and the free-text boxes are all escaped.
    d <- data.frame(`Tumor Grade` = factor(c("a", 'G "hi"', "a", 'G "hi"')),
                    `Ki-67 %` = c(1, 2, 3, 4), check.names = FALSE)
    opts <- lollipopOptions$new(dep = "Ki-67 %", group = "Tumor Grade",
                                useHighlight = TRUE, highlight = 'G "hi"',
                                title = 'Hb ("g/dL") \\x', xlabel = "", ylabel = "Y'lab")
    an <- lollipopClass$new(options = opts, data = d)
    an$run()
    src <- an$asSource()

    expect_silent(parsed <- parse(text = src))
    args <- as.list(eval(parse(text = paste0("quote(", src, ")"))))
    expect_identical(args$title, 'Hb ("g/dL") \\x')
    expect_identical(args$group, "Tumor Grade")
    expect_identical(args$highlight, 'G "hi"')
    # options still at their default are not emitted
    expect_null(args$xlabel)
})

test_that("highest/lowest name the groups the chart actually draws", {
    # These read the group MEANS while "Value Range" two rows above read the raw
    # extremes - two bases in one table. Group A holding {1, 100} draws the
    # tallest lollipop, but the mean picked B.
    d <- data.frame(g = factor(c("A", "A", "B", "B")), v = c(1, 100, 60, 60))
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL,
                    aggregation = "none")
    expect_equal(lol_stat(res, "Highest Value Group"), "A")
    expect_equal(lol_stat(res, "Value Range"), "1 - 100")

    # ties name every tied group instead of silently taking the first
    tied <- data.frame(g = factor(c("A", "B", "C")), v = c(5, 5, 1))
    expect_equal(lol_stat(lollipop(data = tied, dep = "v", group = "g", highlight = NULL),
                          "Highest Value Group"), "A, B")

    # unchanged under aggregation, where the plotted value IS the group summary
    set.seed(9)
    d2 <- data.frame(g = factor(rep(c("A", "B", "C", "D"), each = 7)),
                     v = c(rnorm(7, 10), rnorm(7, 20), rnorm(7, 5), rnorm(7, 15)))
    r2 <- lollipop(data = d2, dep = "v", group = "g", highlight = NULL, aggregation = "mean")
    means <- tapply(d2$v, d2$g, mean)
    expect_equal(lol_stat(r2, "Highest Value Group"), names(which.max(means)))
    expect_equal(lol_stat(r2, "Lowest Value Group"), names(which.min(means)))
})

test_that("aggregation defaults to mean, so the chart does not over-plot", {
    d <- data.frame(g = factor(rep(c("A", "B", "C", "D"), each = 5)), v = 1:20)
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL)
    expect_equal(nrow(res$plot$state$data), 4)
    expect_false(grepl("Duplicate Groups Detected", lol_notices(res)))
    expect_equal(res$summary$asDF$statistic[1], "Number of Plotted Points")
})
