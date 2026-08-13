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
        expect_equal(as.numeric(lol_stat(res, "Number of Observations")), 3, info = m)
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
    expect_match(lol_notices(res), "Multiple observations per group")
})

test_that("a highlight level that is absent is reported, not silently ignored", {
    d <- data.frame(v = c(1, 2, 3, 4), g = factor(c("a", "a", "b", "b")))
    res <- lollipop(data = d, dep = "v", group = "g",
                    useHighlight = TRUE, highlight = "zzz")
    expect_match(lol_notices(res), "not found in grouping variable")
})

test_that("missing rows are removed and the count disclosed", {
    d <- data.frame(v = c(1, 2, NA, 4, 5, 6),
                    g = factor(c("a", "a", "a", "b", "b", "b")))
    res <- lollipop(data = d, dep = "v", group = "g", highlight = NULL, aggregation = "mean")
    expect_match(lol_notices(res), "missing values were removed")
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
