# Regression tests from the `jwaffle` release review.
#
# A waffle chart is drawn with waffle::geom_waffle(make_proportional = TRUE), which lays the
# data out on a 100-square grid: one square per percentage point. Anything below 0.5% of the
# total therefore rounds to zero squares and is simply not drawn.

wf_run <- function(dat, ...) {
    o <- do.call(ClinicoPath:::jwaffleOptions$new,
                 utils::modifyList(list(groups = "g"), list(...)))
    a <- ClinicoPath:::jwaffleClass$new(options = o, data = dat)
    a$init()
    tryCatch(a$.__enclos_env__$private$.run(), error = function(e) NULL)
    a
}
notices_of <- function(a) {
    v <- tryCatch(a$results$notices$content, error = function(e) "")
    if (is.null(v)) "" else v
}
fills_drawn <- function(dat) {
    ag <- stats::aggregate(list(count = rep(1, nrow(dat))), by = list(g = dat$g), FUN = sum)
    gp <- ggplot2::ggplot(ag, ggplot2::aes(fill = g, values = count)) +
          waffle::geom_waffle(n_rows = 5, make_proportional = TRUE)
    length(unique(ggplot2::ggplot_build(gp)$data[[1]]$fill))
}


test_that("categories too small to draw are named, and the warning tracks the chart exactly", {
    # Measured before the fix: 700/297/3 drew only two fill colours, and 70000/29700/300
    # (300 cases, 0.3%) likewise -- with no warning of any kind, while the Analysis Summary
    # went on reporting "Rare: 0.3% (n=300)". Figure and text disagreed and nothing said so.
    skip_if_not_installed("waffle")
    for (n_rare in c(1, 3, 4, 5, 6, 10, 50)) {
        d <- data.frame(g = factor(c(rep("A", 700), rep("B", 1000 - 700 - n_rare),
                                     rep("C", n_rare))))
        lost   <- fills_drawn(d) < 3
        warned <- grepl("missing from the chart", notices_of(wf_run(d)), fixed = TRUE)
        expect_equal(warned, lost, info = paste0("C n=", n_rare, " (", 100 * n_rare / 1000, "%)"))
    }
    # the boundary is exactly 0.5%: 0.4% is dropped, 0.5% is drawn
    d04 <- data.frame(g = factor(c(rep("A", 700), rep("B", 296), rep("C", 4))))
    d05 <- data.frame(g = factor(c(rep("A", 700), rep("B", 295), rep("C", 5))))
    expect_true(grepl("missing from the chart", notices_of(wf_run(d04)), fixed = TRUE))
    expect_false(grepl("missing from the chart", notices_of(wf_run(d05)), fixed = TRUE))

    # the message names the category and its share
    n <- notices_of(wf_run(d04))
    expect_match(n, "C", fixed = TRUE)
    expect_match(n, "0.40%", fixed = TRUE)
})


test_that("the vanishing-category check honours the counts (weights) variable", {
    # A weighted table can hide a tiny category just as an unweighted one can, and the shares
    # must be computed from the weights, not the row counts.
    d <- data.frame(g = factor(c("A", "B", "C")), w = c(700, 297, 3))
    expect_true(grepl("missing from the chart",
                      notices_of(wf_run(d, counts = "w")), fixed = TRUE))
    # equal weights -> nothing missing
    d2 <- data.frame(g = factor(c("A", "B", "C")), w = c(300, 350, 350))
    expect_false(grepl("missing from the chart",
                       notices_of(wf_run(d2, counts = "w")), fixed = TRUE))
})


test_that("a missing weight does not abort the analysis", {
    # xtabs() propagates an NA weight into the cell total, which made the share total NA and
    # turned the guard into `if (NA)` -> "missing value where TRUE/FALSE needed", killing the
    # whole analysis. Missing weights are ordinary in clinical data.
    d <- data.frame(g = factor(rep(c("A", "B", "C"), each = 10)),
                    w = c(rep(5, 10), rep(3, 10), c(NA, rep(2, 9))))
    expect_no_error(ClinicoPath::jwaffle(data = d, groups = "g", counts = "w"))

    # an all-missing weight column is refused with a clear message, not an opaque one
    d2 <- d; d2$w <- NA_real_
    msg <- tryCatch({ ClinicoPath::jwaffle(data = d2, groups = "g", counts = "w"); NA_character_ },
                    error = conditionMessage)
    expect_false(is.na(msg))
    expect_false(grepl("missing value where TRUE/FALSE needed", msg, fixed = TRUE))
})


test_that("the vanishing-category list is capped so the notice stays readable", {
    # A continuous variable coerced to a factor can produce a hundred vanishing levels.
    d <- data.frame(g = factor(c(rep("Big", 900), paste0("tiny", 1:100))))
    n <- notices_of(wf_run(d))
    expect_match(n, "missing from the chart", fixed = TRUE)
    expect_match(n, "more", fixed = TRUE)          # "... and N more"
    expect_lt(nchar(gsub("<[^>]*>", "", n)), 1200) # was 2063 characters unbounded
})


test_that("rejection messages interpolate their arguments instead of printing {}", {
    # jmvcore::reject's formals are (formats, code, ...), so a value passed second becomes
    # the error CODE: the first {} was filled from the third argument and the last {} was
    # left literal. Verified: reject("Grouping variable {} ... not {}", "MyVar", "numeric")
    # rendered as "Grouping variable numeric ... not {}".
    d <- data.frame(g = factor(rep(c("A", "B"), each = 20)))
    msg <- tryCatch({
        ClinicoPath::jwaffle(data = d, groups = "g", facet = "no_such_column")
        NA_character_
    }, error = conditionMessage)
    expect_false(is.na(msg))
    expect_false(grepl("{}", msg, fixed = TRUE))

    # a genuinely non-categorical grouping variable is named in its own message
    d2 <- data.frame(g = c(rnorm(30), rnorm(30) + 5))
    o <- ClinicoPath:::jwaffleOptions$new(groups = "g")
    a <- ClinicoPath:::jwaffleClass$new(options = o, data = d2)
    a$init()
    m2 <- tryCatch({ a$.__enclos_env__$private$.validateInputs(); NA_character_ },
                   error = conditionMessage)
    expect_false(is.na(m2))
    expect_match(m2, "'g'", fixed = TRUE)          # names the variable, not its class
    expect_false(grepl("{}", m2, fixed = TRUE))
})


test_that("the palette scales to any number of categories", {
    # colorRampPalette() interpolates, so unlike a fixed-length manual scale this cannot hit
    # "Insufficient values in manual scale".
    skip_if_not_installed("waffle")
    for (k in c(2, 3, 7, 30)) {
        d <- data.frame(g = factor(rep(paste0("L", seq_len(k)), length.out = 600)))
        a <- wf_run(d)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        ok <- tryCatch({ a$.__enclos_env__$private$.plot(a$results$plot); TRUE },
                       error = function(e) FALSE)
        grDevices::dev.off()
        expect_true(ok, info = paste(k, "categories"))
    }
})
