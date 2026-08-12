# Residual regression tests for `jwaffle` (round 2 of the release review).
# One test per defect fixed; each asserts what the USER sees, not the mechanism.

wfr <- function(dat, ...) {
    o <- do.call(ClinicoPath:::jwaffleOptions$new,
                 utils::modifyList(list(groups = "g"), list(...)))
    a <- ClinicoPath:::jwaffleClass$new(options = o, data = dat)
    a$init()
    a
}
wfr_run_err <- function(a) {
    tryCatch({ a$.__enclos_env__$private$.run(); NA_character_ },
             error = function(e) conditionMessage(e))
}
wfr_build <- function(a) {
    tmp <- tempfile(fileext = ".png")
    grDevices::png(tmp, width = 700, height = 550)
    on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
    a$.__enclos_env__$private$.plot(NULL)
    a$.__enclos_env__$private$.cached_plot
}

test_that("an Inf weight is rejected by name instead of poisoning the summary and the chart", {
    dat <- data.frame(g = rep(c("A", "B", "C"), each = 3),
                      w = c(rep(1, 8), Inf), stringsAsFactors = FALSE)
    a <- wfr(dat, counts = "w", showSummaries = TRUE)
    err <- wfr_run_err(a)

    expect_true(grepl("non-finite", err, ignore.case = TRUE))
    expect_true(grepl("'w'", err, fixed = TRUE))
    # The old behaviour: "C: NaN% (n=Inf) ... A represents the largest proportion".
    summary_txt <- paste(a$results$analysisSummary$content, collapse = "")
    expect_false(grepl("NaN", summary_txt, fixed = TRUE))
    expect_false(grepl("Inf", summary_txt, fixed = TRUE))
})

test_that("a counts variable that sums to zero is rejected instead of failing inside waffle", {
    dat <- data.frame(g = rep(c("A", "B", "C"), each = 3), w = 0)
    a <- wfr(dat, counts = "w", showSummaries = TRUE)
    err <- wfr_run_err(a)

    expect_true(grepl("sums to zero", err, fixed = TRUE))
    # The old behaviour: .run() finished with "Waffle chart created successfully" while
    # .plot() died with tail.default()'s "invalid 'n'".
    expect_false(grepl("created successfully",
                       paste(a$results$todo$content, collapse = ""), fixed = TRUE))
})

test_that("the rare-categories notice keeps its body (the '<5' is not eaten as a tag)", {
    dat <- data.frame(g = c(rep("Big", 20), rep("Mid", 20), rep("Rare", 4)),
                      stringsAsFactors = FALSE)
    a <- wfr(dat)
    expect_true(is.na(wfr_run_err(a)))

    notices <- paste(a$results$notices$content, collapse = "")
    expect_true(grepl("5 cases: Rare", notices, fixed = TRUE))
    expect_true(grepl("Consider combining rare categories", notices, fixed = TRUE))
})

test_that("Chart Title renders as a title, not as caption text", {
    dat <- data.frame(g = rep(c("A", "B"), each = 25), stringsAsFactors = FALSE)

    p <- wfr_build(wfr(dat, mytitle = "My Chart Title"))
    expect_identical(p$labels$title, "My Chart Title")
    cap <- if (is.null(p$labels$caption)) "" else p$labels$caption
    expect_false(grepl("My Chart Title", cap, fixed = TRUE))
    # and the figure is actually drawn
    expect_gt(nrow(ggplot2::ggplot_build(p)$data[[1]]), 0)

    p0 <- wfr_build(wfr(dat))
    expect_null(p0$labels$title)
})

test_that("asSource() emits parseable R for a title containing quotes and backslashes", {
    dat <- data.frame(g = rep(c("A", "B"), each = 5), stringsAsFactors = FALSE)
    a <- wfr(dat, mytitle = "He said \"hi\"\\x")
    src <- a$asSource()

    expect_silent(parsed <- parse(text = src))
    expect_true(grepl("mytitle", src, fixed = TRUE))
    # empty String options stay out of the generated call
    expect_false(grepl("legendtitle", src, fixed = TRUE))
})
