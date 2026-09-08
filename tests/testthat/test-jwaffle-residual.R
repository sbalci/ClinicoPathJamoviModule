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
    # the literal "<5" (now "fewer than 5") must survive the HTML tag-stripper, and so
    # must the remedy sentence after it -- the old bug ate everything between the "<"
    # and the next ">".
    expect_true(grepl("5 cases: Rare", notices, fixed = TRUE))
    expect_true(grepl("consider combining them into an 'Other' group", notices,
                      fixed = TRUE))
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

test_that("the faceted summary lists categories in factor-level order, not alphabetically", {
    # Levels G1 < G2 < G10 sort alphabetically as G1, G10, G2. The chart legend uses
    # level order, so an alphabetical text summary contradicted the figure for any
    # ordinal scale (tumour grade, stage, risk band).
    set.seed(42)
    dat <- data.frame(
        g = factor(sample(c("G1", "G2", "G10"), 300, TRUE), levels = c("G1", "G2", "G10")),
        f = factor(sample(c("X", "Y"), 300, TRUE)))
    a <- wfr(dat, facet = "f", showSummaries = TRUE)
    a$.__enclos_env__$private$.run()

    txt <- gsub("<[^>]*>", "", a$results$analysisSummary$content)
    pos <- vapply(c("G1:", "G2:", "G10:"), function(p) regexpr(p, txt, fixed = TRUE)[1],
                  numeric(1))
    expect_true(all(pos > 0))
    expect_false(is.unsorted(pos))
})

test_that("rows dropped for missing values are reported in a notice, not only on the console", {
    set.seed(43)
    dat <- data.frame(g = factor(sample(c("A", "B", "C"), 200, TRUE)),
                      w = runif(200, 1, 3))
    dat$g[1:15] <- NA
    dat$w[20:25] <- NA
    a <- wfr(dat, counts = "w")
    a$.__enclos_env__$private$.run()

    notice <- a$results$notices$content
    expect_true(grepl("Missing Data", notice, fixed = TRUE))
    expect_true(grepl("21 of 200", notice, fixed = TRUE))
    expect_true(grepl("179", notice, fixed = TRUE))
})

test_that("no missing-data notice is emitted for complete data", {
    set.seed(44)
    dat <- data.frame(g = factor(sample(c("A", "B", "C"), 200, TRUE)))
    a <- wfr(dat)
    a$.__enclos_env__$private$.run()

    expect_false(grepl("Missing Data", a$results$notices$content, fixed = TRUE))
})

test_that("faceted panels carry their own n, because each panel is normalised to 100 squares", {
    # waffle's make_proportional draws every panel at 100 squares, so a 10-case panel
    # is exactly as large as a 110-case one. Without n on the strip the figure invites
    # a comparison that carries no information.
    set.seed(45)
    dat <- data.frame(g = factor(sample(c("A", "B", "C"), 120, TRUE)),
                      f = factor(c(rep("X", 10), rep("Y", 110))))
    a <- wfr(dat, facet = "f")
    p <- wfr_build(a)

    g <- ggplot2::ggplotGrob(p)
    strips <- unlist(lapply(g$grobs[grepl("strip", g$layout$name)],
                            function(s) tryCatch(s$grobs[[1]]$children[[2]]$children[[1]]$label,
                                                 error = function(e) NA_character_)))
    strips <- stats::na.omit(strips)
    expect_true(any(grepl("X (n=10)", strips, fixed = TRUE)))
    expect_true(any(grepl("Y (n=110)", strips, fixed = TRUE)))

    # and the caption states the grand total, which it previously omitted entirely
    expect_true(grepl("total n=120", p$labels$caption, fixed = TRUE))
})

test_that("the notice block takes the highest severity raised, not the last one", {
    # A category under 0.5% of the total is dropped from the chart -- a STRONG_WARNING.
    # Plain WARNINGs (rare categories, missing rows) are raised alongside it; the block
    # must not be downgraded to WARNING by them.
    set.seed(46)
    dat <- data.frame(g = factor(c(rep("A", 700), rep("B", 290), rep("C", 3),
                                   rep(NA, 7))))
    a <- wfr(dat)
    a$.__enclos_env__$private$.run()

    notice <- a$results$notices$content
    expect_true(startsWith(notice, "IMPORTANT:"))
    expect_true(grepl("missing from the chart", notice, fixed = TRUE))
    # no ragged runs of blank lines left over from the stripped <br> markers
    expect_false(grepl("\n\n\n", notice, fixed = TRUE))
})

test_that("a large dataset is described by what actually changes, not by a false slow-render claim", {
    set.seed(47)
    dat <- data.frame(g = factor(sample(c("A", "B", "C"), 150000, TRUE)))
    a <- wfr(dat)
    a$.__enclos_env__$private$.run()

    notice <- a$results$notices$content
    expect_true(grepl("150,000 rows", notice, fixed = TRUE))
    expect_true(grepl("1,500 cases", notice, fixed = TRUE))
    expect_false(grepl("slow", notice, ignore.case = TRUE))
})

test_that("the plot renderer returns FALSE rather than NULL when there is nothing to draw", {
    # Built without init(): jmvcore::select() raises "invalid 'row.names' length" when
    # the option set names no variables at all, which is upstream of anything jwaffle
    # does. The renderer itself still has to answer FALSE, not NULL -- jamovi reads the
    # return value to decide whether anything was drawn.
    o <- ClinicoPath:::jwaffleOptions$new(groups = NULL)
    a <- ClinicoPath:::jwaffleClass$new(options = o,
                                        data = data.frame(g = factor(c("A", "B"))))
    expect_identical(a$.__enclos_env__$private$.plot(NULL), FALSE)
})

test_that("sample-size diagnostics count weighted cases, not rows", {
    # The roxygen example verbatim: 1,000 patients held in 3 pre-aggregated rows.
    # The row-count version told the clinician "Small Sample: Total n=3. Consider
    # collecting more data" while the caption on the same figure said total n=1000.
    dat <- data.frame(RiskCategory = factor(c("Low", "Medium", "High")),
                      PatientCount = c(420, 310, 270))
    a <- wfr(dat, groups = "RiskCategory", counts = "PatientCount")
    a$.__enclos_env__$private$.run()

    expect_false(grepl("Small Sample", a$results$notices$content, fixed = TRUE))
    expect_false(grepl("Rare Categories", a$results$notices$content, fixed = TRUE))
})

test_that("categories holding hundreds of weighted cases are not called rare", {
    # 40 pre-aggregated rows over ~50,000 patients; the smallest category still holds
    # 500+. The row-count version named all 40 as "categories with <5 cases".
    set.seed(48)
    dat <- data.frame(Subtype = factor(paste0("S", 1:40)), N = sample(500:2000, 40))
    a <- wfr(dat, groups = "Subtype", counts = "N")
    a$.__enclos_env__$private$.run()

    expect_false(grepl("fewer than 5", a$results$notices$content, fixed = TRUE))
    # the legitimate many-categories notice must still fire
    expect_true(grepl("Many Categories", a$results$notices$content, fixed = TRUE))
})

test_that("a genuinely small weighted study still gets the small-sample warning", {
    # Guard against 'fixed' by disabling: 21 real patients must still warn.
    dat <- data.frame(g = factor(c("A", "B", "C")), w = c(10, 7, 4))
    a <- wfr(dat, counts = "w")
    a$.__enclos_env__$private$.run()

    notice <- a$results$notices$content
    expect_true(grepl("Small Sample", notice, fixed = TRUE))
    expect_true(grepl("n=21 weighted cases", notice, fixed = TRUE))
})

test_that("unweighted sample-size thresholds are unchanged", {
    a22 <- wfr(data.frame(g = factor(c(rep("A", 10), rep("B", 10), rep("C", 2)))))
    a22$.__enclos_env__$private$.run()
    expect_true(grepl("Small Sample", a22$results$notices$content, fixed = TRUE))

    a32 <- wfr(data.frame(g = factor(c(rep("A", 15), rep("B", 15), rep("C", 2)))))
    a32$.__enclos_env__$private$.run()
    expect_true(grepl("fewer than 5 cases", a32$results$notices$content, fixed = TRUE))
})

test_that("a tie for the largest category is named rather than silently broken", {
    tie3 <- wfr(data.frame(g = factor(rep(c("A", "B", "C"), each = 40))),
                showSummaries = TRUE)
    tie3$.__enclos_env__$private$.run()
    txt3 <- gsub("<[^>]*>", "", tie3$results$analysisSummary$content)
    expect_true(grepl("A, B and C are tied for the largest proportion", txt3, fixed = TRUE))

    tie2 <- wfr(data.frame(g = factor(c(rep("A", 40), rep("B", 40), rep("C", 10)))),
                showSummaries = TRUE)
    tie2$.__enclos_env__$private$.run()
    txt2 <- gsub("<[^>]*>", "", tie2$results$analysisSummary$content)
    expect_true(grepl("A and B are tied", txt2, fixed = TRUE))

    solo <- wfr(data.frame(g = factor(c(rep("A", 50), rep("B", 40), rep("C", 10)))),
                showSummaries = TRUE)
    solo$.__enclos_env__$private$.run()
    txt1 <- gsub("<[^>]*>", "", solo$results$analysisSummary$content)
    expect_true(grepl("A represents the largest proportion", txt1, fixed = TRUE))
})

test_that("the aggregate is computed once per cycle, not once per consumer", {
    set.seed(49)
    dat <- data.frame(g = factor(sample(c("A", "B", "C"), 200, TRUE)))
    a <- wfr(dat, showSummaries = TRUE)
    a$.__enclos_env__$private$.run()
    first <- a$.__enclos_env__$private$.aggregate_cache
    expect_false(is.null(first))

    invisible(wfr_build(a))                      # .plot() must reuse it
    expect_identical(a$.__enclos_env__$private$.aggregate_cache, first)
})
