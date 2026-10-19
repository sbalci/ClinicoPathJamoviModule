# Regression tests for the waterfall findings of the 2026-10-18 OncoPath release-profile review
# (R68-R90), fixed by `/fix-function waterfall --apply` on 2026-10-19.
# Record: quality-reports/fix-waterfall-2026-10-19/README.md

wr <- function(d, ...) suppressWarnings(ClinicoPath::waterfall(data = d, ...))
wn <- function(res) paste(res$notices$content, collapse = " ")
wcounts <- function(res) { s <- res$summaryTable$asDF; stats::setNames(s$n, s$category) }
wnotes <- function(tbl) paste(vapply(tbl$notes, function(n) n$note, ""), collapse = " ")
wplot <- function(res, item, fun) {
    img <- res[[item]]
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    img$analysis$.__enclos_env__$private[[fun]](img, ggtheme = ggplot2::theme_grey(), theme = list())
    ggplot2::last_plot()
}
wyaml <- function(kind) yaml::read_yaml(test_path("..", "..", "jamovi", paste0("waterfall.", kind, ".yaml")))

# P4's -60% scan has no time; P1's follow-ups are measured but untimed.
# P1 has TWO untimed rows, so the old duplicated() test would call them a repeated visit.
r80 <- data.frame(pid = c("P1", "P1", "P1", "P2", "P2", "P3", "P3", "P4", "P4", "P4"),
                  t = c(0, NA, NA, 0, 2, 0, 2, 0, 2, NA),
                  chg = c(0, -50, -45, 0, -40, 0, 5, 0, 5, -60))

test_that("R80: a measured assessment with no time is disclosed, not dropped in silence", {
    res <- wr(r80, patientID = "pid", responseVar = "chg", timeVar = "t")
    nt <- wn(res)
    expect_match(nt, "ASSESSMENTS WITHOUT A TIME", fixed = TRUE)
    expect_match(nt, "3 assessment(s) for 2 patient(s) have a response value but no time", fixed = TRUE)
    # P1 is not blamed on missing measurements, and NA times are not "repeated visits"
    expect_match(nt, "some of their measured rows have no time", fixed = TRUE)
    expect_false(grepl("REPEATED VISIT TIMES", nt, fixed = TRUE))
    expect_equal(unname(wcounts(res)[c("PR", "SD")]), c(1, 2))
})

test_that("R80: a missing time cannot hide a missing raw baseline", {
    d <- data.frame(pid = rep(c("A", "B", "C"), each = 3), t = c(NA, 2, 4, 0, 2, 4, 0, 2, 4),
                    size = c(20, 15, 10, 30, 20, 15, 25, 30, 35))
    expect_error(ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "size",
                                        timeVar = "t", inputType = "raw"),
                 "lack baseline", fixed = TRUE)
})

test_that("R74: rows at one visit give the same result in any row order", {
    d <- data.frame(pid = c("P1", "P1", "P1", "P1", "P2", "P2", "P3", "P3", "P4", "P4"),
                    t = c(0, 2, 2, 4, 0, 2, 0, 2, 0, 2),
                    chg = c(0, -10, 15, -40, 0, -50, 0, 5, 0, 30))
    swap <- d[c(1, 3, 2, 4:10), ]
    a <- wr(d, patientID = "pid", responseVar = "chg", timeVar = "t")
    b <- wr(swap, patientID = "pid", responseVar = "chg", timeVar = "t")
    expect_identical(wcounts(a), wcounts(b))
    expect_equal(unname(wcounts(a)["PR"]), 2)          # P1 keeps its -40 (no same-visit "progression")
    expect_match(wn(a), "largest value first", fixed = TRUE)
    # a genuine rise over an EARLIER nadir is still progression in both orders
    e <- data.frame(pid = c("Q", "Q", "Q", "Q", "Q", "R", "R"), t = c(0, 1, 2, 2, 3, 0, 1),
                    chg = c(0, -50, -10, 15, -60, 0, -35))
    ea <- wr(e, patientID = "pid", responseVar = "chg", timeVar = "t")
    eb <- wr(e[c(1, 2, 4, 3, 5:7), ], patientID = "pid", responseVar = "chg", timeVar = "t")
    expect_identical(wcounts(ea), wcounts(eb))
    expect_equal(unname(wcounts(ea)["PR"]), 2)         # Q: -50 before the progression at t = 2
})

test_that("R76: numeric IDs past the 15th digit are separate patients in both notices", {
    d1 <- data.frame(id = 1e15 + 0:3, chg = c(-40, -10, 25, -35))
    expect_false(grepl("SEVERAL ROWS PER PATIENT", wn(wr(d1, patientID = "id", responseVar = "chg")), fixed = TRUE))
    d2 <- data.frame(id = rep(1e15 + 0:1, each = 2), time = rep(c(0, 2), 2), chg = c(0, NA, 0, -40))
    expect_match(wn(wr(d2, patientID = "id", responseVar = "chg", timeVar = "time")),
                 "NOT RESPONSE-EVALUABLE", fixed = TRUE)
})

# raw sizes: P1 nadir 10 mm then 13 mm (+30% but +3 mm), later 0 mm; P3 20 -> 24 mm (+20%, +4 mm)
r68 <- data.frame(pid = rep(c("P1", "P2", "P3", "P4"), c(4, 2, 2, 2)),
                  t = c(0, 1, 2, 3, 0, 1, 0, 1, 0, 1),
                  size = c(20, 10, 13, 0, 40, 20, 20, 24, 50, 45))

test_that("R68: with a stated unit, PD also needs the 5 mm absolute increase", {
    rel <- wr(r68, patientID = "pid", responseVar = "size", timeVar = "t", inputType = "raw")
    abs <- wr(r68, patientID = "pid", responseVar = "size", timeVar = "t", inputType = "raw",
              measurementUnit = "mm")
    expect_equal(unname(wcounts(rel)[c("CR", "PR", "SD", "PD")]), c(0, 2, 1, 1))
    expect_equal(unname(wcounts(abs)[c("CR", "PR", "SD", "PD")]), c(1, 1, 2, 0))
    # cm: 2.0 -> 2.4 cm is +4 mm, not PD either
    cm <- r68; cm$size <- cm$size / 10
    expect_identical(wcounts(wr(cm, patientID = "pid", responseVar = "size", timeVar = "t",
                                inputType = "raw", measurementUnit = "cm")), wcounts(abs))
    expect_match(wn(rel), "progression is called more often than RECIST v1.1 would", fixed = TRUE)
    expect_match(wn(abs), "Progression also requires the RECIST v1.1 absolute increase of at least 5 mm", fixed = TRUE)
    expect_false(grepl("may still be under-detected", wn(rel), fixed = TRUE))
    # the rule reaches duration of response too: P1's +3 mm rise is not a DoR event
    dor <- function(res) { m <- res$clinicalMetrics$asDF; m$value[grepl("Duration of Response", m$metric)][1] }
    expect_match(dor(rel), "1 of 2 responders progressed", fixed = TRUE)
    expect_match(dor(abs), "0 of 2 responders progressed", fixed = TRUE)
    # and the SD definition says so
    expect_match(paste(abs$summaryTable$getCell(rowNo = 3, "category")$footnotes, collapse = " "),
                 "+20% or more with an increase under 5 mm", fixed = TRUE)
})

test_that("R69/R75: the Methods text states the rule that was applied", {
    d <- data.frame(pid = rep(c("C", "D", "E"), each = 4), t = rep(0:3, 3),
                    size = c(100, 130, 50, 40, 50, 30, 25, 25, 40, 38, 36, 35))
    res <- wr(d, patientID = "pid", responseVar = "size", timeVar = "t", inputType = "raw",
              generateCopyReadyReport = TRUE)
    cr <- gsub("<[^>]+>", " ", res$copyReadyReport$content)
    expect_match(cr, "up to and including the first progression", fixed = TRUE)
    expect_match(wn(res), "later assessments are not used", fixed = TRUE)
    # supplied categories are named, and an all-supplied cohort is not called unconfirmed
    p <- data.frame(pid = paste0("P", 1:4), chg = c(-50, -40, 5, -45), cat = c("PR", "PR", "SD", "SD"))
    rp <- wr(p, patientID = "pid", responseVar = "chg", responseCategoryVar = "cat",
             generateCopyReadyReport = TRUE)
    crp <- gsub("<[^>]+>", " ", rp$copyReadyReport$content)
    expect_match(crp, "For 4 patient(s) the category was taken from a supplied response-category variable", fixed = TRUE)
    expect_false(grepl("unconfirmed objective response rate", crp, fixed = TRUE))
    expect_match(wn(rp), "Categories for 4 patient(s) were taken from the response-category variable", fixed = TRUE)
})

test_that("R70: the denominator notes exempt the all-patients ORR row", {
    d <- data.frame(pid = rep(paste0("P", 1:6), each = 2), t = rep(c(0, 2), 6),
                    chg = c(0, -50, 0, -40, 0, 5, 0, 30, 0, NA, 0, NA))
    res <- wr(d, patientID = "pid", responseVar = "chg", timeVar = "t")
    expect_match(wnotes(res$summaryTable), "except the all-patients ORR row", fixed = TRUE)
    expect_false(grepl("excluded from every denominator", wnotes(res$summaryTable), fixed = TRUE))
})

test_that("R72/R71/R73: plots draw no empty slot, label assessments, and the spider has no median", {
    d <- data.frame(pid = rep(paste0("P", 1:5), each = 3), t = rep(c(0, 1, 2), 5),
                    chg = c(0, -80, -60, 0, -40, -45, 0, 25, -35, 0, 10, 30, 0, NA, NA))
    res <- wr(d, patientID = "pid", responseVar = "chg", timeVar = "t", showSpiderPlot = TRUE,
              showMedian = TRUE)
    p <- wplot(res, "waterfallplot", ".waterfallplot")
    b <- ggplot2::ggplot_build(p)
    expect_equal(length(unique(b$data[[1]]$x)), 4)                     # P5 (not evaluable) has no slot
    expect_match(p$labels$caption, "1 not-evaluable patient(s) are not shown", fixed = TRUE)
    s <- wplot(res, "spiderplot", ".spiderplot")
    fill <- s$scales$get_scales("fill")
    expect_identical(fill$name, "Assessment change from baseline")
    expect_false(any(vapply(s$layers, function(l) identical(l$aes_params$linetype, "dotted"), TRUE)))
})

test_that("R77: a capped scan that is not the best response is not called a complete response", {
    d <- data.frame(pid = rep(c("A", "B", "C"), each = 3), t = rep(0:2, 3),
                    chg = c(0, 30, -150, 0, -40, -50, 0, 5, 10))
    res <- wr(d, patientID = "pid", responseVar = "chg", timeVar = "t", generateCopyReadyReport = TRUE)
    expect_false(grepl("counted as a complete response", res$copyReadyReport$content, fixed = TRUE))
    expect_match(wn(res), "only when a capped value is their best response", fixed = TRUE)
})

test_that("R82: all-missing groups are explained on both tables and in a notice", {
    d <- data.frame(pid = paste0("P", 1:6), chg = c(-50, -40, 5, 30, -10, 25), g = NA_character_)
    res <- wr(d, patientID = "pid", responseVar = "chg", groupVar = "g")
    expect_match(wnotes(res$groupComparisonTable), "no group comparison is possible", fixed = TRUE)
    expect_match(wnotes(res$groupComparisonTest), "no group comparison is possible", fixed = TRUE)
    expect_match(wn(res), "NO GROUP COMPARISON", fixed = TRUE)
})

test_that("R83: a rejected run in guided mode shows STOPPED, not READY", {
    d <- data.frame(pid = paste0("P", 1:4), chg = c(-50, Inf, 5, 30))
    a <- ClinicoPath:::waterfallClass$new(
        options = ClinicoPath:::waterfallOptions$new(patientID = "pid", responseVar = "chg", enableGuidedMode = TRUE),
        data = d)
    a$init(noThrow = TRUE); a$run(noThrow = TRUE)
    g <- paste(a$results$guidedAnalysis$content, collapse = " ")
    expect_match(g, "STOPPED", fixed = TRUE)
    expect_false(grepl("[READY]", g, fixed = TRUE))
})

test_that("R84/R87: a wide DCR interval is warned about whatever the CI table option says", {
    d <- data.frame(pid = paste0("P", 1:24), chg = c(rep(-50, 2), rep(5, 10), rep(40, 12)))
    for (ci in c(TRUE, FALSE)) {
        nt <- wn(wr(d, patientID = "pid", responseVar = "chg", showConfidenceIntervals = ci))
        expect_match(nt, "The DCR 95% CI spans", fixed = TRUE)
        expect_false(grepl("The ORR 95% CI spans", nt, fixed = TRUE))   # ORR 2/24 is 26 points wide
    }
})

test_that("R89: each extreme value and each narrative block appears once", {
    d <- data.frame(pid = paste0("P", 1:10), chg = c(-150, 250, -30, 20, -10, 5, -45, 60, -70, 15))
    nt <- wn(wr(d, patientID = "pid", responseVar = "chg"))
    expect_equal(lengths(regmatches(nt, gregexpr("-150", nt, fixed = TRUE))), 1)
    res <- wr(d, patientID = "pid", responseVar = "chg", showExplanations = TRUE)
    expect_match(paste(res$aboutAnalysis$content, collapse = ""), "What This Analysis Does", fixed = TRUE)
    # the retired duplicate panels are gone from the results (audit 2026-10-19)
    item_names <- vapply(res$items, function(i) i$name, "")
    expect_false(any(c("explanations", "naturalLanguageSummary") %in% item_names))
})

test_that("R85/R86/R87/R88/R90: interface definitions", {
    u <- wyaml("u"); a <- wyaml("a"); r <- wyaml("r")
    # R85: the annotation target sits under a VariableSupplier
    path_to <- function(x, target, trail = character()) {
        if (is.list(x)) {
            if (identical(x$name, target)) return(trail)
            for (ch in x$children %||% (if (is.null(names(x))) x else list())) {
                found <- path_to(ch, target, c(trail, x$type %||% ""))
                if (!is.null(found)) return(found)
            }
        }
        NULL
    }
    expect_true("VariableSupplier" %in% path_to(u, "annotationVars"))
    # R86: notices right after the guidance panels
    items <- vapply(r$items, `[[`, "", "name")
    expect_equal(which(items == "notices"), which(items == "todo2") + 1L)
    # R87: titles say what the options control; the time unit is not spider-only
    opt <- function(n) Filter(function(o) identical(o$name, n), a$options)[[1]]
    expect_identical(opt("timeUnitLabel")$title, "Time unit")
    expect_identical(opt("showConfidenceIntervals")$title, "ORR/DCR confidence-interval table")
    # R90: one title in all three files
    expect_identical(r$title, a$title)
    # R88: the group handlers leave the colour scheme alone
    js <- readLines(test_path("..", "..", "jamovi", "js", "waterfall.events.js"))
    expect_false(any(grepl("ColorScheme.setValue|colorScheme.setValue", js)))
})

test_that("R88: both plots colour a group the same way", {
    d <- data.frame(pid = rep(c("P1", "P2", "P3", "P4"), each = 2), t = rep(c(0, 1), 4),
                    chg = c(0, -50, 0, 20, 0, -40, 0, 30), arm = rep(c("B", "A", "B", "A"), each = 2))
    res <- wr(d, patientID = "pid", responseVar = "chg", timeVar = "t", groupVar = "arm",
              colorBy = "group", spiderColorBy = "group", showSpiderPlot = TRUE,
              colorScheme = "colorblind", spiderColorScheme = "colorblind")
    # the two plots order their rows differently (by response, by patient); the
    # same group must still get the same colour
    wb <- ggplot2::ggplot_build(wplot(res, "waterfallplot", ".waterfallplot"))
    sb <- ggplot2::ggplot_build(wplot(res, "spiderplot", ".spiderplot"))
    wmap <- wb$plot$scales$get_scales("fill")$map(c("A", "B"))
    smap <- sb$plot$scales$get_scales("colour")$map(c("A", "B"))
    expect_identical(unname(wmap), unname(smap))
})

# --- adversarial review round (2026-10-19) ----------------------------------------------

test_that("review: an override to Unknown is not a supplied category of anyone in the rates", {
    d <- data.frame(pid = paste0("P", 1:5), chg = c(-50, 10, -40, 5, 30),
                    cat = c(NA, NA, "Unknown", "Unknown", "Unknown"))
    res <- wr(d, patientID = "pid", responseVar = "chg", responseCategoryVar = "cat",
              generateCopyReadyReport = TRUE)
    cr <- gsub("<[^>]+>", " ", res$copyReadyReport$content)
    expect_match(cr, "unconfirmed objective response rate", fixed = TRUE)
    expect_false(grepl("the category was taken from a supplied", cr, fixed = TRUE))
})

test_that("review: a supplied category with no measured change is not called not-evaluable", {
    d <- data.frame(pid = c("P1", "P1", "P2", "P2", "P3", "P3", "P4"), t = c(0, 2, 0, 2, 0, 2, 0),
                    size = c(20, 10, 30, 28, 25, 33, 40), cat = c(NA, NA, NA, NA, NA, NA, "PD"))
    res <- wr(d, patientID = "pid", responseVar = "size", timeVar = "t", inputType = "raw",
              responseCategoryVar = "cat")
    cap <- wplot(res, "waterfallplot", ".waterfallplot")$labels$caption
    expect_match(cap, "1 patient(s) with a supplied category but no measured change are not shown", fixed = TRUE)
    expect_false(grepl("not-evaluable", cap, fixed = TRUE))
})

test_that("review: without a time variable the rule says a baseline row takes part", {
    d <- data.frame(pid = rep(paste0("P", 1:4), each = 2), chg = c(0, 30, 0, -40, 0, 25, 0, -10))
    res <- wr(d, patientID = "pid", responseVar = "chg", generateCopyReadyReport = TRUE)
    expect_match(wn(res), "a 0% baseline row takes part in it", fixed = TRUE)
    expect_match(res$copyReadyReport$content, "a 0% baseline row included", fixed = TRUE)
})

test_that("review: a group with no evaluable patient keeps the colours aligned", {
    d <- data.frame(pid = rep(paste0("P", 1:6), each = 2), t = rep(c(0, 1), 6),
                    chg = c(0, -50, 0, 20, 0, NA, 0, NA, 0, -40, 0, 30),
                    arm = rep(c("A", "A", "B", "B", "C", "C"), each = 2))
    res <- wr(d, patientID = "pid", responseVar = "chg", timeVar = "t", groupVar = "arm",
              colorBy = "group", spiderColorBy = "group", showSpiderPlot = TRUE,
              colorScheme = "colorblind", spiderColorScheme = "colorblind")
    wb <- ggplot2::ggplot_build(wplot(res, "waterfallplot", ".waterfallplot"))
    sb <- ggplot2::ggplot_build(wplot(res, "spiderplot", ".spiderplot"))
    expect_identical(unname(wb$plot$scales$get_scales("fill")$map(c("A", "C"))),
                     unname(sb$plot$scales$get_scales("colour")$map(c("A", "C"))))
})

test_that("review: notices are ordered most severe first, and a guided stop names its reason", {
    d <- data.frame(pid = paste0("P", 1:12), chg = c(-0.35, -0.5, 0.1, -0.2, 0.05, -0.4, 0.3, -0.6, 0.15, -0.1, 0.2, -0.45))
    nt <- wn(wr(d, patientID = "pid", responseVar = "chg"))
    expect_lt(regexpr("PROPORTIONS", nt, fixed = TRUE), regexpr("EXPLORATORY RESPONSE CATEGORIES", nt, fixed = TRUE))
    a <- ClinicoPath:::waterfallClass$new(
        options = ClinicoPath:::waterfallOptions$new(patientID = "pid", responseVar = "chg", enableGuidedMode = TRUE),
        data = data.frame(pid = paste0("P", 1:4), chg = c(-50, Inf, 5, 30)))
    a$init(noThrow = TRUE); a$run(noThrow = TRUE)
    expect_match(paste(a$results$guidedAnalysis$content, collapse = " "), "Infinite response or time values", fixed = TRUE)
})
