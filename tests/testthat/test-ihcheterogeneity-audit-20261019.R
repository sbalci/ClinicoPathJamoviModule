# ═══════════════════════════════════════════════════════════
# ihcheterogeneity: module audit 2026-10-19 (MODULE_AUDIT_REPORT_20261019-1535)
# ═══════════════════════════════════════════════════════════
#
# M1: in the reference design the proportional-bias slope was regressed on the
# reference value, so the reference's own error made an unbiased region "change with
# the level" (20 of 20 simulated studies at n = 1500). It is now regressed on the mean
# of the two measurements (Bland & Altman, Lancet 1995;346:1085), as the inter-regional
# design already was. L2: the recommendations box could not be unticked. L3: advanced
# option panels open by default. L4: plot state saved on hidden plots, and the value
# matrix saved three times.

txt <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))
ihc <- function(d, ...) ClinicoPath::ihcheterogeneity(showInterpretation = TRUE, data = d, ...)

# Independent oracle: OLS of d on the pair mean, HC3 slope SE by the sandwich
# (X'X)^-1 X' diag(e^2 / (1 - h)^2) X (X'X)^-1 (MacKinnon & White 1985), from lm()
# residuals and hat values; no module helper and no sandwich package.
hc3_slope <- function(x, y) {
    d <- x - y; L <- (x + y) / 2
    fit <- lm(d ~ L)
    X <- model.matrix(fit); e <- resid(fit); h <- hatvalues(fit)
    bread <- solve(crossprod(X))
    V <- bread %*% crossprod(X * (e / (1 - h))) %*% bread
    b <- unname(coef(fit)[2]); se <- sqrt(V[2, 2])
    list(slope = b, p = 2 * pt(-abs(b / se), length(d) - 2))
}

noisy_pair <- function(n, seed, scale = 1) {
    set.seed(seed); t <- runif(n, 5, 90)
    data.frame(ref = t + rnorm(n, 0, 5), b1 = scale * t + rnorm(n, 0, 5))
}

test_that("an unbiased region against an equally noisy reference is not called level-dependent (M1)", {
    d <- noisy_pair(1500, 1)
    res <- ihc(d, wholesection = "ref", biopsy1 = "b1", bias_margin = 3, showReportSentences = TRUE)
    b <- res$samplingbiastable$asDF
    o <- hc3_slope(d$b1, d$ref)
    expect_equal(b$slope[1], o$slope, tolerance = 1e-10)
    expect_equal(b$slope_p[1], o$p, tolerance = 1e-8)
    # on the reference value this slope was about -0.03 at p ~ 1e-7
    expect_gt(b$slope_p[1], 0.05)
    expect_false(grepl("Changes with the level", b$clinical_impact[1], fixed = TRUE))
    expect_match(b$clinical_impact[1], "ruled out", fixed = TRUE)
    for (panel in list(res$interpretation$content, res$report_sentences$content))
        expect_false(grepl("changes with the level, an estimated", txt(panel), fixed = TRUE))
})

test_that("the mean of all regions is also regressed on the pair mean (M1)", {
    set.seed(5); n <- 400; t <- runif(n, 5, 90)
    d <- data.frame(ref = t + rnorm(n, 0, 5), b1 = t + rnorm(n, 0, 5), b2 = t + rnorm(n, 0, 5))
    b <- ihc(d, wholesection = "ref", biopsy1 = "b1", biopsy2 = "b2")$samplingbiastable$asDF
    expect_equal(b$slope[2], hc3_slope(d$b2, d$ref)$slope, tolerance = 1e-10)
    expect_equal(b$slope[3], hc3_slope((d$b1 + d$b2) / 2, d$ref)$slope, tolerance = 1e-10)
})

test_that("a true proportional bias is still detected (M1)", {
    d <- noisy_pair(300, 2, scale = 1.1)
    b <- ihc(d, wholesection = "ref", biopsy1 = "b1", bias_margin = 3)$samplingbiastable$asDF
    o <- hc3_slope(d$b1, d$ref)
    expect_equal(b$slope[1], o$slope, tolerance = 1e-10)
    expect_gt(b$slope[1], 0.05)
    expect_lt(b$slope_p[1], 0.001)
    # and a compressing region with no average offset is named at both ends
    set.seed(3); t <- runif(300, 40, 90)
    d2 <- data.frame(ref = t + rnorm(300, 0, 1), b1 = mean(t) + 0.8 * (t - mean(t)) + rnorm(300, 0, 1))
    imp <- ihc(d2, wholesection = "ref", biopsy1 = "b1")$samplingbiastable$asDF$clinical_impact[1]
    expect_match(imp, "Changes with the level (the mean of the two measurements)", fixed = TRUE)
})

test_that("every text names the pair mean as the level (M1)", {
    d <- noisy_pair(100, 4)
    res <- ihc(d, wholesection = "ref", biopsy1 = "b1", showReportSentences = TRUE,
               showAssumptions = TRUE, showGlossary = TRUE)
    notes <- paste(vapply(res$samplingbiastable$notes, function(n) n$note, ""), collapse = " ")
    expect_match(notes, "Slope of the difference on the mean of the two measurements", fixed = TRUE)
    # the displayed slope is not the one on the reference value (the reference value
    # is now named as one of the three slopes that must agree: release review IH-1)
    expect_false(grepl("Slope of the difference on the reference value", notes, fixed = TRUE))
    # release review IH-1 (2026-10-19): with a reference the slope must also agree on
    # the reference and the region value; the pair-mean slope is the one shown
    expect_match(txt(res$report_sentences$content), "regressing it on the reference value, on the region value and on the mean of the two", fixed = TRUE)
    expect_match(txt(res$glossary$content), "shows the slope against the mean", fixed = TRUE)
})

uyaml <- function() {
    path <- testthat::test_path("..", "..", "jamovi", "ihcheterogeneity.u.yaml")
    skip_if_not(file.exists(path), "u.yaml not available")
    yaml::read_yaml(path)$children
}

test_that("the recommendations box can be unticked whatever Clinical interpretation is (L2)", {
    boxes <- uyaml()
    controls <- unlist(lapply(boxes, function(b) b$children), recursive = FALSE)
    rec <- Filter(function(c) identical(c$name, "generate_recommendations"), controls)
    expect_length(rec, 1)
    expect_null(rec[[1]]$enable)
    # the INFO still tells a user who ticked it why nothing appears
    res <- ClinicoPath::ihcheterogeneity(data = noisy_pair(30, 1), wholesection = "ref", biopsy1 = "b1",
                                         showInterpretation = FALSE, generate_recommendations = TRUE)
    expect_match(txt(res$notices$content), "Recommendations not shown", fixed = TRUE)
})

test_that("advanced option panels start collapsed; Analysis Configuration stays open (L3)", {
    boxes <- Filter(function(b) identical(b$type, "CollapseBox"), uyaml())
    collapsed <- vapply(boxes, function(b) isTRUE(b$collapsed), logical(1))
    names(collapsed) <- vapply(boxes, function(b) b$label, "")
    expect_false(collapsed[["Analysis Configuration"]])
    expect_true(all(collapsed[c("Spatial Compartments", "Clinical Thresholds", "Additional Analyses", "Output Options")]))
})

run_cls <- function(d, ...) {
    o <- ClinicoPath:::ihcheterogeneityOptions$new(show_variability_plots = TRUE, ...)
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d); an$run(); an
}

test_that("hidden plots get no state and each plot stores only its own small frame (L4)", {
    set.seed(7); n <- 40; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n), b2 = w + rnorm(n), comp = factor(rep(c("A", "B"), each = 20)))
    an <- run_cls(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp")
    r <- an$results
    expect_named(r$biopsyplot$state, c("whole_section", "biopsy_data", "ref_label"))
    vs <- r$variabilityplot$state
    expect_null(vs$biopsy_data)
    expect_equal(nrow(vs$cases), n)
    expect_equal(vs$cases$Mean, rowMeans(as.matrix(d[, c("whole", "b1", "b2")])), tolerance = 1e-12)
    expect_equal(vs$cases$CV_Percent,
                 apply(d[, c("whole", "b1", "b2")], 1, function(v) sd(v) / mean(v) * 100), tolerance = 1e-10)
    ss <- r$spatialplot$state
    expect_equal(ss$Region, c("A", "B"))
    expect_equal(ss$CV, r$spatialanalysistable$asDF$cv_percent, tolerance = 1e-10)
    grDevices::png(tempfile(fileext = ".png")); on.exit(grDevices::dev.off(), add = TRUE)
    priv <- an$.__enclos_env__$private
    expect_true(priv$.variabilityplot(r$variabilityplot, ggtheme = ggplot2::theme_grey(), theme = list()))
    expect_true(suppressMessages(priv$.spatialplot(r$spatialplot, ggtheme = ggplot2::theme_grey(), theme = list())))

    # one compartment: the spatial plot is hidden and gets no state
    d$comp <- factor("A")
    one <- run_cls(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp")$results
    expect_false(one$spatialplot$visible)
    expect_null(one$spatialplot$state)
    # no computable CV: the variability plot is hidden and gets no state
    z <- data.frame(whole = rep(0, 10), b1 = rep(0, 10), b2 = rep(0, 10))
    zero <- run_cls(z, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$results
    expect_false(zero$variabilityplot$visible)
    expect_null(zero$variabilityplot$state)
})
