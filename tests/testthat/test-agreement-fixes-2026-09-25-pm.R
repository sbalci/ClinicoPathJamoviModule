# /fix-function agreement --apply, second pass 2026-09-25: the defects the earlier passes
# deferred. Each test fails on the file before this pass.

test_that("an exact proportional relation (B = 2A) gets no slope p-value, and says why", {
    set.seed(1)
    x <- round(runif(20, 1, 10), 3)
    d <- x - 2 * x; m <- (x + 2 * x) / 2
    fit <- stats::lm(d ~ m)
    # oracle: the residuals are rounding error, so summary.lm's p is noise, not a test
    expect_lt(sqrt(sum(stats::residuals(fit)^2)), 1e-10)
    expect_lt(suppressWarnings(summary(fit))$coefficients[2, 4], 1e-100)

    a <- agreement_run(data.frame(A = x, B = 2 * x), vars = c("A", "B"),
                       blandAltmanPlot = TRUE, proportionalBias = TRUE)
    expect_true(is.na(a$results$blandAltmanStats$asDF$propBiasP))
    notes <- agreement_notes(a$results$blandAltmanStats)
    expect_match(notes[["prop_bias_failed"]], "lie exactly on a line", fixed = TRUE)
    expect_false("prop_bias_assumption" %in% names(notes))
})

test_that("a noisy proportional relation still gets its slope p-value", {
    set.seed(1)
    x <- runif(40, 1, 10)
    a <- agreement_run(data.frame(A = x, B = 2 * x + rnorm(40, 0, 0.5)), vars = c("A", "B"),
                       blandAltmanPlot = TRUE, proportionalBias = TRUE)
    p <- a$results$blandAltmanStats$asDF$propBiasP
    expect_true(is.finite(p))
    expect_false("prop_bias_failed" %in% names(agreement_notes(a$results$blandAltmanStats)))
})

test_that("the three heatmaps draw a colour key for their fixed scale", {
    set.seed(4); lv <- c("G1", "G2", "G3")
    tr <- sample(lv, 60, TRUE)
    fl <- function() factor(ifelse(runif(60) < .7, tr, sample(lv, 60, TRUE)), lv)
    d <- data.frame(R1 = fl(), R2 = fl(), R3 = fl(), R4 = fl())
    a <- agreement_run(d, vars = names(d), agreementHeatmap = TRUE,
                       raterClustering = TRUE, showClusterHeatmap = TRUE,
                       caseClustering = TRUE, showCaseClusterHeatmap = TRUE)
    p <- a$.__enclos_env__$private
    rec <- new.env()
    suppressMessages(trace("legend", where = asNamespace("graphics"), print = FALSE,
        tracer = bquote(assign("keys", c(get0("keys", envir = .(rec)), title), envir = .(rec)))))
    on.exit(suppressMessages(untrace("legend", where = asNamespace("graphics"))), add = TRUE)
    f <- tempfile(fileext = ".png"); grDevices::png(f)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(p$.agreementHeatmap(a$results$agreementHeatmapPlot, NULL, NULL))
    expect_true(p$.raterClusterHeatmap(a$results$raterClusterHeatmap, NULL, NULL))
    expect_true(p$.caseClusterHeatmap(a$results$caseClusterHeatmap, NULL, NULL))
    expect_identical(rec$keys, c("Cell colour: share of all rated cases",
                                 rep("Cell colour: similarity (1 - distance)", 2)))
})

test_that("the Lin's CCC fallback matches Lin's asymptotic estimate and interval", {
    skip_if_not_installed("DescTools")
    set.seed(8)
    x <- rnorm(40, 50, 10); y <- x + 4 + rnorm(40, 0, 5)       # an offset pair: u != 0
    ref <- DescTools::CCC(x, y, ci = "asymptotic", conf.level = 0.95)$rho.c
    testthat::local_mocked_bindings(CCC = function(...) stop("forced fallback"), .package = "DescTools")
    t <- agreement_run(data.frame(A = x, B = y), vars = c("A", "B"), linCCC = TRUE)$results$linCCCTable
    df <- t$asDF
    expect_equal(df$ccc, ref$est, tolerance = 1e-10)
    expect_equal(c(df$ci_lower, df$ci_upper), c(ref$lwr.ci, ref$upr.ci), tolerance = 1e-10)
    expect_match(agreement_notes(t)[["ci_method"]], "Wald", fixed = TRUE)
})

test_that("hierarchical ICC(2,k) uses the harmonic mean of ratings per case when ratings are missing", {
    skip_if_not_installed("lme4")
    withr::local_seed(21)
    n <- 120
    lab <- rep(paste0("Lab", 1:4), each = 30)
    base <- stats::rnorm(n, 50, 15) + c(Lab1 = -8, Lab2 = 0, Lab3 = 4, Lab4 = 10)[lab]
    d <- data.frame(r1 = base + stats::rnorm(n, 0, 5), r2 = base + 2 + stats::rnorm(n, 0, 5),
                    r3 = base - 1 + stats::rnorm(n, 0, 5), lab = factor(lab))
    d$r3[1:60] <- NA                       # half the cases have 2 ratings, half 3
    r <- agreement_run(d, vars = c("r1", "r2", "r3"), hierarchicalKappa = TRUE,
                       clusterVariable = "lab", iccHierarchical = TRUE)$results

    long <- data.frame(case_id = factor(rep(seq_len(n), 3)),
                       rater = factor(rep(c("r1", "r2", "r3"), each = n)),
                       cluster = factor(rep(d$lab, 3)), score = c(d$r1, d$r2, d$r3))
    long <- long[stats::complete.cases(long), ]
    m <- lme4::lmer(score ~ 1 + (1 | case_id) + (1 | rater) + (1 | cluster), data = long,
                    control = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
    vc <- as.data.frame(lme4::VarCorr(m))
    g <- function(x) vc$vcov[vc$grp == x]
    k <- 1 / mean(1 / c(rep(2, 60), rep(3, 60)))                  # 2.4, not 3
    icc2k <- g("case_id") / (g("case_id") + (g("rater") + g("Residual")) / k)

    it <- r$hierarchicalICCTable
    expect_equal(it$asDF$icc_value[2], icc2k, tolerance = 1e-4)
    expect_match(agreement_notes(it)[["info"]], "mean of 2.40 (the harmonic mean", fixed = TRUE)
})

test_that("ICC labels in the inter/intra and subgroup tables name the bands their CI spans", {
    skip_if_not_installed("irr")
    withr::local_seed(5)
    n <- 10
    t <- stats::rnorm(n, 50, 10)
    d <- data.frame(A_T1 = t + stats::rnorm(n, 0, 4), A_T2 = t + stats::rnorm(n, 0, 4),
                    B_T1 = t + stats::rnorm(n, 0, 4), B_T2 = t + stats::rnorm(n, 0, 4))
    oracle <- irr::icc(d[c("A_T1", "A_T2")], model = "twoway", type = "agreement", unit = "single")
    koo <- function(v) if (v < .5) "Poor" else if (v < .75) "Moderate" else if (v < .9) "Good" else "Excellent"
    expect_false(identical(koo(oracle$lbound), koo(oracle$ubound)))   # the fixture's CI crosses a band

    r <- agreement_run(d, vars = names(d), interIntraRater = TRUE, interIntraSeparator = "_")$results
    intra <- r$interIntraRaterIntraTable$asDF
    expect_identical(intra$interpretation[intra$rater == "A"],
                     sprintf("%s (CI spans %s to %s)", koo(oracle$value), koo(oracle$lbound), koo(oracle$ubound)))

    d2 <- data.frame(R1 = t + stats::rnorm(n, 0, 4), R2 = t + stats::rnorm(n, 0, 4))
    d2 <- rbind(d2, d2 + stats::rnorm(2 * n, 0, 1)); d2$site <- factor(rep(c("S1", "S2"), each = n))
    sg <- agreement_run(d2, vars = c("R1", "R2"), agreementBySubgroup = TRUE,
                        subgroupVariable = "site", subgroupMinCases = 5)$results$subgroupAgreementTable$asDF
    o1 <- irr::icc(d2[d2$site == "S1", 1:2], model = "twoway", type = "agreement", unit = "single")
    expect_equal(sg$agreement_stat[1], o1$value, tolerance = 1e-8)
    want <- if (identical(koo(o1$lbound), koo(o1$ubound))) koo(o1$value) else
        sprintf("%s (CI spans %s to %s)", koo(o1$value), koo(o1$lbound), koo(o1$ubound))
    expect_identical(sg$interpretation[1], want)
    expect_match(sg$interpretation[1], "CI spans", fixed = TRUE)
})

test_that("a rater column with no ratings is named in the grading section, not left as a blank row", {
    lv <- c("G1", "G2", "G3")
    set.seed(3)
    x <- sample(lv, 60, TRUE)
    f <- function() factor(ifelse(stats::runif(60) < 0.8, x, sample(lv, 60, TRUE)), lv, ordered = TRUE)
    d <- data.frame(P1 = f(), P2 = f(), P3 = f(), P4 = factor(rep(NA, 60), lv, ordered = TRUE))
    r <- agreement_run(d, vars = names(d), perPathologist = TRUE, gradingDesign = TRUE)$results
    pt <- r$pathologistTable$asDF
    expect_true(is.na(pt$cases_read[pt$pathologist == "P4"]))      # the blank row the note explains
    for (tbl in list(r$pathologistTable, r$designSummaryTable))
        expect_match(agreement_notes(tbl)[["no_ratings"]], "No ratings from P4", fixed = TRUE)

    r2 <- agreement_run(d[1:3], vars = c("P1", "P2", "P3"), perPathologist = TRUE)$results
    expect_false("no_ratings" %in% names(agreement_notes(r2$pathologistTable)))
})

test_that("the CCC fallback's location shift does not overflow on very large values", {
    skip_if_not_installed("DescTools")
    x <- (1:12) * 1e80; y <- (2:13) * 1e80
    ref <- DescTools::CCC(x / 1e80, y / 1e80, ci = "asymptotic")$rho.c$est   # scale-free oracle
    testthat::local_mocked_bindings(CCC = function(...) stop("forced fallback"), .package = "DescTools")
    t <- agreement_run(data.frame(A = x, B = y), vars = c("A", "B"), linCCC = TRUE)$results$linCCCTable
    expect_equal(t$asDF$ccc, ref, tolerance = 1e-10)
})
