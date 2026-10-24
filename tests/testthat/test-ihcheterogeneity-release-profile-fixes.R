# Regression tests for the ihcheterogeneity findings of the 2026-10-18 release-profile check
# (quality-reports/ONCOPATH_RELEASE_PROFILE_2026-10-18-night.md, R24-R43 and C2). Each test
# names its finding and fails on the backend as it stood before the 2026-10-19 fix. Numbers
# are checked against independent computations, never the module's own helpers.

txt <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))
notes_of <- function(tbl) paste(vapply(tbl$notes, function(n) n$note, ""), collapse = " | ")
ihc <- function(d, ...) ClinicoPath::ihcheterogeneity(data = d, ...)
verdict_of <- function(res) {
    t <- txt(res$interpretation$content)
    regmatches(t, regexpr(
        "(Agreement thresholds met, not confirmed|Agreement thresholds met|Moderate sampling|Inadequate sampling|Not adequate for substitution|Insufficient data):", t)) |>
        sub(pattern = ":$", replacement = "")  # labels are sentence case; the colon marks the verdict
}
# The CI of the percentage difference, by inverting the paired t-test of d - R z (root finding).
ratio_ci <- function(d, z, level) {
    n <- length(d); q <- qt((1 + level) / 2, n - 1)
    t_of <- function(R) { w <- d - R * z; mean(w) / (sd(w) / sqrt(n)) }
    R0 <- mean(d) / mean(z)
    c(uniroot(function(R) t_of(R) - q, c(R0 - 1, R0), tol = 1e-12)$root,
      uniroot(function(R) t_of(R) + q, c(R0, R0 + 1), tol = 1e-12)$root) * 100
}
one_offset <- function() {
    set.seed(4); n <- 60; t0 <- runif(n, 20, 80)
    data.frame(A = t0 + rnorm(n), B = t0 + rnorm(n), C = t0 + 4 + rnorm(n))
}


test_that("one offset region names only itself, not the regions that agree (R24)", {
    # Was: each region was compared with the mean of the others, so C's offset spread
    # onto A and B; all three were MATERIAL and A and B were to be calibrated.
    res <- ihc(one_offset(), biopsy1 = "A", biopsy2 = "B", biopsy3 = "C",
               showInterpretation = TRUE, generate_recommendations = TRUE, showSummary = TRUE)
    b <- res$samplingbiastable$asDF
    expect_equal(b$comparison, c("B vs A", "C vs A", "C vs B"))
    expect_match(b$clinical_impact[1], "ruled out", fixed = TRUE)
    expect_match(b$clinical_impact[2], "shown to exceed", fixed = TRUE)
    expect_match(b$clinical_impact[3], "shown to exceed", fixed = TRUE)
    notice <- txt(res$notices$content)
    expect_match(notice, "Region 'C' differs systematically from every other region", fixed = TRUE)
    expect_false(grepl("'A'", notice, fixed = TRUE))
    interp <- txt(res$interpretation$content)
    expect_match(interp, "Region 'C' disagrees with every other region", fixed = TRUE)
    expect_false(grepl("Calibrate region(s) 'A'", interp, fixed = TRUE))
    expect_match(txt(res$summary$content), "region 'C' differs systematically from the other regions", fixed = TRUE)
    # two regions: the pair is named, neither slot is the culprit
    two <- ihc(one_offset(), biopsy1 = "A", biopsy2 = "C")
    expect_match(txt(two$notices$content), "Regions differ systematically by more than the 5% margin ('C' vs 'A')", fixed = TRUE)
})


test_that("the variance table grades shares, not sampling variability (R25)", {
    set.seed(1); n <- 30; w <- runif(n, 2, 90)
    res <- ihc(data.frame(a = w + rnorm(n, 0, 6), b = w + rnorm(n, 0, 6)), biopsy1 = "a", biopsy2 = "b")
    v <- res$variancetable$asDF
    expect_match(v$contribution[2], "share of total variance", fixed = TRUE)
    expect_false(any(grepl("sampling variability", v$contribution, fixed = TRUE)))
    expect_match(notes_of(res$variancetable), "the share labels are descriptive cut-offs", fixed = TRUE)
})


test_that("the default output states the scope and points only at visible panels (R26/R40)", {
    set.seed(2); n <- 30; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w * 1.2 + rnorm(n))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1")
    expect_match(notes_of(res$reproducibilitytable), "not an assay or external validation", fixed = TRUE)
    expect_match(notes_of(res$reproducibilitytable), "Each row must be a different patient.", fixed = TRUE)
    notice <- txt(res$notices$content)
    expect_match(notice, "Material systematic difference", fixed = TRUE)
    expect_false(grepl("see the Clinical Assessment", notice, ignore.case = TRUE))
    expect_match(notice, "Tick Clinical interpretation under Output Options", fixed = TRUE)
    # with the panel on, the pointer leads to it
    expect_match(txt(ihc(d, wholesection = "whole", biopsy1 = "b1", showInterpretation = TRUE)$notices$content),
                 "See the Clinical Assessment.", fixed = TRUE)
})


test_that("with one comparison the sentences quote the interval the rule used (R27/R35)", {
    # Was: "95% CI 2.38 to 4.62" (raw units, reaching inside the margin) beside
    # "shown to exceed", and Methods describing a mean of all regions and Bonferroni.
    set.seed(27); n <- 40; w <- runif(n, 30, 70)
    d <- data.frame(whole = w, b1 = w + 0.09 * mean(w) + rnorm(n, 0, 3))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", showReportSentences = TRUE)
    rep <- txt(res$report_sentences$content)
    ci <- ratio_ci(d$b1 - d$whole, d$whole, 0.90)
    expect_gt(min(ci), 5)
    expect_match(rep, sprintf("90%% CI %.1f%% to %.1f%%); the difference is shown to exceed", ci[1], ci[2]), fixed = TRUE)
    expect_false(grepl("95% CI [-0-9.]+ to [-0-9.]+\\); the difference is shown to exceed", rep))
    expect_false(grepl("mean of all regions", rep, fixed = TRUE))
    expect_false(grepl("Bonferroni", rep, fixed = TRUE))
})


test_that("a bimodal marker raises no outlier warning (R28)", {
    set.seed(28); n <- 30; er <- c(runif(24, 75, 98), runif(6, 0, 3))
    d <- data.frame(whole = er, b1 = pmax(0, er + rnorm(n, 0, 2)), b2 = pmax(0, er + rnorm(n, 0, 2)))
    expect_false(grepl("outlier", txt(ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$notices$content),
                       ignore.case = TRUE))
})


test_that("the compartment tests state independence as an assumption (R29)", {
    set.seed(29); n <- 30; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n), b2 = w + rnorm(n), comp = rep(c("centre", "front"), 15))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp", compartmentTests = TRUE)
    n_txt <- notes_of(res$compartmentTests)
    expect_match(n_txt, "The tests assume that every row is a different patient", fixed = TRUE)
    expect_false(grepl("so the observations are independent", n_txt, fixed = TRUE))
})


test_that("identical differences in every case are never ruled out (R30)", {
    # Was: a zero-width CI, "within the 5% margin", AGREEMENT THRESHOLDS MET and ICC CI 1 to 1.
    w <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
    res <- ihc(data.frame(ref = w, r1 = w), wholesection = "ref", biopsy1 = "r1", showInterpretation = TRUE)
    b <- res$samplingbiastable$asDF
    expect_match(b$clinical_impact[1], "its precision cannot be estimated, so it is not ruled out", fixed = TRUE)
    expect_true(is.na(b$ci_lower[1]) && is.na(b$loa_lower[1]) && is.na(b$loa_lower_lcl[1]))
    expect_length(verdict_of(res), 1)
    expect_false(identical(verdict_of(res), "Agreement thresholds met"))
    icc <- res$reproducibilitytable$asDF
    expect_true(all(is.na(icc$ci_lower[grepl("^ICC", icc$metric)])))
    # an offset beyond the margin is material only with enough cases for the sign
    # test at the adjusted level: m = 3 needs 0.5^n <= 0.05 / 3, so n >= 6
    impact <- function(n) {
        w <- seq(20, 70, length.out = n)
        ihc(data.frame(w = w, a = w - 5, b = w - 5), wholesection = "w", biopsy1 = "a", biopsy2 = "b")$samplingbiastable$asDF$clinical_impact[1]
    }
    expect_match(impact(5), "not ruled out", fixed = TRUE)
    expect_match(impact(6), "beyond the 5% margin", fixed = TRUE)
    # ... and only when every case lies beyond the margin in the same direction: a
    # 5-point offset is within 5% of a case at 200
    w <- c(20, 25, 30, 35, 40, 45, 50, 200)
    imp <- ihc(data.frame(w = w, a = w - 5, b = w - 5), wholesection = "w", biopsy1 = "a", biopsy2 = "b")$samplingbiastable$asDF$clinical_impact[1]
    expect_lt(-5 / mean(w) * 100, -5)
    expect_match(imp, "not ruled out", fixed = TRUE)
})


test_that("agreement between regions is claimed only when every pair agrees (R31)", {
    # Was: rows against the mean of the others all lay within 5% while c1 and c2
    # differed by 6%, and the verdict was AGREEMENT THRESHOLDS MET.
    set.seed(3); n <- 60; t <- runif(n, 30, 80)
    d <- data.frame(c1 = t * 1.03 + rnorm(n, 0, 0.3), c2 = t * 0.97 + rnorm(n, 0, 0.3), c3 = t + rnorm(n, 0, 0.3))
    res <- ihc(d, biopsy1 = "c1", biopsy2 = "c2", biopsy3 = "c3", showInterpretation = TRUE, showReportSentences = TRUE)
    expect_equal(verdict_of(res), "Not adequate for substitution")
    expect_false(grepl("every comparison between regions was shown", txt(res$report_sentences$content), fixed = TRUE))
    expect_match(res$samplingbiastable$asDF$clinical_impact[1], "shown to exceed", fixed = TRUE)
})


test_that("CV tests between compartments use cases with every measurement (R32)", {
    # Was: CVs from 2 values (lost cores) against CVs from 5, which read as a
    # difference in heterogeneity when the true CV was the same.
    set.seed(32); n <- 60; mu <- runif(n, 30, 70)
    m <- sapply(1:4, function(i) mu * (1 + rnorm(n, 0, 0.15)))
    d <- data.frame(whole = mu * (1 + rnorm(n, 0, 0.15)), b1 = m[, 1], b2 = m[, 2], b3 = m[, 3], b4 = m[, 4],
                    comp = rep(c("Invasive", "Central"), c(15, 45)))
    d[1:15, c("b2", "b3", "b4")] <- NA                     # Invasive: reference + 1 region
    d[16:20, "b4"] <- NA                                   # a few incomplete Central cases too
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3", biopsy4 = "b4",
               spatial_id = "comp", compartmentTests = TRUE)
    expect_match(notes_of(res$compartmentTests), "The CV tests use only the cases with every measurement", fixed = TRUE)
    # Invasive has no complete case, so no CV test can compare it
    tests <- res$compartmentTests$asDF
    expect_false(any(grepl("per-case CV", tests$test_type, fixed = TRUE)))
    expect_match(notes_of(res$compartmentTests), "No CV test", fixed = TRUE)
    # with complete cases in both compartments the statistic is the complete-case one
    d2 <- d; d2[1:8, c("b2", "b3", "b4")] <- m[1:8, 2:4]
    res2 <- ihc(d2, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3", biopsy4 = "b4",
                spatial_id = "comp", compartmentTests = TRUE)
    full <- stats::complete.cases(d2[, 1:5])
    cv <- apply(as.matrix(d2[full, 1:5]), 1, function(v) sd(v) / mean(v) * 100)
    kw <- kruskal.test(cv, factor(d2$comp[full]))
    t2 <- res2$compartmentTests$asDF
    expect_equal(t2$statistic[t2$test_type == "Kruskal-Wallis test (per-case CV)"], unname(kw$statistic), tolerance = 1e-10)
})


test_that("the ratio CI allows for the uncertainty in the comparison mean (R33)", {
    # Was: the CI of the difference divided by the sample mean of the reference.
    set.seed(33); n <- 40; w <- runif(n, 5, 95)
    d <- data.frame(whole = w, b1 = w + 0.15 * mean(w) + rnorm(n, 0, 6))     # additive: no slope
    imp <- ihc(d, wholesection = "whole", biopsy1 = "b1", bias_margin = 20)$samplingbiastable$asDF$clinical_impact[1]
    ci <- ratio_ci(d$b1 - d$whole, d$whole, 0.90)
    expect_match(imp, sprintf("(90%% CI %.1f%% to %.1f%%)", ci[1], ci[2]), fixed = TRUE)
})


test_that("swapping the two regions mirrors the result (R34)", {
    # Was: relative to the first region's mean, +22% (material) one way and -18%
    # (ruled out) the other, on identical data.
    set.seed(5); x <- runif(60, 30, 70); d <- data.frame(x = x, y = x + 0.22 * mean(x) + rnorm(60, 0, 0.6))
    one <- ihc(d, biopsy1 = "x", biopsy2 = "y", bias_margin = 20, showInterpretation = TRUE)
    two <- ihc(d, biopsy1 = "y", biopsy2 = "x", bias_margin = 20, showInterpretation = TRUE)
    expect_length(verdict_of(one), 1)
    expect_equal(verdict_of(one), verdict_of(two))
    flip <- function(s) chartr("+-", "-+", s)
    i1 <- one$samplingbiastable$asDF$clinical_impact; i2 <- two$samplingbiastable$asDF$clinical_impact
    expect_equal(sub(" \\(.*", "", i1), flip(sub(" \\(.*", "", i2)))       # the estimate, mirrored
    expect_equal(sub(".*\\): ", "", i1), sub(".*\\): ", "", i2))            # the zone, identical
    pm <- (d$x + d$y) / 2
    ci <- ratio_ci(d$y - d$x, pm, 0.90)
    expect_match(one$samplingbiastable$asDF$clinical_impact, sprintf("90%% CI %.1f%% to %.1f%%", ci[1], ci[2]), fixed = TRUE)
})


test_that("the ICCs state the complete cases they rest on (R36)", {
    set.seed(36); n <- 30; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n, 0, 2), b2 = w + rnorm(n, 0, 2), b3 = NA_real_)
    d$b3[1:6] <- w[1:6] + rnorm(6, 0, 2)
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3", showReportSentences = TRUE)
    expect_match(notes_of(res$reproducibilitytable), "The ICCs use the 6 cases with every measurement; 24 case(s)", fixed = TRUE)
    expect_match(txt(res$notices$content), "The ICCs rest on only 6 cases with every measurement (of 30 analyzed)", fixed = TRUE)
    expect_match(txt(res$report_sentences$content), "The ICC was computed on the 6 cases with every measurement.", fixed = TRUE)
})


test_that("a rejected run blanks the previous narrative (R37)", {
    a <- ClinicoPath:::ihcheterogeneityClass$new(
        options = ClinicoPath:::ihcheterogeneityOptions$new(wholesection = "w", biopsy1 = "a", showInterpretation = TRUE,
                                                            showSummary = TRUE, showReportSentences = TRUE, showAssumptions = TRUE),
        data = data.frame(w = numeric(0), a = numeric(0)))
    panels <- c("interpretation", "report_sentences", "summary", "assumptions")
    for (nm in panels) a$results[[nm]]$setContent("<p>OLD VERDICT</p>")
    a$init(noThrow = TRUE); a$run(noThrow = TRUE)
    expect_true(a$errored)
    for (nm in panels)
        expect_false(grepl("OLD VERDICT", a$results[[nm]]$content, fixed = TRUE), info = nm)
})


test_that("a correlation skipped for a constant paired subset is named (R38)", {
    set.seed(38); n <- 20; w <- runif(n, 20, 80); w[1:6] <- 50
    d <- data.frame(whole = w, b1 = w + rnorm(n), b2 = w + rnorm(n), b3 = NA_real_)
    d$b3[1:6] <- 50 + rnorm(6, 0, 2)
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3")
    expect_match(notes_of(res$reproducibilitytable), "the same value in every paired case: b3 / whole", fixed = TRUE)
})


test_that("variance components give the reason that applies when no region reaches 5 values (R39)", {
    w <- c(22, 35, 48, 57, 66, 79, 30, 41)
    d <- data.frame(whole = w, b1 = c(w[1:4] + 1, rep(NA, 4)), b2 = c(rep(NA, 4), w[5:8] - 1))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    v <- notes_of(res$variancetable)
    expect_match(v, "Variance components need at least two measurements, each available in at least 5 cases", fixed = TRUE)
    expect_match(v, "b1 (n = 4)", fixed = TRUE)
    expect_false(grepl("case(s) qualify", v, fixed = TRUE))
    expect_false(grepl("needs at least two measurements per case", notes_of(res$samplesizetable), fixed = TRUE))
})


test_that("the sampling strategy is visible with the default outputs (R41)", {
    set.seed(41); w <- runif(20, 20, 80); d <- data.frame(whole = w, b1 = w + rnorm(20))
    note <- function(s) txt(ihc(d, wholesection = "whole", biopsy1 = "b1", sampling_strategy = s)$notices$content)
    expect_match(note("systematic"), "Sampling strategy recorded as systematic", fixed = TRUE)
    expect_match(note("random"), "Sampling strategy recorded as random", fixed = TRUE)
    expect_false(grepl("Sampling strategy", note("unknown"), fixed = TRUE))
})


test_that("recommendations requested without the panel that shows them say so (R42)", {
    set.seed(42); w <- runif(20, 20, 80); d <- data.frame(whole = w, b1 = w + rnorm(20))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", generate_recommendations = TRUE)
    expect_match(txt(res$notices$content), "tick Clinical interpretation under Output Options to see them", fixed = TRUE)
    path <- testthat::test_path("..", "..", "jamovi", "ihcheterogeneity.a.yaml")
    skip_if_not(file.exists(path), "jamovi yaml not available in the installed test context")
    opts <- yaml::read_yaml(path)$options
    desc <- function(nm) Filter(function(o) identical(o$name, nm), opts)[[1]]$description$R
    expect_gt(nchar(desc("showInterpretation")), 40)
    expect_match(desc("generate_recommendations"), "showInterpretation = TRUE", fixed = TRUE)
})


test_that("labels from the data cannot be read as HTML tags in table cells (R43)", {
    set.seed(43); n <- 24; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n), comp = rep(c("<IF", "Centre"), 12))
    names(d)[2] <- "<IF core"
    res <- ihc(d, wholesection = "whole", biopsy1 = "<IF core", spatial_id = "comp")
    expect_equal(res$samplingbiastable$asDF$comparison[1], "< IF core vs reference")
    expect_true("< IF" %in% res$spatialanalysistable$asDF$region)
    d$`<IF b2` <- w + rnorm(n)
    res2 <- ihc(d, wholesection = "whole", biopsy1 = "<IF core", biopsy2 = "<IF b2", spatial_id = "comp", compareCompartments = TRUE)
    expect_true("< IF" %in% res2$compartmentComparison$asDF$compartment)
    expect_true("Spearman correlation: < IF core vs reference" %in% res2$reproducibilitytable$asDF$metric)
})


test_that("syntax mode parses for column names with spaces and symbols (C2)", {
    a <- ClinicoPath:::ihcheterogeneityClass$new(
        options = ClinicoPath:::ihcheterogeneityOptions$new(wholesection = "Whole section", biopsy1 = "Core 1 (%)",
                                                            biopsies = "core \"b\""),
        data = data.frame())
    code <- a$asSource()
    expect_no_error(parse(text = code))
    call <- parse(text = code)[[1]]
    expect_equal(call$wholesection, "Whole section")
    expect_equal(call$biopsy1, "Core 1 (%)")
    expect_equal(call$biopsies, "core \"b\"")
})


test_that("a skipped pair is counted, named and never blamed on a near-zero mean (review 2026-10-19)", {
    set.seed(7); n <- 30; t0 <- runif(n, 20, 80)
    d <- data.frame(A = t0 + rnorm(n, 0, 0.3), B = t0 + rnorm(n, 0, 0.3), C = t0 + rnorm(n, 0, 0.3))
    d$C[3:n] <- NA; d$C[1:2] <- t0[1:2]                   # C shares 2 cases with A and with B
    d$C[3:4] <- t0[3:4]; d$B[3:4] <- NA                   # ...and 4 with A
    res <- ihc(d, biopsy1 = "A", biopsy2 = "B", biopsy3 = "C", showInterpretation = TRUE)
    b <- res$samplingbiastable$asDF
    expect_equal(b$comparison, c("B vs A", "C vs A"))
    n_txt <- notes_of(res$samplingbiastable)
    expect_match(n_txt, "2 paired comparisons are reported", fixed = TRUE)
    expect_match(n_txt, "Bonferroni-adjusted 96.7% CI", fixed = TRUE)    # three planned pairs
    expect_match(n_txt, "The adjustment also counts the comparisons that could not be made.", fixed = TRUE)
    expect_match(n_txt, "C vs B", fixed = TRUE)
    interp <- txt(res$interpretation$content)
    expect_false(grepl("near zero", interp, fixed = TRUE))
    expect_match(interp, "'C vs B'", fixed = TRUE)
})


test_that("one region is named only when the pattern implicates it alone (R24 culprit rule)", {
    set.seed(4); n <- 60; t0 <- runif(n, 20, 80)
    # k = 4: D offset from all three others, which agree
    d4 <- data.frame(A = t0 + rnorm(n), B = t0 + rnorm(n), C = t0 + rnorm(n), D = t0 + 5 + rnorm(n))
    r4 <- ihc(d4, biopsy1 = "A", biopsy2 = "B", biopsy3 = "C", biopsy4 = "D")
    expect_equal(nrow(r4$samplingbiastable$asDF), 6)
    expect_match(txt(r4$notices$content), "Region 'D' differs systematically from every other region", fixed = TRUE)
    # two regions offset in opposite directions: no single region is to blame
    opp <- data.frame(A = t0 + 4 + rnorm(n), B = t0 + rnorm(n), C = t0 - 4 + rnorm(n))
    ro <- ihc(opp, biopsy1 = "A", biopsy2 = "B", biopsy3 = "C")
    expect_match(txt(ro$notices$content), "Regions differ systematically by more than the 5% margin (", fixed = TRUE)
    expect_false(grepl("differs systematically from every other region", txt(ro$notices$content), fixed = TRUE))
})
