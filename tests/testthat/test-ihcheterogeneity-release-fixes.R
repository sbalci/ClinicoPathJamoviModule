# ═══════════════════════════════════════════════════════════
# Release-check fixes: ihcheterogeneity (OncoPath release check, 2026-09-18)
# ═══════════════════════════════════════════════════════════
#
# One block per defect found by the release-profile check (finding IDs I00-I36,
# GT00/GT01 in quality-reports/oncopath-release-2026-09-18/findings.json). Every
# number is checked against an independent computation (stats, psych, hand
# formulas), never against the module's own helpers.

txt <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))
verdict_of <- function(res) {
    t <- txt(res$interpretation$content)
    regmatches(t, regexpr(
        "(AGREEMENT THRESHOLDS MET, NOT CONFIRMED|AGREEMENT THRESHOLDS MET|MODERATE SAMPLING|INADEQUATE SAMPLING|NOT ADEQUATE FOR SUBSTITUTION|INSUFFICIENT DATA)", t))
}
ihc <- function(d, ...) ClinicoPath::ihcheterogeneity(data = d, ...)
note_text <- function(tbl) paste(vapply(tbl$notes, function(n) n$note, ""), collapse = " | ")

# 30 cases: region b1 over-reads the reference by ~11 %, region b2 under-reads by
# ~11 %. The mean of the two regions matches the reference.
opposite_bias <- function() {
    set.seed(11); n <- 30; w <- round(runif(n, 20, 80), 1)
    data.frame(whole = w, b1 = w * 1.11 + rnorm(n, 0, 1), b2 = w * 0.89 + rnorm(n, 0, 1))
}
clean <- function(n = 25, seed = 42) {
    set.seed(seed); w <- round(runif(n, 20, 80), 1)
    data.frame(whole = w, b1 = w + rnorm(n, 0, 2), b2 = w + rnorm(n, 0, 2))
}
underread <- function() {
    set.seed(42); n <- 25; w <- round(runif(n, 20, 80), 1)
    data.frame(whole = w, b1 = 0.70 * w + rnorm(n, 0, 0.5), b2 = 0.70 * w + rnorm(n, 0, 0.5))
}

# ── B. Bias judged per region (I00, I01, I14, I35) ────────────────────────────

test_that("opposite regional biases cannot cancel in the verdict (I00, I01)", {
    d <- opposite_bias()
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", showReportSentences = TRUE,
               showSummary = TRUE)
    # independent: each region is biased by ~11 % with p << 0.05
    for (b in c("b1", "b2")) {
        tt <- t.test(d[[b]], d$whole, paired = TRUE)
        expect_lt(tt$p.value, 1e-6)
        expect_gt(abs(unname(tt$estimate)) / mean(d$whole) * 100, 5)
    }
    expect_equal(verdict_of(res), "NOT ADEQUATE FOR SUBSTITUTION")
    it <- txt(res$interpretation$content)
    expect_match(it, "b1", fixed = TRUE)          # offending regions are named
    expect_match(it, "b2", fixed = TRUE)
    expect_false(grepl("no material systematic bias was detected", it))
    rs <- txt(res$report_sentences$content)
    expect_false(grepl("No systematic bias was detected", rs))
    expect_false(grepl("met the predefined quality criteria", rs, fixed = TRUE))
})

test_that("an exact constant offset vetoes the verdict and gives no -Inf effect size (I14)", {
    w <- c(12, 18, 25, 31, 36, 44, 50, 57, 63, 70, 22, 41)
    d <- data.frame(whole = w, b1 = w - 6, b2 = w - 6)      # 6 / mean(w) = 15 %
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_equal(verdict_of(res), "NOT ADEQUATE FOR SUBSTITUTION")
    it <- txt(res$interpretation$content)
    expect_false(grepl("Not enough paired observations", it))
    bt <- res$samplingbiastable$asDF
    expect_true(all(is.na(bt$p_value)))
    expect_true(all(is.na(bt$effect_size)))              # sd(d) = 0: g undefined, not -Inf
    expect_true(all(abs(bt$mean_diff + 6) < 1e-9))
})

test_that("Hedges' small-sample correction is applied at every n (I35)", {
    set.seed(5); n <- 60; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n, 1, 3))
    bt <- ihc(d, wholesection = "whole", biopsy1 = "b1")$samplingbiastable$asDF
    dd <- d$b1 - d$whole
    expect_equal(bt$effect_size[1], mean(dd) / sd(dd) * (1 - 3 / (4 * n - 5)), tolerance = 1e-8)
})

# ── C. One verdict for every panel (I02, I03, I17, I20, I21, I26, I27) ────────

test_that("copy-ready text, summary and recommendations follow the bias veto (I02, I21)", {
    res <- ihc(underread(), wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
               showReportSentences = TRUE, showSummary = TRUE, generate_recommendations = TRUE)
    expect_equal(verdict_of(res), "NOT ADEQUATE FOR SUBSTITUTION")
    rs <- txt(res$report_sentences$content)
    expect_false(grepl("met the predefined quality criteria", rs, fixed = TRUE))
    expect_match(rs, "systematic difference", ignore.case = TRUE)
    expect_false(grepl("p = <", rs, fixed = TRUE))
    sm <- txt(res$summary$content)
    expect_match(sm, "systematic", ignore.case = TRUE)
    expect_false(grepl("highly representative", sm, fixed = TRUE))
    it <- txt(res$interpretation$content)
    expect_match(it, "Bias Correction", fixed = TRUE)
})

test_that("the Methods paragraph describes a reference comparison only when there is a reference (I03)", {
    d <- clean()
    inter <- ihc(d, biopsy1 = "b1", biopsy2 = "whole", showReportSentences = TRUE)
    # since the second review the regions are compared with one another (paired
    # t-tests between regions); nothing may claim a reference that does not exist
    expect_false(grepl("from the reference measurement", txt(inter$report_sentences$content), fixed = TRUE))
    expect_match(txt(inter$report_sentences$content), "each region against the other region(s)", fixed = TRUE)
    ref <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", showReportSentences = TRUE)
    expect_match(txt(ref$report_sentences$content), "paired t-test", fixed = TRUE)
})

test_that("verdict text inherits the theme colour (I17)", {
    for (d in list(clean(), underread())) {
        html <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$interpretation$content
        expect_false(grepl("color: *(red|green|orange)", html))
    }
})

test_that("no panel claims a simulation study, a gold standard or unnamed guidelines (I20, I27)", {
    for (with_ref in c(TRUE, FALSE)) {
        d <- clean()
        res <- if (with_ref) {
            ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
                generate_recommendations = TRUE, showAssumptions = TRUE)
        } else {
            ihc(d, biopsy1 = "b1", biopsy2 = "b2", generate_recommendations = TRUE, showAssumptions = TRUE)
        }
        all_txt <- paste(txt(res$interpretation$content), txt(res$assumptions$content))
        expect_true(nzchar(txt(res$assumptions$content)))
        expect_false(grepl("simulation", all_txt, ignore.case = TRUE))
        expect_false(grepl("Zilenaite", all_txt))
        expect_false(grepl("gold standard", all_txt, ignore.case = TRUE))
        expect_false(grepl("ASCO/CAP", all_txt, fixed = TRUE))
        if (!with_ref) expect_false(grepl("Reference Standard", all_txt, fixed = TRUE))
    }
})

test_that("default options print no sampling-strategy warning and no false module list (I26)", {
    res <- ihc(clean(), biopsy1 = "b1", biopsy2 = "b2")
    it <- txt(res$interpretation$content)
    expect_false(grepl("Unknown sampling strategy", it, fixed = TRUE))
    expect_false(grepl("All analysis modules", it, fixed = TRUE))
})

# ── A. Usable cases (I06, I07, I08, I09) ──────────────────────────────────────

test_that("the gate counts cases with a reference value, not rows (I06, I07, I08)", {
    d <- clean(30)
    d3 <- d; d3$whole[-(1:3)] <- NA                       # 3 references in 30 rows
    expect_error(ihc(d3, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2"), "At least 5 complete cases")
    d4 <- clean(12); d4$whole[-(1:4)] <- NA               # 4 references in 12 rows
    expect_error(ihc(d4, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2"), "At least 5 complete cases")
    d0 <- d; d0$whole <- NA_real_                          # no reference at all
    expect_error(ihc(d0, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2"), "whole", fixed = TRUE)
    # inter-regional: only 4 rows carry two regions
    di <- clean(10); di$b2[-(1:4)] <- NA
    expect_error(ihc(di, biopsy1 = "b1", biopsy2 = "b2"), "At least 5 complete cases")
})

test_that("the small-sample notice and Study Design line count usable cases (I07, I08)", {
    d <- clean(20); d$whole[-(1:7)] <- NA                 # 7 usable cases in 20 rows
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_match(txt(res$notices$content), "7 cases", fixed = TRUE)
    expect_match(txt(res$interpretation$content), "7 cases", fixed = TRUE)
    # the correlations and the bias test use the same 7 cases
    bt <- res$samplingbiastable$asDF
    expect_equal(bt$n[1], 7)
    tt <- t.test(d$b1[1:7], d$whole[1:7], paired = TRUE)
    expect_equal(bt$mean_diff[1], unname(tt$estimate), tolerance = 1e-10)
})

test_that("a correlation from fewer than 5 pairs is not reported and a sparse region cannot remove the ICC (I09)", {
    d <- clean(20); d$b3 <- NA_real_; d$b3[1:2] <- d$whole[1:2] + c(1, -1)
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3")
    tab <- res$reproducibilitytable
    df <- tab$asDF
    r_ref <- c(cor(d$whole, d$b1, method = "spearman"), cor(d$whole, d$b2, method = "spearman"))
    expect_equal(df$value[df$metric == "Mean Regional-Reference Correlation"], mean(r_ref), tolerance = 1e-10)
    expect_match(note_text(tab), "b3", fixed = TRUE)
    icc_row <- df[grepl("absolute agreement", df$metric), ]
    ref_icc <- psych::ICC(as.matrix(d[, c("whole", "b1", "b2")]), lmer = FALSE)$results
    expect_equal(icc_row$value, ref_icc$ICC[2], tolerance = 1e-8)
})

# ── D. Bias uncertainty (I11, I30) ────────────────────────────────────────────

test_that("the bias table shows the CI of the mean difference and the limits of agreement (I11, I30)", {
    set.seed(9); n <- 15; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n, 0.5, 8))
    bt <- ihc(d, wholesection = "whole", biopsy1 = "b1")$samplingbiastable$asDF
    dd <- d$b1 - d$whole
    tt <- t.test(d$b1, d$whole, paired = TRUE)
    expect_equal(c(bt$ci_lower[1], bt$ci_upper[1]), as.numeric(tt$conf.int), tolerance = 1e-10)
    expect_equal(c(bt$loa_lower[1], bt$loa_upper[1]), mean(dd) + c(-1.96, 1.96) * sd(dd), tolerance = 1e-10)
    # small point estimate but a CI reaching well past 5 % of the reference mean
    reach <- max(abs(tt$conf.int)) / mean(w) * 100
    expect_gt(reach, 5)
    expect_match(bt$clinical_impact[1], "CI", fixed = TRUE)
})

test_that("the regional-reference correlation row carries a Fisher-z CI (I11; Bonett-Wright variance, review R05)", {
    d <- clean(25)
    df <- ihc(d, wholesection = "whole", biopsy1 = "b1")$reproducibilitytable$asDF
    row <- df[df$metric == "Mean Regional-Reference Correlation", ]
    # Bonett & Wright (2000), Psychometrika 65:23-28: var(atanh r_s) = (1 + r^2/2)/(n - 3).
    # The 1.06/(n - 3) used first covered only 85-92% at r = 0.90-0.95.
    r <- cor(d$whole, d$b1, method = "spearman"); se <- sqrt((1 + r^2 / 2) / (25 - 3))
    expect_equal(c(row$ci_lower, row$ci_upper), tanh(atanh(r) + c(-1, 1) * qnorm(0.975) * se), tolerance = 1e-10)
})

# ── E. Sample-size planning (I12, I13) ────────────────────────────────────────

test_that("sample-size planning is ICC precision (Bonett 2002), for both designs (I12, I13)", {
    d <- clean(30)
    # Shown under the Comprehensive focus (it now answers the right question) and
    # otherwise only when ticked.
    repro <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", analysis_type = "reproducibility")
    expect_false(repro$samplesizetable$visible)

    bonett_n <- function(rho, k, w) ceiling(8 * qnorm(0.975)^2 * (1 - rho)^2 * (1 + (k - 1) * rho)^2 / (k * (k - 1) * w^2) + 1)
    for (with_ref in c(TRUE, FALSE)) {
        res <- if (with_ref) ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", sample_size_planning = TRUE)
               else ihc(d, biopsy1 = "b1", biopsy2 = "b2", sample_size_planning = TRUE)
        k <- if (with_ref) 3 else 2
        pt <- res$samplesizetable$asDF
        expect_true(res$samplesizetable$visible)
        row <- pt[abs(pt$planning_icc - 0.75) < 1e-12, ]
        expect_equal(nrow(row), 1)
        expect_equal(row$n_w20, bonett_n(0.75, k, 0.20))
        expect_equal(row$n_w10, bonett_n(0.75, k, 0.10))
        expect_false(any(grepl("sample increase", unlist(pt), ignore.case = TRUE)))
    }
    # Bonett's worked example (Stat Med 2002;21:1331): rho = 0.8, k = 2, w = 0.2 needs 51 subjects
    expect_equal(bonett_n(0.8, 2, 0.2), 51)
})

# ── F. One grading rule (I10, I29) ────────────────────────────────────────────

test_that("correlations are graded against the user's threshold in every panel (I10, I29)", {
    set.seed(3); n <- 40; t <- runif(n, 0, 100)
    d <- data.frame(b1 = t + rnorm(n, 0, 25), b2 = t + rnorm(n, 0, 25))
    r <- cor(d$b1, d$b2, method = "spearman")
    expect_true(r > 0.55 && r < 0.9)
    lo <- ihc(d, biopsy1 = "b1", biopsy2 = "b2", correlation_threshold = 0.5, showReportSentences = TRUE)
    hi <- ihc(d, biopsy1 = "b1", biopsy2 = "b2", correlation_threshold = 0.95, showReportSentences = TRUE)
    row_of <- function(res) { df <- res$reproducibilitytable$asDF; df[df$metric == "Mean Inter-Regional Correlation", ] }
    expect_match(row_of(lo)$interpretation, "Meets", fixed = TRUE)
    expect_match(row_of(hi)$interpretation, "Below", fixed = TRUE)
    expect_false(grepl("excellent", txt(lo$report_sentences$content), ignore.case = TRUE))
})

test_that("ICC bands follow Koo & Li in the table and the summary (I10, I29)", {
    d <- clean(25)                             # ICC(A,1) well above 0.90
    icc <- psych::ICC(as.matrix(d), lmer = FALSE)$results$ICC[2]
    expect_gt(icc, 0.90)
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", showSummary = TRUE)
    df <- res$reproducibilitytable$asDF
    expect_equal(df$interpretation[grepl("absolute agreement", df$metric)], "Excellent reliability")
    expect_match(txt(res$summary$content), "Excellent", fixed = TRUE)
})

# ── G. Compartments (I04, I05, I23, I24, I25, I34) ────────────────────────────

compartments <- function() {
    set.seed(21); n <- 24; w <- runif(n, 20, 80)
    comp <- factor(rep(c("Central", "Front", "Edge"), each = 8))
    sdv <- c(Central = 1, Front = 6, Edge = 2)[as.character(comp)]
    off <- c(Central = -1.1, Front = 3.2, Edge = -2.3)[as.character(comp)]
    data.frame(whole = w, b1 = w + off + rnorm(n, 0, sdv), b2 = w + off + rnorm(n, 0, sdv), comp = comp)
}

test_that("compartment bias text uses the sign of the bias and no raw-unit window (I04, I24, I25)", {
    d <- compartments()
    cc <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp",
              compareCompartments = TRUE)$compartmentComparison$asDF
    bias <- cc[grepl("Bias", cc$metric), ]
    expect_equal(nrow(bias), 3)
    expect_false(any(grepl("Higher (positive|negative) bias", bias$comparison)))
    for (i in seq_len(nrow(bias))) {
        m <- d$comp == bias$compartment[i]
        own <- (mean(rowMeans(d[m, c("b1", "b2")]) - d$whole[m])) / mean(d$whole[m]) * 100
        shown <- as.numeric(sub("^[^-+0-9]*([-+][0-9.]+)%.*$", "\\1", bias$comparison[i]))
        expect_equal(shown, round(own, 1), tolerance = 0.051)
    }
})

test_that("compartment tests include a test of per-case CV and report both F degrees of freedom (I05, I34)", {
    d <- compartments()
    tt <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp",
              compartmentTests = TRUE)$compartmentTests$asDF
    cvrow <- tt[grepl("per-case CV", tt$test_type) & grepl("Kruskal", tt$test_type), ]
    expect_equal(nrow(cvrow), 1)
    m <- as.matrix(d[, c("whole", "b1", "b2")])
    cv <- apply(m, 1, sd) / rowMeans(m) * 100
    ref <- kruskal.test(cv ~ d$comp)
    expect_equal(cvrow$statistic, unname(ref$statistic), tolerance = 1e-8)
    expect_equal(cvrow$p_value, ref$p.value, tolerance = 1e-10)
    bf <- tt[grepl("Brown-Forsythe", tt$test_type), ]
    expect_equal(c(bf$df1, bf$df2), c(2, 21))
})

test_that("compartments skipped for size are named, with one minimum everywhere (I23)", {
    d <- compartments(); d$comp <- as.character(d$comp); d$comp[1:6] <- "Tiny"; d$comp[7:8] <- "Tiny2"
    d$comp[7:8] <- "Tiny2"; d$comp <- factor(d$comp)       # Tiny2 has 2 cases
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp",
               compareCompartments = TRUE, compartmentTests = TRUE)
    for (tb in list(res$spatialanalysistable, res$compartmentComparison, res$compartmentTests))
        expect_match(note_text(tb), "Tiny2", fixed = TRUE)
    expect_false("Tiny2" %in% res$spatialanalysistable$asDF$region)
})

# ── H. CV (I15, I16, I36) ─────────────────────────────────────────────────────

test_that("near-zero cases do not dominate the mean CV and exclusions are counted (I15, I16)", {
    set.seed(7); n <- 20; hi <- runif(n, 20, 60)
    d_hi <- data.frame(whole = round(hi, 1), b1 = round(hi * (1 + rnorm(n, 0, .04)), 1),
                       b2 = round(hi * (1 + rnorm(n, 0, .04)), 1))
    d_lo <- rbind(d_hi, data.frame(whole = c(0, 1, 0, 1), b1 = c(1, 0, 0, 0), b2 = c(0, 0, 1, 1)),
                  data.frame(whole = 0, b1 = 0, b2 = 0))           # one all-zero case: exact agreement
    tab <- ihc(d_lo, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$reproducibilitytable
    df <- tab$asDF
    cv_mean <- df$value[grepl("Mean Coefficient of Variation", df$metric)]
    # Floor = 2% of the 95th percentile of all values; every case below it is left
    # out whatever its spread (review R03: keeping only the all-zero case selected
    # on the outcome).
    m <- as.matrix(d_lo); floor_v <- 0.02 * quantile(abs(m), 0.95, names = FALSE)
    case_cv <- ifelse(rowMeans(m) < floor_v, NA, apply(m, 1, sd) / rowMeans(m) * 100)
    expect_equal(cv_mean, mean(case_cv, na.rm = TRUE), tolerance = 1e-10)
    expect_lt(cv_mean, 10)
    expect_match(note_text(tab), "5 case(s) whose mean", fixed = TRUE)
    expect_match(note_text(tab), "1 of them had identical", fixed = TRUE)
    med <- df$value[grepl("Median per-case CV", df$metric)]
    expect_equal(med, median(case_cv, na.rm = TRUE), tolerance = 1e-10)
})

test_that("all-constant data are reported as having no variability, not 100% sampling variance (I36)", {
    d <- data.frame(whole = rep(40, 8), b1 = rep(40, 8), b2 = rep(40, 8))
    vt <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
              variance_components = TRUE)$variancetable
    df <- vt$asDF
    expect_true(all(is.na(df$percentage)))
    expect_match(note_text(vt), "no variability", ignore.case = TRUE)
})

# ── I. Recommendations (I18, I19) ─────────────────────────────────────────────

test_that("recommendations use the user's threshold and a material-bias rule (I18, I19)", {
    set.seed(8); n <- 400; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + 0.4 + rnorm(n, 0, 2), b2 = w + 0.4 + rnorm(n, 0, 2))
    tt <- t.test(rowMeans(d[, 2:3]), d$whole, paired = TRUE)
    expect_lt(tt$p.value, 0.05)                           # significant ...
    expect_lt(unname(tt$estimate) / mean(w) * 100, 2)     # ... but trivial
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
               generate_recommendations = TRUE, cv_threshold = 25)
    it <- txt(res$interpretation$content)
    expect_false(grepl("Bias Correction", it, fixed = TRUE))
    expect_false(grepl("2-3 additional", it, fixed = TRUE))
    expect_false(grepl("CV > 30%", it, fixed = TRUE))
    expect_match(it, "25%", fixed = TRUE)
})

# ── J. Constant reference (I28) ───────────────────────────────────────────────

test_that("a constant reference is named and never shown as NaN (I28)", {
    set.seed(4); d <- data.frame(whole = rep(50, 10), b1 = runif(10, 30, 70), b2 = runif(10, 30, 70))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    df <- res$reproducibilitytable$asDF
    expect_false(any(is.nan(df$value)))
    expect_true(any(grepl("absolute agreement", df$metric)))
    expect_match(txt(c(res$notices$content, res$interpretation$content)), "whole", fixed = TRUE)
    expect_match(txt(res$notices$content), "constant", ignore.case = TRUE)
})

# ── K. Closed-form mean squares (I32) ─────────────────────────────────────────

test_that("ICC(A,1) and ICC(C,1) with CIs equal psych::ICC (I32)", {
    for (k in c(2, 4)) {
        set.seed(k); n <- 30; t <- runif(n, 10, 90)
        m <- sapply(seq_len(k), function(j) t + j * 1.5 + rnorm(n, 0, 6))
        d <- as.data.frame(m); names(d) <- paste0("r", seq_len(k))
        args <- c(list(data = d), setNames(as.list(names(d))[1:min(k, 4)], paste0("biopsy", 1:min(k, 4))))
        df <- do.call(ClinicoPath::ihcheterogeneity, args)$reproducibilitytable$asDF
        ref <- psych::ICC(m, lmer = FALSE)$results
        a <- df[grepl("absolute agreement", df$metric), ]
        c3 <- df[grepl("consistency", df$metric), ]
        expect_equal(c(a$value, a$ci_lower, a$ci_upper),
                     c(ref$ICC[2], ref$`lower bound`[2], ref$`upper bound`[2]), tolerance = 1e-8)
        expect_equal(c(c3$value, c3$ci_lower, c3$ci_upper),
                     c(ref$ICC[3], ref$`lower bound`[3], ref$`upper bound`[3]), tolerance = 1e-8)
    }
})

test_that("a 2,000-case cohort runs in seconds, not minutes (I32)", {
    set.seed(1); n <- 2000; t <- runif(n, 5, 95)
    d <- data.frame(whole = t, b1 = t + rnorm(n, 0, 4), b2 = t + rnorm(n, 0, 4))
    el <- system.time(ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
                          variance_components = TRUE))[["elapsed"]]
    expect_lt(el, 10)
})

# ── L. Labels (I31, I33) ──────────────────────────────────────────────────────

test_that("regions are labelled by variable name (I31) and the case component is one label (I33)", {
    d <- clean(); names(d) <- c("whole", "invasive_front", "centre")
    res <- ihc(d, wholesection = "whole", biopsy1 = "invasive_front", biopsy3 = "centre")
    bt <- res$samplingbiastable$asDF
    expect_true(any(grepl("invasive_front", bt$comparison, fixed = TRUE)))
    expect_true(any(grepl("centre", bt$comparison, fixed = TRUE)))
    inter <- ihc(d, biopsy1 = "invasive_front", biopsy2 = "centre", variance_components = TRUE)
    expect_equal(inter$variancetable$asDF$component[1], "Between-Case Variance")
})

# ── GT00 / GT01. Turkish catalogue ────────────────────────────────────────────

po_entries <- function(path) {
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    unq <- function(s) gsub('\\\\"', '"', sub('^"', "", sub('"$', "", s)))
    out <- list(); cur <- NULL; field <- NULL; refs <- character(0)
    flush <- function() if (!is.null(cur) && nzchar(cur$msgid) && nzchar(cur$msgstr)) out[[length(out) + 1]] <<- cur
    for (ln in lines) {
        if (startsWith(ln, "#:")) refs <- c(refs, strsplit(trimws(sub("^#:", "", ln)), " +")[[1]])
        else if (startsWith(ln, "msgid ")) { flush(); cur <- list(msgid = unq(sub("^msgid ", "", ln)), msgstr = "", refs = refs); field <- "msgid"; refs <- character(0) }
        else if (startsWith(ln, "msgstr ")) { cur$msgstr <- unq(sub("^msgstr ", "", ln)); field <- "msgstr" }
        else if (startsWith(ln, "\"") && !is.null(cur) && !is.null(field)) cur[[field]] <- paste0(cur[[field]], unq(ln))
        else if (!nzchar(ln)) field <- NULL
    }
    flush(); out
}
conversions <- function(s) {
    s <- gsub("%%", "", s, fixed = TRUE)
    regmatches(s, gregexpr("%([0-9]+\\$)?[-+ #0]*[0-9]*(\\.[0-9]+)?[sdifeEgGxX]", s))[[1]]
}
conv_type <- function(cv) sub(".*([sdifeEgGxX])$", "\\1", cv)

test_that("no Turkish msgstr reorders conversions of different types without %n$ markers (GT00)", {
    po <- testthat::test_path("..", "..", "jamovi", "i18n", "tr.po")
    skip_if_not(file.exists(po))
    # Scope: the OncoPath analyses (the check that found this). The same scan over
    # the whole umbrella catalogue finds 18 more entries in other modules, logged
    # as a follow-up in the release-check folder.
    oncopath <- c("R/ihcheterogeneity.b.R", "R/diagnosticmeta.b.R", "R/swimmerplot.b.R", "R/waterfall.b.R")
    bad <- character(0)
    for (e in po_entries(po)) {
        if (!any(e$refs %in% oncopath)) next
        ci <- conversions(e$msgid); cs <- conversions(e$msgstr)
        if (length(ci) < 2 || length(cs) != length(ci)) next
        if (any(grepl("\\$", cs))) next
        if (!identical(conv_type(ci), conv_type(cs))) bad <- c(bad, e$msgid)
    }
    expect_equal(bad, character(0))
})

test_that("Turkish msgstrs that reword count sentences keep the counts on their nouns (GT01)", {
    po <- testthat::test_path("..", "..", "jamovi", "i18n", "tr.po")
    skip_if_not(file.exists(po))
    entries <- po_entries(po)
    tr_of <- function(id) { hit <- Filter(function(e) identical(e$msgid, id), entries)
                            if (length(hit)) hit[[1]]$msgstr else NA_character_ }
    # sprintf() fills conversions in English order; a reworded Turkish sentence
    # must use %n$ markers or the counts land on the wrong nouns.
    dm <- tr_of("Meta-regression on %d studies with a covariate contributing %d model parameter(s) (%d residual degrees of freedom). The usual recommendation is at least 10 studies per covariate, so estimates may be unstable and confidence intervals unreliable. Report this as exploratory only.")
    expect_false(is.na(dm))
    out <- sprintf(dm, 12L, 3L, 9L)
    expect_match(out, "3 model parametresine", fixed = TRUE)
    expect_match(out, "12 \u00e7al\u0131\u015fma")
    sw <- tr_of("Study included %d patients with %d timeline observations. Median observed duration was %.1f %s (range: %.1f to %.1f %s).")
    expect_false(is.na(sw))
    out <- sprintf(sw, 40L, 170L, 5.5, "ay", 1, 9, "ay")
    expect_match(out, "170 zaman", fixed = TRUE)
    expect_match(out, "40 hasta", fixed = TRUE)
})

# ═══════════════════════════════════════════════════════════
# Independent review of the fix (2026-09-18): findings R01-R38
# ═══════════════════════════════════════════════════════════

test_that("an offset near the margin whose CI spans it is inconclusive, never green or 'material' (R01, S02)", {
    # b1 about +6%, b2 about -5%: point estimates just beyond 5%, CIs from about 1% to 11%.
    set.seed(50); n <- 30; w <- round(runif(n, 20, 80), 1)
    d <- data.frame(whole = w, b1 = w * 1.07 + rnorm(n, 0, 6), b2 = w + rnorm(n, 0, 6))
    rel_ci <- function(x, level, m) { dd <- x - d$whole
        (mean(dd) + c(-1, 1) * qt(1 - (1 - level) / 2, n - 1) * sd(dd) / sqrt(n)) / mean(d$whole) * 100 }
    # independent three-zone check with m = 3 comparisons (2 regions + the mean of both)
    for (b in c("b1", "b2")) {
        ci_adj <- rel_ci(d[[b]], 1 - 0.10 / 3); ci90 <- rel_ci(d[[b]], 0.90)
        expect_false(min(ci_adj) > 5 || max(ci_adj) < -5)   # not shown material
        expect_false(all(abs(ci90) <= 5))                    # not ruled out
    }
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET, NOT CONFIRMED")
    expect_match(txt(res$interpretation$content), "neither ruled out nor shown", fixed = TRUE)
})

test_that("a difference that is not significant but not ruled out blocks the plain green verdict (R06)", {
    w <- c(22, 35, 48, 57, 66, 79)
    d <- data.frame(whole = w, b1 = w + c(-5, 1, -4, 2, -3, -3))
    tt <- t.test(d$b1, d$whole, paired = TRUE)
    reach <- max(abs(tt$conf.int)) / mean(w) * 100
    expect_gt(tt$p.value, 0.05)                    # not significant ...
    expect_gt(reach, 5)                            # ... and not ruled out within 5%
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1")
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET, NOT CONFIRMED")
    ci90 <- (mean(d$b1 - w) + c(-1, 1) * qt(0.95, 5) * sd(d$b1 - w) / sqrt(6)) / mean(w) * 100
    expect_match(txt(res$interpretation$content), sprintf("(90%% CI %.1f%% to %.1f%%)", ci90[1], ci90[2]), fixed = TRUE)
    # with agreement actually shown (every CI inside 5%) the plain label is used
    ok <- ihc(clean(40), wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_equal(verdict_of(ok), "AGREEMENT THRESHOLDS MET")
})

test_that("near-zero cases are excluded whether or not they agree exactly (R03)", {
    set.seed(9); n <- 24; w <- runif(n, 20, 90)
    base <- data.frame(whole = w, b1 = w * (1 + rnorm(n, 0, 0.3)), b2 = w * (1 + rnorm(n, 0, 0.3)))
    a <- rbind(base, data.frame(whole = rep(0, 12), b1 = rep(0, 12), b2 = rep(0, 12)))
    b <- rbind(base, data.frame(whole = rep(0, 12), b1 = rep(0.1, 12), b2 = rep(0, 12)))
    cv_of <- function(d) { df <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$reproducibilitytable$asDF
                           df$value[grepl("Mean Coefficient of Variation", df$metric)] }
    hand <- mean(apply(as.matrix(base), 1, sd) / rowMeans(as.matrix(base)) * 100)
    expect_equal(cv_of(a), hand, tolerance = 1e-8)
    expect_equal(cv_of(b), hand, tolerance = 1e-8)
})

test_that("one mistyped high value does not move the CV floor (R07, R21)", {
    set.seed(4); n <- 40; w <- c(runif(24, 2, 8), runif(16, 20, 85))
    d <- data.frame(whole = w, b1 = w + rnorm(n, 0, 0.5), b2 = w + rnorm(n, 0, 0.5))
    d$b1 <- pmax(d$b1, 0.5); d$b2 <- pmax(d$b2, 0.5)
    typo <- d; typo$b2[40] <- 250
    notes <- function(x) note_text(ihc(x, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$reproducibilitytable)
    expect_false(grepl("left out", notes(d)))
    expect_false(grepl("left out", notes(typo)))
})

test_that("the regions-per-case advice uses a single region's error (R04)", {
    set.seed(31); n <- 60; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w * (1 + rnorm(n, 0, 0.30)))
    rms <- sqrt(mean(((d$b1 - d$whole) / d$whole)^2)) * 100
    expect_gt(rms, 20)
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", generate_recommendations = TRUE)
    it <- txt(res$interpretation$content)
    expect_match(it, sprintf("about %.1f%% (root-mean-square relative difference", rms), fixed = TRUE)
    # descriptive only: the single-region error is not graded against the CV threshold,
    # which applies to a different quantity (second review S01)
    expect_false(grepl("A single region met the CV threshold", it, fixed = TRUE))
    expect_false(grepl("regions per case would bring", it, fixed = TRUE))
})

test_that("a region fails the verdict only when SHOWN below the threshold; the widest limits of agreement are named (R10, R14, S04)", {
    four <- function(noise4) {
        set.seed(8); n <- 60; w <- runif(n, 10, 90)
        data.frame(whole = w, c1 = w + rnorm(n, 0, 1.5), c2 = w + rnorm(n, 0, 1.5),
                   c3 = w + rnorm(n, 0, 1.5), c4 = w + rnorm(n, 0, noise4))
    }
    bw_upper <- function(r, n) tanh(atanh(r) + qnorm(0.975) * sqrt((1 + r^2 / 2) / (n - 3)))
    run <- function(d) ihc(d, wholesection = "whole", biopsy1 = "c1", biopsy2 = "c2", biopsy3 = "c3",
                           biopsy4 = "c4", correlation_threshold = 0.90)

    # (a) c4 below 0.90 as a point estimate only: named, but it does not fail the verdict
    d <- four(15); r4 <- cor(d$whole, d$c4, method = "spearman")
    expect_lt(r4, 0.90); expect_gt(bw_upper(r4, 60), 0.90)
    res <- run(d)
    it <- txt(res$interpretation$content)
    expect_match(it, "Below the threshold as a point estimate only: 'c4' (r = ", fixed = TRUE)
    df <- res$reproducibilitytable$asDF
    expect_equal(df$value[df$metric == "Spearman correlation: c4 vs reference"], r4, tolerance = 1e-12)
    loa <- mean(d$c4 - d$whole) + c(-1.96, 1.96) * sd(d$c4 - d$whole)
    expect_match(it, sprintf("limits of agreement are %.2f to %.2f (region 'c4')", loa[1], loa[2]), fixed = TRUE)

    # (b) a noisier c4 whose whole CI lies below 0.90: shown below, the verdict fails
    d <- four(30); r4 <- cor(d$whole, d$c4, method = "spearman")
    expect_lt(bw_upper(r4, 60), 0.90)
    res <- run(d)
    expect_false(grepl("AGREEMENT THRESHOLDS MET", verdict_of(res)))
    expect_match(txt(res$interpretation$content), "Shown to be below the threshold on their own", fixed = TRUE)
})

test_that("the plain-language summary follows a non-green verdict (R12, R36)", {
    set.seed(8); n <- 60; w <- runif(n, 10, 90)
    d <- data.frame(whole = w, c1 = w + rnorm(n, 0, 1.5), c4 = w + rnorm(n, 0, 15))
    res <- ihc(d, wholesection = "whole", biopsy1 = "c1", biopsy2 = "c4", correlation_threshold = 0.90,
               showSummary = TRUE)
    expect_false(grepl("AGREEMENT THRESHOLDS MET", verdict_of(res)))
    sm <- txt(res$summary$content)
    expect_false(grepl("agree closely|gave similar values", sm))
    expect_match(sm, "Overall:", fixed = TRUE)
})

test_that("with one biased region only that region is named for calibration (R13)", {
    set.seed(13); n <- 40; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, front = w + rnorm(n, 0, 1), centre = w + rnorm(n, 0, 1), margin = w * 0.85 + rnorm(n, 0, 1))
    res <- ihc(d, wholesection = "whole", biopsy1 = "front", biopsy2 = "centre", biopsy3 = "margin",
               showReportSentences = TRUE, showSummary = TRUE)
    rs <- txt(res$report_sentences$content)
    expect_match(rs, "region(s) 'margin' are systematically offset", fixed = TRUE)
    expect_false(grepl("'front' are", rs, fixed = TRUE))
    expect_match(txt(res$summary$content), "region(s) 'margin'", fixed = TRUE)
})

test_that("a region name containing ' [' does not cut its table note short (R20)", {
    set.seed(1); n <- 20; w <- runif(n, 10, 80)
    d <- data.frame(w = w, a = w + rnorm(n), b = w + rnorm(n)); names(d)[3] <- "Ki67 [core 3]"
    d[[3]][-(1:2)] <- NA
    res <- ihc(d, wholesection = "w", biopsy1 = "a", biopsy2 = "Ki67 [core 3]")
    nt <- note_text(res$reproducibilitytable)
    expect_match(nt, "core 3] (n = 2)", fixed = TRUE)
})

test_that("the biopsy plot draws a reference-design state saved without ref_label (R22) and a repeated variable is analysed once (R24)", {
    d <- clean(20)
    o <- ClinicoPath:::ihcheterogeneityOptions$new(wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
                                                  show_variability_plots = TRUE)
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d)
    an$results$biopsyplot$setState(list(whole_section = d$whole, biopsy_data = d[, c("b1", "b2")],
                                        spatial_regions = NULL, n_cases = 20, n_biopsies = 2))
    grDevices::png(tempfile(fileext = ".png")); on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(an$.__enclos_env__$private$.biopsyplot(an$results$biopsyplot, ggtheme = ggplot2::theme_grey(), theme = list()))

    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsies = "b1")
    expect_match(txt(res$notices$content), "selected more than once", fixed = TRUE)
    icc <- res$reproducibilitytable$asDF
    expect_equal(icc$value[grepl("absolute agreement", icc$metric)],
                 psych::ICC(as.matrix(d[, c("whole", "b1", "b2")]), lmer = FALSE)$results$ICC[2], tolerance = 1e-8)
})

test_that("sample-size planning works under a non-comprehensive focus and uses ICC(3,1) for the observed row (R26, R02)", {
    d <- clean(30)
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
               analysis_type = "reproducibility", sample_size_planning = TRUE)
    expect_true(res$samplesizetable$visible)
    pt <- res$samplesizetable$asDF
    obs <- pt[grepl("Observed", pt$scenario), ]
    expect_match(obs$scenario, "ICC(3,1)", fixed = TRUE)
    expect_equal(obs$planning_icc, psych::ICC(as.matrix(d), lmer = FALSE)$results$ICC[3], tolerance = 1e-8)
    expect_match(note_text(res$samplesizetable), "CI of ICC(2,1) has width", fixed = TRUE)
})

test_that("regions measured in different cases give 'not estimable', never a silent subset ICC (R26, R33)", {
    w <- c(12, 25, 33, 41, 48, 55, 61, 67, 74, 80)
    b1 <- c(w[1:6] * 0.8, rep(NA, 4)); b2 <- c(rep(NA, 3), w[4:10] + c(1, -1, 2, -2, 1, 0, -1))
    d <- data.frame(ref = w, b1 = b1, b2 = b2)
    expect_equal(sum(complete.cases(d)), 3)
    res <- ihc(d, wholesection = "ref", biopsy1 = "b1", biopsy2 = "b2", showReportSentences = TRUE)
    df <- res$reproducibilitytable$asDF
    row <- df[grepl("absolute agreement", df$metric), ]
    expect_true(is.na(row$value))
    expect_match(row$interpretation, "Not estimable", fixed = TRUE)
    expect_match(note_text(res$reproducibilitytable), "Only 3 cases have every measurement", fixed = TRUE)
    expect_match(txt(res$notices$content), "ICC not estimable", fixed = TRUE)
    expect_false(grepl("The ICC(2,1) was", txt(res$report_sentences$content), fixed = TRUE))
})

test_that("tied per-case CVs do not abort the compartment tests (R32)", {
    x <- seq(10, 80, 10)
    d <- data.frame(w = x, b1 = x, b2 = x, comp = factor(rep(c("A", "B"), each = 4)))
    res <- expect_no_error(ihc(d, wholesection = "w", biopsy1 = "b1", biopsy2 = "b2",
                               spatial_id = "comp", compartmentTests = TRUE))
    tt <- res$compartmentTests$asDF
    expect_match(tt$interpretation[grepl("per-case CV", tt$test_type) & grepl("Kruskal", tt$test_type)],
                 "identical", fixed = TRUE)
})

test_that("ICC CIs are reported when the residual mean square is zero (R35)", {
    ref <- c(5, 12, 18, 25, 31, 40, 47, 55, 62, 70)
    d <- data.frame(ref = ref, b1 = ref + 3)
    df <- ihc(d, wholesection = "ref", biopsy1 = "b1")$reproducibilitytable$asDF
    ps <- psych::ICC(as.matrix(d), lmer = FALSE)$results
    a <- df[grepl("absolute agreement", df$metric), ]
    expect_equal(c(a$value, a$ci_lower, a$ci_upper), c(ps$ICC[2], ps$`lower bound`[2], ps$`upper bound`[2]), tolerance = 1e-6)
    # r = 1 up to rounding: no zero-width correlation CI (R38)
    r1 <- df[df$metric == "Mean Regional-Reference Correlation", ]
    expect_true(is.na(r1$ci_lower) && is.na(r1$ci_upper))
})

test_that("a constant reference gives INSUFFICIENT DATA, not a calibration verdict (R37)", {
    set.seed(5); d <- data.frame(whole = rep(40, 20), b1 = runif(20, 5, 50), b2 = runif(20, 5, 50))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_equal(verdict_of(res), "INSUFFICIENT DATA")
    expect_match(txt(res$interpretation$content), "'whole' has the same value in every case", fixed = TRUE)
})

test_that("compartment CV bands are compared with each other compartment (R09, R16)", {
    set.seed(17); n <- 24; w <- runif(n, 30, 70)
    comp <- factor(rep(c("A", "B", "C"), each = 8))
    s <- c(A = 0.02, B = 0.02, C = 0.15)[as.character(comp)]
    d <- data.frame(whole = w, b1 = w * (1 + rnorm(n, 0, s)), b2 = w * (1 + rnorm(n, 0, s)), comp = comp)
    cc <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp",
              compareCompartments = TRUE)$compartmentComparison$asDF
    cvr <- cc[cc$metric == "Mean CV (%)", ]
    expect_equal(cvr$comparison[cvr$compartment == "A"], "Lower CV band than C")
    expect_equal(cvr$comparison[cvr$compartment == "B"], "Lower CV band than C")
    expect_equal(cvr$comparison[cvr$compartment == "C"], "Higher CV band than A, B")
})

test_that("wording fits the design: two regions and a single region (R18, R41)", {
    d <- clean()
    two <- ihc(d, biopsy1 = "b1", biopsy2 = "b2", showReportSentences = TRUE)
    expect_match(txt(two$report_sentences$content), "each with both regional measurements", fixed = TRUE)
    expect_false(grepl("at least two of 2", txt(two$interpretation$content), fixed = TRUE))
    one <- ihc(d, wholesection = "whole", biopsy1 = "b1", showReportSentences = TRUE)
    rs <- txt(one$report_sentences$content)
    expect_match(rs, "The Spearman correlation between the regional and reference measurements was", fixed = TRUE)
    expect_false(grepl("Holm-adjusted", sub("Methods Section:.*Results Section:", "", rs)))
})

test_that("the spatial plot does not print 'CV: NA%' (R31)", {
    set.seed(3); w <- c(runif(20, 0, 0.3), runif(20, 20, 60))
    d <- data.frame(whole = w, b1 = w + runif(40, 0, 0.2), b2 = w + runif(40, 0, 0.2),
                    comp = factor(rep(c("A", "B"), each = 20)))
    o <- ClinicoPath:::ihcheterogeneityOptions$new(wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
                                                  spatial_id = "comp", show_variability_plots = TRUE)
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d); an$run()
    grDevices::png(tempfile(fileext = ".png")); on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(an$.__enclos_env__$private$.spatialplot(an$results$spatialplot, ggtheme = ggplot2::theme_grey(), theme = list()))
    labels <- unlist(lapply(ggplot2::ggplot_build(ggplot2::last_plot())$data, function(l) l$label))
    expect_false(any(grepl("NA%", labels, fixed = TRUE)))
})

# ═══════════════════════════════════════════════════════════
# Second independent review (2026-09-18): findings S01-S21
# ═══════════════════════════════════════════════════════════

test_that("a 3% offset is never 'material' and is ruled out once the 90% CI fits the margin (S02, S03)", {
    set.seed(21); n <- 150; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w * 1.03 * (1 + rnorm(n, 0, 0.04)), b2 = w * 1.03 * (1 + rnorm(n, 0, 0.04)))
    for (b in c("b1", "b2")) {
        dd <- d[[b]] - w
        ci90 <- (mean(dd) + c(-1, 1) * qt(0.95, n - 1) * sd(dd) / sqrt(n)) / mean(w) * 100
        expect_true(all(abs(ci90) <= 5))     # independent TOST at the 5% margin
        expect_lt(t.test(dd)$p.value, 1e-6)  # yet clearly non-zero: the old rule vetoed on p alone
    }
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET")
})

test_that("the margin is the user's choice (S03)", {
    set.seed(22); n <- 80; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w * 1.07 * (1 + rnorm(n, 0, 0.03)))
    dd <- d$b1 - w
    ci90 <- (mean(dd) + c(-1, 1) * qt(0.95, n - 1) * sd(dd) / sqrt(n)) / mean(w) * 100
    expect_true(min(ci90) > 5 && max(ci90) < 10)
    expect_equal(verdict_of(ihc(d, wholesection = "whole", biopsy1 = "b1")), "NOT ADEQUATE FOR SUBSTITUTION")
    expect_equal(verdict_of(ihc(d, wholesection = "whole", biopsy1 = "b1", bias_margin = 10)), "AGREEMENT THRESHOLDS MET")
})

test_that("two regions reading systematically apart are not called interchangeable (S11)", {
    set.seed(23); n <- 60; t <- runif(n, 20, 80)
    d <- data.frame(b1 = t * 1.27 * (1 + rnorm(n, 0, 0.03)), b2 = t * (1 + rnorm(n, 0, 0.03)))
    expect_gt(mean(d$b1) / mean(d$b2), 1.2)
    res <- ihc(d, biopsy1 = "b1", biopsy2 = "b2", showReportSentences = TRUE)
    expect_equal(verdict_of(res), "NOT ADEQUATE FOR SUBSTITUTION")
    bt <- res$samplingbiastable$asDF
    expect_equal(bt$comparison, "b2 vs b1")
    expect_equal(bt$mean_diff, mean(d$b2 - d$b1), tolerance = 1e-10)
    expect_match(txt(res$report_sentences$content), "should not be used interchangeably", fixed = TRUE)
})

test_that("the summary reports the weakest region and makes no variance claim under a failing verdict (S05, S13)", {
    set.seed(8); n <- 60; w <- runif(n, 10, 90)
    d <- data.frame(whole = w, c1 = w + rnorm(n, 0, 1.5), c4 = w + rnorm(n, 0, 30))
    res <- ihc(d, wholesection = "whole", biopsy1 = "c1", biopsy2 = "c4", correlation_threshold = 0.90, showSummary = TRUE)
    sm <- txt(res$summary$content)
    expect_match(sm, "The weakest region, 'c4'", fixed = TRUE)
    expect_false(grepl("not from which measurement was used", sm, fixed = TRUE))
    under <- ihc(underread(), wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", showSummary = TRUE)
    expect_false(grepl("not from which measurement was used", txt(under$summary$content), fixed = TRUE))
})

test_that("a region that cannot be assessed blocks the green verdict and is named (S06, S14)", {
    set.seed(24); n <- 60; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n, 0, 1), b2 = w + rnorm(n, 0, 1), b3 = 50)
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3")
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET, NOT CONFIRMED")
    expect_match(txt(res$interpretation$content), "Not assessed (too few paired values or the same value in every case): 'b3'", fixed = TRUE)
})

test_that("tied per-case CV spread gives no Brown-Forsythe result (S17)", {
    x <- rep(seq(10, 80, 10), 3)
    d <- data.frame(w = x, b1 = x, b2 = x, comp = factor(rep(c("A", "B", "C"), each = 8)))
    tt <- ihc(d, wholesection = "w", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp", compartmentTests = TRUE)$compartmentTests$asDF
    bf <- tt[grepl("Brown-Forsythe", tt$test_type), ]
    expect_true(is.na(bf$statistic) && is.na(bf$p_value))
    expect_match(bf$interpretation, "no test is possible", fixed = TRUE)
})

test_that("no regions-per-case advice against an unusable reference (S18)", {
    set.seed(5); d <- data.frame(whole = rep(50, 20), b1 = runif(20, 5, 90), b2 = runif(20, 5, 90))
    it <- txt(ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", generate_recommendations = TRUE)$interpretation$content)
    expect_match(it, "Insufficient data for a sampling recommendation.", fixed = TRUE)
    expect_false(grepl("differs from the reference by about", it, fixed = TRUE))
})

test_that("the reference selected again as a region is analysed once, as the reference (S19)", {
    d <- clean(30)
    both <- ihc(d, wholesection = "whole", biopsy1 = "whole", biopsy2 = "b1")
    only <- ihc(d, wholesection = "whole", biopsy1 = "b1")
    expect_match(txt(both$notices$content), "analysed as the reference only", fixed = TRUE)
    expect_equal(both$reproducibilitytable$asDF, only$reproducibilitytable$asDF)
})

test_that("an all-zero marker gets a truthful CV note, no 'robust' label and no empty plot (S21)", {
    d <- data.frame(whole = rep(0, 10), b1 = rep(0, 10), b2 = rep(0, 10))
    o <- ClinicoPath:::ihcheterogeneityOptions$new(wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
                                                  show_variability_plots = TRUE)
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d); an$run()
    tab <- an$results$reproducibilitytable
    expect_match(note_text(tab), "10 case(s) whose mean is 0 were left out", fixed = TRUE)
    df <- tab$asDF
    expect_equal(df$interpretation[df$metric == "Median per-case CV (%)"], "Not estimable")
    expect_false(an$results$variabilityplot$visible)
})

# ═══════════════════════════════════════════════════════════
# Function check (2026-09-19): findings FC1-FC17, RC1-RC14
# ═══════════════════════════════════════════════════════════

margin_data <- function() {
    set.seed(22); n <- 80; w <- runif(n, 20, 80)
    data.frame(whole = w, b1 = w * 1.07 * (1 + rnorm(n, 0, 0.03)), b2 = w * (1 + rnorm(n, 0, 0.05)))
}

test_that("Clinical Impact states the zone at the user's margin with the 90% CI the rule reads (FC2, RC1)", {
    d <- margin_data(); n <- nrow(d)
    diffs <- list(d$b1 - d$whole, d$b2 - d$whole, (d$b1 + d$b2) / 2 - d$whole)
    rel_ci <- function(x, level) (mean(x) + c(-1, 1) * qt(1 - (1 - level) / 2, n - 1) * sd(x) / sqrt(n)) / mean(d$whole) * 100
    for (mg in c(2, 5, 10)) {
        imp <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", bias_margin = mg)$samplingbiastable$asDF$clinical_impact
        for (i in 1:3) {
            ci90 <- rel_ci(diffs[[i]], 0.90)
            ci_adj <- rel_ci(diffs[[i]], 1 - 0.10 / 3)       # Bonferroni over 3 rows
            zone <- if (min(ci_adj) > mg || max(ci_adj) < -mg) "shown to exceed" else
                    if (all(abs(ci90) <= mg)) "ruled out" else "inconclusive"
            expect_match(imp[i], zone, fixed = TRUE)
            expect_match(imp[i], sprintf("90%% CI %.1f%% to %.1f%%", ci90[1], ci90[2]), fixed = TRUE)
            expect_match(imp[i], sprintf("the %s%% margin", mg), fixed = TRUE)
        }
    }
    # a constant reference is not judged; a constant offset keeps its marker
    set.seed(5); dc <- data.frame(whole = rep(40, 20), b1 = runif(20, 5, 50), b2 = runif(20, 5, 50))
    expect_false(any(grepl("margin", ihc(dc, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$samplingbiastable$asDF$clinical_impact)))
    w <- c(12, 18, 25, 31, 36, 44, 50, 57, 63, 70, 22, 41)
    imp <- ihc(data.frame(whole = w, b1 = w - 6, b2 = w - 6), wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$samplingbiastable$asDF$clinical_impact
    expect_match(imp[1], sprintf("%+.1f%%, identical in every case: beyond the 5%% margin", -6 / mean(w) * 100), fixed = TRUE)
})

test_that("the inter-regional design names its comparator, never a reference (FC3, FC9, FC13)", {
    set.seed(9); t0 <- runif(30, 20, 80)
    d <- data.frame(c1 = t0 + rnorm(30), c2 = t0 + rnorm(30), c3 = t0 + rnorm(30))
    three <- ihc(d, biopsy1 = "c1", biopsy2 = "c2", biopsy3 = "c3", showReportSentences = TRUE)
    two <- ihc(d, biopsy1 = "c1", biopsy2 = "c2", showReportSentences = TRUE)
    for (res in list(three, two)) {
        expect_false(grepl("differ from the reference",
                           paste(txt(res$interpretation$content), txt(res$report_sentences$content)), fixed = TRUE))
        expect_equal(res$samplingbiastable$getColumn("mean_diff")$title, "Mean Difference (Region - Comparator)")
    }
    expect_match(txt(three$interpretation$content), "can differ from the mean of the other regions by about this much", fixed = TRUE)
    expect_match(txt(two$report_sentences$content), "can differ from region 'c1' by about this much", fixed = TRUE)
    # the value is the mean over pairs of regions (independent)
    rho <- mean(combn(3, 2, function(p) cor(d[[p[1]]], d[[p[2]]], method = "spearman")))
    expect_match(txt(three$interpretation$content), sprintf("Mean Spearman correlation between regions = %.3f", rho), fixed = TRUE)
    ref <- ihc(clean(), wholesection = "whole", biopsy1 = "b1")
    expect_equal(ref$samplingbiastable$getColumn("mean_diff")$title, "Mean Difference (Region - Reference)")
})

test_that("a measurement in slot 2 starts the analysis; without one only the welcome panel shows (FC4, FC11, RC7)", {
    d <- clean(30)
    res <- ihc(d, wholesection = "whole", biopsy2 = "b1", biopsy3 = "b2")
    expect_false(res$welcome$visible)
    expect_equal(res$reproducibilitytable$asDF,
                 ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$reproducibilitytable$asDF)
    # the R wrapper cannot run with no variable at all, so use the class
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = ClinicoPath:::ihcheterogeneityOptions$new(wholesection = "whole"), data = d)
    an$init()
    r <- an$results
    expect_true(r$welcome$visible)
    expect_false(any(r$reproducibilitytable$visible, r$samplingbiastable$visible, r$variancetable$visible,
                     r$samplesizetable$visible, r$biopsyplot$visible, r$variabilityplot$visible))
    expect_false(grepl("Required", r$welcome$content, fixed = TRUE))
})

test_that("plot styling survives jamovi's theme and compartments keep the level order (FC5, FC6, RC12, RC13)", {
    set.seed(17); n <- 30; w <- runif(n, 30, 70)
    comp <- factor(sample(c("peripheral", "central", "front"), n, TRUE), levels = c("front", "peripheral", "central"))
    d <- data.frame(whole = w, b1 = w * (1 + rnorm(n, 0, 0.05)), b2 = w * (1 + rnorm(n, 0, 0.05)), comp = comp)
    expect_false(identical(unique(as.character(comp)), levels(comp)))     # appearance order differs
    o <- ClinicoPath:::ihcheterogeneityOptions$new(wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp",
                                                  compareCompartments = TRUE, show_variability_plots = TRUE)
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d); an$run()
    expect_equal(an$results$spatialanalysistable$asDF$region, levels(comp))
    expect_equal(unique(an$results$compartmentComparison$asDF$compartment), levels(comp))

    gt <- jmvcore:::getGlobalTheme("default", "jmv")            # the theme jamovi passes
    priv <- an$.__enclos_env__$private
    grDevices::png(tempfile(fileext = ".png")); on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(priv$.biopsyplot(an$results$biopsyplot, ggtheme = gt$ggtheme, theme = gt$theme))
    p <- ggplot2::last_plot()
    expect_equal(c(p$theme$axis.text.x$angle, p$theme$axis.text.x$vjust), c(45, 1))
    expect_equal(p$theme$legend.position, "none")
    expect_equal(p$layers[[2]]$position$height, 0)                  # jitter keeps the measured value
    expect_true(suppressMessages(priv$.spatialplot(an$results$spatialplot, ggtheme = gt$ggtheme, theme = gt$theme)))
    p <- ggplot2::last_plot()
    expect_equal(p$theme$axis.text.x$angle, 45)
    expect_equal(p$scales$get_scales("fill")$name, "Variability Level")   # not replaced by jamovi's palette
    expect_true(isTRUE(p$layers[[1]]$show.legend))
    expect_equal(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels(), levels(comp))
})

test_that("missing and blank compartment IDs are counted, and thresholds print as used (FC7, FC8, RC5)", {
    set.seed(17); n <- 30; w <- runif(n, 30, 70)
    comp <- sample(c("A", "B", "C"), n, TRUE); comp[c(2, 5)] <- NA; comp[c(3, 4)] <- ""
    d <- data.frame(whole = w, b1 = w * (1 + rnorm(n, 0, 0.05)), b2 = w * (1 + rnorm(n, 0, 0.05)), comp = factor(comp))
    res <- expect_no_error(ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp",
                               compareCompartments = TRUE, compartmentTests = TRUE, cv_threshold = 15))
    expect_equal(res$spatialanalysistable$asDF$region, c("A", "B", "C"))
    for (tbl in list(res$spatialanalysistable, res$compartmentComparison, res$compartmentTests))
        expect_match(note_text(tbl), "4 cases without a Spatial Region ID are left out", fixed = TRUE)
    expect_match(note_text(res$spatialanalysistable), "your 15% threshold: Low at or below 7.5%", fixed = TRUE)
})

test_that("a reference name with braces reaches the error message intact (FC10)", {
    d <- data.frame(`ki {core}` = NA_real_, b1 = 1:10 + 0.5, b2 = 1:10, check.names = FALSE)
    expect_error(ihc(d, wholesection = "ki {core}", biopsy1 = "b1", biopsy2 = "b2"),
                 "The reference variable 'ki {core}' has no values", fixed = TRUE)
})

test_that("Statistical Interpretation states this run's variance shares, and nothing when they are not estimable (FC12)", {
    res <- ihc(margin_data(), wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
               generate_recommendations = TRUE, variance_components = TRUE)
    pc <- res$variancetable$asDF$percentage
    expect_match(txt(res$interpretation$content), sprintf(
        "Of the total variance, %.0f%% lies between cases, %.0f%% within cases (sampling and scoring error) and %.0f%% between measurement methods",
        pc[1], pc[2], pc[3]), fixed = TRUE)
    d <- data.frame(whole = c(10, 20, 30, 40, 50, 60), b1 = c(11, NA, 32, NA, 48, 62), b2 = c(NA, 22, NA, 38, NA, 59))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", generate_recommendations = TRUE, variance_components = TRUE)
    expect_false(grepl("Statistical Interpretation", txt(res$interpretation$content), fixed = TRUE))
})

test_that("a compartment without an ICC is shown and explained, not dropped (RC3)", {
    set.seed(4); n <- 20; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n), b2 = w + rnorm(n), comp = factor(rep(c("A", "B", "small"), c(8, 8, 4))))
    d$b1[17] <- NA; d$b2[18] <- NA              # 'small' keeps 2 complete cases
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp", compareCompartments = TRUE)
    cc <- res$compartmentComparison$asDF
    row <- cc[cc$compartment == "small" & grepl("ICC", cc$metric), ]
    expect_equal(nrow(row), 1)
    expect_true(is.na(row$value))
    expect_equal(row$comparison, "Not estimable - see note")
    expect_match(note_text(res$compartmentComparison), "No ICC for compartment 'small'. Only 2 cases have every measurement", fixed = TRUE)
})

test_that("hidden plots get no state, visible ones do (RC8)", {
    d <- clean(30)
    run <- function(...) {
        o <- ClinicoPath:::ihcheterogeneityOptions$new(wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", ...)
        an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d); an$run(); an$results
    }
    hidden <- run(analysis_type = "reproducibility")
    expect_false(hidden$biopsyplot$visible)
    expect_null(hidden$biopsyplot$state)
    expect_null(hidden$variabilityplot$state)
    shown <- run(analysis_type = "reproducibility", show_variability_plots = TRUE)
    expect_false(is.null(shown$biopsyplot$state))
})

test_that("Methods describe only the tests that ran; the small-sample message appears once (RC9, RC10)", {
    set.seed(5); d <- data.frame(whole = rep(40, 20), b1 = runif(20, 5, 50), b2 = runif(20, 5, 50))
    methods <- sub("Results Section:.*", "", txt(ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
                                                     showReportSentences = TRUE)$report_sentences$content))
    expect_false(grepl("paired t-tests", methods, fixed = TRUE))
    expect_false(grepl("Fisher z", methods, fixed = TRUE))
    methods <- sub("Results Section:.*", "", txt(ihc(clean(), wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2",
                                                     showReportSentences = TRUE)$report_sentences$content))
    expect_match(methods, "paired t-tests", fixed = TRUE)
    expect_match(methods, "Fisher z", fixed = TRUE)
    small <- ihc(data.frame(whole = c(10, 20, 30, 40, 50, 60), b1 = c(11, 19, 32, 41, 48, 62), b2 = c(9, 22, 29, 38, 51, 59)),
                 wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_match(txt(small$notices$content), "Small sample", fixed = TRUE)
    expect_false(grepl("Small sample size", txt(small$interpretation$content), fixed = TRUE))
})

test_that("a blank expected width is explained, and p columns share one title (RC11, FC15)", {
    set.seed(1); n <- 40; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = c(w[1:20] + rnorm(20), rep(NA, 20)), b2 = c(rep(NA, 20), w[21:40] + rnorm(20)))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", sample_size_planning = TRUE)
    expect_true(all(is.na(res$samplesizetable$asDF$width_current)))
    expect_match(note_text(res$samplesizetable), "blank because fewer than 2 cases have every measurement", fixed = TRUE)
    expect_equal(res$samplingbiastable$getColumn("p_value")$title, "p-value")
})

# ── check-function-full 2026-09-19: notices, LoA CIs, verdict wording, example ──

test_that("each limit of agreement carries the Bland & Altman (1999) 95% CI", {
    d <- clean()
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    row <- res$samplingbiastable$asDF[1, ]
    dif <- d$b1 - d$whole; n <- length(dif); s <- sd(dif)
    loa <- mean(dif) + c(-1, 1) * 1.96 * s
    half <- qt(0.975, n - 1) * s * sqrt(1 / n + 1.96^2 / (2 * (n - 1)))  # independent of the module
    expect_equal(c(row$loa_lower_lcl, row$loa_lower_ucl), loa[1] + c(-1, 1) * half, tolerance = 1e-10)
    expect_equal(c(row$loa_upper_lcl, row$loa_upper_ucl), loa[2] + c(-1, 1) * half, tolerance = 1e-10)
    expect_match(note_text(res$samplingbiastable), "Bland & Altman (1999)", fixed = TRUE)
})

test_that("data-quality checks are notices, not a box inside the interpretation", {
    d <- clean(); d$b1[1] <- -5
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_match(txt(res$notices$content), "Data quality Negative values detected", fixed = TRUE)
    expect_false(grepl("Data Quality Warnings|Negative values", txt(res$interpretation$content)))
})

test_that("a material difference is also a strong warning, listed before warnings and info", {
    d <- rbind(opposite_bias(), data.frame(whole = NA, b1 = 1, b2 = 2))   # one row dropped -> INFO notice first
    notes <- txt(ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")$notices$content)
    expect_match(notes, "Material systematic difference Region(s) 'b1', 'b2' are offset from the reference", fixed = TRUE)
    expect_lt(regexpr("Material systematic difference", notes, fixed = TRUE),
              regexpr("Cases not analysed", notes, fixed = TRUE))
})

test_that("a hidden variability plot is explained", {
    d <- data.frame(whole = rep(0, 12), b1 = rep(0, 12), b2 = rep(0, 12))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_false(res$variabilityplot$visible)
    expect_match(txt(res$notices$content), "Variability plot not drawn", fixed = TRUE)
})

test_that("the moderate verdict and its relaxed band are labelled a heuristic", {
    glossary <- txt(ihc(clean(), wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", showGlossary = TRUE)$glossary$content)
    expect_match(glossary, "Moderate sampling: The thresholds are met only after relaxing them", fixed = TRUE)
    expect_match(glossary, "heuristic of this analysis, not a published criterion", fixed = TRUE)
})

test_that("the help-page example runs", {
    y <- yaml::read_yaml(test_path("..", "..", "jamovi", "ihcheterogeneity.a.yaml"))
    code <- gsub("package = 'ClinicoPath'", "package = 'ClinicoPath', envir = environment()", y$description$R$usage, fixed = TRUE)
    res <- eval(parse(text = code))
    expect_gt(nrow(res$samplingbiastable$asDF), 0)
})
