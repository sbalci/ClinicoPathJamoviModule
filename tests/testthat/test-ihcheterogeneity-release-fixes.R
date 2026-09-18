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
        "(AGREEMENT THRESHOLDS MET|MODERATE SAMPLING|INADEQUATE SAMPLING|NOT ADEQUATE FOR SUBSTITUTION|INSUFFICIENT DATA)", t))
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

test_that("the Methods paragraph mentions a paired t-test only when there is a reference (I03)", {
    d <- clean()
    inter <- ihc(d, biopsy1 = "b1", biopsy2 = "whole", showReportSentences = TRUE)
    expect_false(grepl("paired t-test", txt(inter$report_sentences$content)))
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

test_that("the regional-reference correlation row carries a Fisher-z CI (I11)", {
    d <- clean(25)
    df <- ihc(d, wholesection = "whole", biopsy1 = "b1")$reproducibilitytable$asDF
    row <- df[df$metric == "Mean Regional-Reference Correlation", ]
    r <- cor(d$whole, d$b1, method = "spearman"); se <- sqrt(1.06 / (25 - 3))
    expect_equal(c(row$ci_lower, row$ci_upper), tanh(atanh(r) + c(-1, 1) * qnorm(0.975) * se), tolerance = 1e-10)
})

# ── E. Sample-size planning (I12, I13) ────────────────────────────────────────

test_that("sample-size planning is ICC precision (Bonett 2002), opt-in, for both designs (I12, I13)", {
    d <- clean(30)
    def <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    expect_false(def$poweranalysistable$visible)

    bonett_n <- function(rho, k, w) ceiling(8 * qnorm(0.975)^2 * (1 - rho)^2 * (1 + (k - 1) * rho)^2 / (k * (k - 1) * w^2) + 1)
    for (with_ref in c(TRUE, FALSE)) {
        res <- if (with_ref) ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", power_analysis = TRUE)
               else ihc(d, biopsy1 = "b1", biopsy2 = "b2", power_analysis = TRUE)
        k <- if (with_ref) 3 else 2
        pt <- res$poweranalysistable$asDF
        expect_true(res$poweranalysistable$visible)
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
    m <- as.matrix(d_lo); floor_v <- 0.02 * max(abs(m))
    case_cv <- ifelse(apply(m, 1, sd) == 0, 0,
                      ifelse(rowMeans(m) < floor_v, NA, apply(m, 1, sd) / rowMeans(m) * 100))
    expect_equal(cv_mean, mean(case_cv, na.rm = TRUE), tolerance = 1e-10)
    expect_lt(cv_mean, 10)
    expect_match(note_text(tab), "4 case", fixed = TRUE)
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
