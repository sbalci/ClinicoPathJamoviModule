# ═══════════════════════════════════════════════════════════
# Release-review regression tests: ihcheterogeneity
# ═══════════════════════════════════════════════════════════
#
# Each test pins a defect confirmed during the release review. Statistics are
# checked against psych / irr / stats rather than against the module's own
# arithmetic.

# 25 cases where every region under-reads the whole section by 30%: a true Ki67
# of 60% scored as 42%. Correlation and consistency-ICC are near-perfect here;
# only absolute agreement and the bias test can see the problem.
biased_data <- function() {
    set.seed(42); n <- 25
    whole <- round(runif(n, 20, 80), 1)
    data.frame(whole = whole,
               b1 = 0.70 * whole + rnorm(n, 0, 0.5),
               b2 = 0.70 * whole + rnorm(n, 0, 0.5))
}

clean_data <- function() {
    set.seed(42); n <- 25
    whole <- round(runif(n, 20, 80), 1)
    data.frame(whole = whole,
               b1 = whole + rnorm(n, 0, 2),
               b2 = whole + rnorm(n, 0, 2))
}

run_ihc <- function(d, ...) {
    ClinicoPath::ihcheterogeneity(data = d, wholesection = "whole",
                                  biopsy1 = "b1", biopsy2 = "b2", ...)
}


test_that("the headline ICC is absolute agreement, not the bias-blind consistency form", {
    d <- biased_data()
    m <- as.matrix(d)
    tab <- run_ihc(d)$reproducibilitytable$asDF

    agreement  <- psych::ICC(m, lmer = FALSE)$results$ICC[2]   # ICC(2,1) / ICC(A,1)
    consistency <- psych::ICC(m, lmer = FALSE)$results$ICC[3]   # ICC(3,1) / ICC(C,1)

    # cross-check the reference values against a second package
    expect_equal(agreement,
                 irr::icc(m, model = "twoway", type = "agreement",  unit = "single")$value,
                 tolerance = 1e-6)
    expect_equal(consistency,
                 irr::icc(m, model = "twoway", type = "consistency", unit = "single")$value,
                 tolerance = 1e-6)

    headline <- tab[grepl("absolute agreement", tab$metric), ]
    expect_equal(nrow(headline), 1)
    expect_equal(headline$value[1], agreement, tolerance = 1e-6)

    # The consistency form is still reported, but separately and labelled.
    cons_row <- tab[grepl("consistency", tab$metric), ]
    expect_equal(nrow(cons_row), 1)
    expect_equal(cons_row$value[1], consistency, tolerance = 1e-6)

    # The whole point: on this data the two differ substantially, and the
    # headline verdict must follow the one that can see the 30% under-read.
    expect_gt(consistency - agreement, 0.2)
    expect_equal(headline$interpretation[1], "Moderate reliability")
})

test_that("a mean correlation is never labelled as an ICC", {
    # Two measurements with a constant column makes the ICC inestimable, so
    # .calculateICC falls back to the mean Spearman correlation. That value used
    # to be printed under "ICC(3,1)" and graded on ICC reliability cut-offs.
    d <- data.frame(whole = c(10, 20, 30, 40, 50),
                    b1    = c(11, 21, 31, 41, 51),
                    b2    = rep(25, 5))                # zero variance
    tab <- run_ihc(d)$reproducibilitytable$asDF

    icc_rows <- tab[grepl("^ICC", tab$metric), ]
    if (nrow(icc_rows) > 0) {
        # if anything is called an ICC it must be the absolute-agreement row
        expect_true(all(grepl("absolute agreement|consistency", icc_rows$metric)))
    }
    fallback <- tab[grepl("ICC not estimable", tab$metric), ]
    if (nrow(fallback) > 0) {
        expect_equal(fallback$interpretation[1], "Not an ICC - see note")
    }
})

test_that("variance components sum to the total variance", {
    set.seed(42); n <- 20
    truth <- rnorm(n, 40, 15)
    d <- data.frame(whole = truth + rnorm(n, 0, 2), b1 = truth + rnorm(n, 0, 6),
                    b2 = truth + rnorm(n, 0, 6), b3 = truth + rnorm(n, 0, 6))

    vt <- ClinicoPath::ihcheterogeneity(data = d, wholesection = "whole",
            biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3",
            variance_components = TRUE)$variancetable$asDF

    # The previous implementation reported three non-orthogonal variances each
    # divided by a pooled total: a component read 102.3% of total and the
    # percentages summed to 107.5%, under a row labelled "Sum of all components".
    expect_equal(sum(vt$variance[1:3]), vt$variance[4], tolerance = 1e-8)
    expect_equal(sum(vt$percentage[1:3]), 100, tolerance = 1e-6)
    expect_true(all(vt$percentage[1:3] <= 100 + 1e-8))

    # and they match an independent two-way random-effects decomposition
    long <- data.frame(case = factor(rep(seq_len(n), 4)),
                       method = factor(rep(c("Reference","b1","b2","b3"), each = n)),
                       value = c(d$whole, d$b1, d$b2, d$b3))
    a  <- summary(stats::aov(value ~ case + method, data = long))[[1]]
    ms <- a[["Mean Sq"]]; rn <- trimws(rownames(a))
    mse <- ms[match("Residuals", rn)]
    expect_equal(vt$variance[1], (ms[match("case", rn)]   - mse) / 4, tolerance = 1e-6)
    expect_equal(vt$variance[2], mse, tolerance = 1e-6)
    expect_equal(vt$variance[3], max((ms[match("method", rn)] - mse) / n, 0), tolerance = 1e-6)
})

test_that("one CV definition is used by the table and the narrative", {
    d <- biased_data()
    res <- run_ihc(d, generate_recommendations = TRUE)
    tab <- res$reproducibilitytable$asDF

    cv_row <- tab[grepl("Coefficient of Variation", tab$metric), ]
    expect_equal(nrow(cv_row), 1)

    txt <- gsub("<[^>]+>", " ", res$interpretation$content)
    prose_cv <- as.numeric(sub(".*Mean CV = ([0-9.]+)%.*", "\\1", txt))

    # The table used regional columns only while the narrative folded in the
    # reference, so the two disagreed on the same screen (23.19 vs 20.0).
    expect_equal(round(cv_row$value[1], 1), prose_cv, tolerance = 0.051)

    # With a reference present the CV must include it, otherwise a systematic
    # under-read is invisible (it showed as 1.2% variability).
    expect_gt(cv_row$value[1], 10)
})

test_that("a material systematic bias vetoes the adequacy verdict", {
    verdict <- function(d) {
        txt <- gsub("<[^>]+>", " ", run_ihc(d)$interpretation$content)
        regmatches(txt, regexpr(
          "(AGREEMENT THRESHOLDS MET|MODERATE SAMPLING|INADEQUATE SAMPLING|NOT ADEQUATE FOR SUBSTITUTION|INSUFFICIENT DATA)", txt))
    }
    # Correlation and CV thresholds alone declared this "ADEQUATE SAMPLING ...
    # suitable for clinical use" while the bias table reported p < 1e-13.
    expect_equal(verdict(biased_data()), "NOT ADEQUATE FOR SUBSTITUTION")
    expect_equal(verdict(clean_data()),  "AGREEMENT THRESHOLDS MET")
})

test_that("copy-ready text makes no claim the analysis did not support", {
    for (d in list(biased_data(), clean_data())) {
        # the panel is opt-in now; without the option the content is empty and
        # every expect_false below would pass vacuously
        txt <- gsub("<[^>]+>", " ", run_ihc(d, showReportSentences = TRUE)$report_sentences$content)
        expect_true(nzchar(trimws(txt)))
        # a blanket endorsement used to close EVERY report, including ones that
        # had just called the sampling inadequate
        expect_false(grepl("results support the use of biopsy simulation", txt))
        # and the Methods paragraph asserted a study design and a citation
        expect_false(grepl("simulated core biopsy", txt))
        expect_false(grepl("Zilenaite", txt))
    }
})

test_that("power analysis uses the number of complete pairs", {
    set.seed(1); n <- 40
    whole <- rnorm(n, 50, 10)
    d <- data.frame(whole = whole, b1 = whole + rnorm(n, 0, 5), b2 = whole + rnorm(n, 0, 5))
    d$whole[1:20] <- NA          # half the references missing

    pt <- run_ihc(d, power_analysis = TRUE)$poweranalysistable$asDF
    small <- pt[pt$scenario == "Small Effect (r=0.1)", ]

    # n must be the 20 complete pairs, not the 40 rows
    z <- 0.5 * log(1.1 / 0.9)
    expect_equal(small$power[1],
                 pnorm(z / (1/sqrt(20 - 3)) - qnorm(0.975)) +
                 pnorm(-z / (1/sqrt(20 - 3)) - qnorm(0.975)),
                 tolerance = 1e-6)

    # observed-effect power must not be sold as evidence of adequacy
    obs <- pt[pt$scenario == "Observed Effect Size", ]
    expect_match(obs$recommendation[1], "not evidence of adequacy")
})

test_that("spatial heterogeneity measures within-case, not between-patient, spread", {
    set.seed(7)
    a <- c(10, 30, 50, 70, 90)     # wide between-patient range, internally consistent
    b <- rep(50, 5)                # narrow range, internally variable
    d <- data.frame(
        whole = c(a, b),
        b1 = c(a + rnorm(5, 0, 0.5), b + c(-20, 15, -18, 22, -16)),
        b2 = c(a + rnorm(5, 0, 0.5), b + c(18, -17, 20, -19, 16)),
        comp = factor(rep(c("A", "B"), each = 5)))

    st <- ClinicoPath::ihcheterogeneity(data = d, wholesection = "whole",
            biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp")$spatialanalysistable$asDF

    # Pooling every case's measurements made A (wide patient range) look the most
    # heterogeneous, inverting the true ranking.
    expect_equal(st$heterogeneity_level[st$region == "A"], "Low")
    expect_equal(st$heterogeneity_level[st$region == "B"], "High")
    expect_lt(st$cv_percent[st$region == "A"], st$cv_percent[st$region == "B"])
})

test_that("a constant offset between region and reference does not abort the analysis", {
    # zero-variance difference vector -> t.test(paired=TRUE) errors with
    # "data are essentially constant"; it was unguarded in four places.
    d <- data.frame(whole = c(10, 20, 30, 40, 50, 60),
                    b1    = c(15, 25, 35, 45, 55, 65),
                    b2    = c(15, 25, 35, 45, 55, 65))
    res <- expect_no_error(run_ihc(d))
    bt <- res$samplingbiastable$asDF
    expect_true(all(abs(bt$mean_diff - 5) < 1e-8))
})

test_that("Levene's test for compartment differences actually reports a result", {
    set.seed(3); n <- 12
    whole <- rnorm(n, 50, 10)
    d <- data.frame(whole = whole,
                    b1 = whole + rnorm(n, 0, 4), b2 = whole + rnorm(n, 0, 4),
                    comp = factor(rep(c("A", "B", "C"), each = 4)))

    res <- ClinicoPath::ihcheterogeneity(data = d, wholesection = "whole",
            biopsy1 = "b1", biopsy2 = "b2", spatial_id = "comp",
            compareCompartments = TRUE, compartmentTests = TRUE)

    tt <- res$compartmentTests$asDF
    lev <- tt[grepl("Levene", tt$test, ignore.case = TRUE), ]
    if (nrow(lev) > 0) {
        # oneway.test()$parameter is c(num df, denom df); passing the length-2
        # vector made addRow() throw inside a tryCatch, so this row ALWAYS said
        # "Could not compute".
        expect_false(is.na(lev$df[1]))
        expect_false(grepl("Could not compute", lev$interpretation[1]))
    }
})

# ── Check-pass regressions (2026-08-23) ──────────────────────────

test_that("tables do not duplicate rows when .run() re-executes on one instance", {
  d <- read.csv("../../data/ihc_heterogeneity.csv")
  options <- ClinicoPath:::ihcheterogeneityOptions$new(
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1", biopsy2 = "ki67_region2")
  analysis <- ClinicoPath:::ihcheterogeneityClass$new(options = options, data = d)
  analysis$run()
  n1 <- analysis$results$reproducibilitytable$rowCount
  b1 <- analysis$results$samplingbiastable$rowCount
  analysis$run()   # jamovi re-runs without clearWith on data-cell edits
  expect_equal(analysis$results$reproducibilitytable$rowCount, n1)
  expect_equal(analysis$results$samplingbiastable$rowCount, b1)
})

test_that("compartment Kruskal-Wallis uses one per-case summary value, not pooled measurements", {
  skip_if_not_installed("psych")
  d <- read.csv("../../data/ihc_heterogeneity.csv")
  res <- ihcheterogeneity(
    data = d, wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1", biopsy2 = "ki67_region2",
    spatial_id = "spatial_region",
    compareCompartments = TRUE, compartmentTests = TRUE)
  tests <- res$compartmentTests$asDF
  kw <- tests[grep("Kruskal", tests$test_type), , drop = FALSE]
  expect_equal(nrow(kw), 1)

  # independent reference: per-case mean of (reference + regions), one value
  # per case, compared across compartments. Mirror the module's rule that a
  # compartment needs at least 2 cases to enter the comparison.
  case_means <- rowMeans(cbind(d$ki67_wholesection, d$ki67_region1, d$ki67_region2),
                         na.rm = TRUE)
  ok <- !is.na(d$spatial_region) & is.finite(case_means)
  grp <- d$spatial_region[ok]
  keep <- grp %in% names(which(table(grp) >= 2))
  ref <- kruskal.test(case_means[ok][keep] ~ factor(grp[keep]))
  expect_equal(unname(kw$statistic), unname(ref$statistic), tolerance = 1e-6)
  expect_equal(kw$p_value, ref$p.value, tolerance = 1e-8)
})


test_that("report sentences and assumptions panels are opt-in", {
  d <- clean_data()
  off <- run_ihc(d)
  expect_false(off$report_sentences$visible)
  expect_false(off$assumptions$visible)
  expect_equal(off$report_sentences$content, "")
  expect_equal(off$assumptions$content, "")

  on <- run_ihc(d, showReportSentences = TRUE, showAssumptions = TRUE)
  expect_true(on$report_sentences$visible)
  expect_true(on$assumptions$visible)
  expect_true(nzchar(on$report_sentences$content))
  expect_true(nzchar(on$assumptions$content))
})

test_that("the removed 'bias' focus level errors clearly", {
  expect_error(run_ihc(clean_data(), analysis_type = "bias"))
})

test_that("every panel grades the mean CV with the same user-threshold bands", {
  d <- read.csv("../../data/ihc_heterogeneity.csv")
  grade_word <- function(x) regmatches(x, regexpr("Low|Moderate|High", x))
  for (thr in c(5, 20, 50)) {
    res <- ihcheterogeneity(
      data = d, wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1", biopsy2 = "ki67_region2",
      cv_threshold = thr, showSummary = TRUE)
    tab <- res$reproducibilitytable$asDF
    tab_word <- grade_word(tab$interpretation[grepl("Coefficient of Variation", tab$metric)])
    interp <- gsub("<[^>]+>", " ", res$interpretation$content)
    key_word <- grade_word(regmatches(interp, regexpr("Mean CV = [0-9.]+% \\([A-Za-z]+ variability", interp)))
    summ <- gsub("<[^>]+>", " ", res$summary$content)
    summ_word <- grade_word(regmatches(summ, regexpr("Variability: *[A-Za-z]+", summ)))
    expect_equal(key_word, tab_word, info = paste("Key Findings vs table at thr", thr))
    expect_equal(summ_word, tab_word, info = paste("Summary vs table at thr", thr))
  }
})

test_that("nominal-integer columns (jamovi factors with a values attribute) are analysed as numbers", {
    # jamovi passes an integer column with a nominal measure type as a factor
    # carrying a 'values' attribute; permitted: numeric admits it. The old code
    # did no coercion and aborted with "non-numeric argument to binary operator".
    d <- clean_data(); d[] <- lapply(d, round)
    as_jamovi_nominal <- function(x) { f <- factor(x); attr(f, "values") <- as.integer(levels(f)); f }
    dj <- data.frame(lapply(d, as_jamovi_nominal))
    num <- run_ihc(d); fac <- run_ihc(dj)
    expect_equal(fac$reproducibilitytable$asDF, num$reproducibilitytable$asDF)
    expect_equal(fac$samplingbiastable$asDF, num$samplingbiastable$asDF)
})

test_that("the multiplicity note counts the comparisons actually shown", {
    tab <- run_ihc(clean_data())$samplingbiastable
    expect_equal(tab$rowCount, 3L)   # b1 vs ref, b2 vs ref, mean of regions vs ref
    expect_match(tab$notes$multiplicity$note, "^3 paired comparisons")
})

test_that("the variability plot does not error when no case has two values to compare", {
    # Image$.render() reports TRUE whatever the renderer does, so call the
    # renderer itself: it used to die on max(numeric(0)) and a 0-row frame.
    d <- data.frame(b1 = c(10, 20, 30, 40, 50, 60), b2 = NA_real_)
    o <- ClinicoPath:::ihcheterogeneityOptions$new(biopsy1 = "b1", biopsy2 = "b2",
                                                  show_variability_plots = TRUE)
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d); an$run()
    grDevices::png(tempfile(fileext = ".png")); on.exit(grDevices::dev.off(), add = TRUE)
    expect_false(an$.__enclos_env__$private$.variabilityplot(
        an$results$variabilityplot, ggtheme = ggplot2::theme_minimal(), theme = list()))
})
