# Regression tests for defects fixed in the 2026-09 pathagreement review.
# Each test targets a defect that was reproduced against the pre-fix code.

pa_ratings <- function(n = 60, raters = 3, seed = 1,
                       levels = c("Benign", "Atypical", "Malignant"),
                       ordered = TRUE, noise = 0.25) {
    set.seed(seed)
    truth <- sample(levels, n, TRUE)
    cols <- lapply(seq_len(raters), function(i) {
        factor(ifelse(stats::runif(n) < noise, sample(levels, n, TRUE), truth),
               levels = levels, ordered = ordered)
    })
    stats::setNames(as.data.frame(cols), paste0("r", seq_len(raters)))
}

# do.call passes VALUES; a bare `vars = v` would be captured as the name "v".
pa_run <- function(data, ...) do.call(pathagreement, c(list(data = data), list(...)))

pa_private <- function(data, ...) {
    cls <- getFromNamespace("pathagreementClass", "ClinicoPath")
    opts <- getFromNamespace("pathagreementOptions", "ClinicoPath")
    a <- cls$new(options = do.call(opts$new, list(...)), data = data)
    a$run()
    list(analysis = a, private = a$.__enclos_env__$private)
}

test_that("clustering runs at the default group count for 3 raters", {
    # nStyleGroups defaults to 3; k == n made cluster::silhouette() return NA
    expect_no_error(pa_run(pa_ratings(raters = 3), vars = paste0("r", 1:3), performClustering = TRUE))
})

test_that("clustering with 2 raters is skipped with a message instead of crashing", {
    r <- pa_run(pa_ratings(raters = 2), vars = c("r1", "r2"), performClustering = TRUE)
    expect_match(r$warnings$content, "at least 3 raters")
})

test_that("a disagreement is not counted as agreement when level orders differ", {
    # r1 = Benign, r2 = EIN on every case; the factors list their levels in
    # opposite orders, so both carry level code 1.
    d <- data.frame(
        r1 = factor(rep(c("Benign", "EIN"), each = 10), levels = c("Benign", "EIN")),
        r2 = factor(rep(c("EIN", "Benign"), each = 10), levels = c("EIN", "Benign"))
    )
    r <- pa_run(d, vars = c("r1", "r2"))
    expect_equal(r$overviewTable$asDF$overall_agreement, 0)
})

test_that("global_mode tie-breaking resolves ties that miss the majority", {
    lv <- c("Benign", "Atypical", "Malignant")
    set.seed(9)
    A <- sample(lv, 60, TRUE)
    B <- sample(lv, 60, TRUE)
    d <- data.frame(r1 = factor(A, lv), r2 = factor(A, lv), r3 = factor(B, lv), r4 = factor(B, lv))
    resolved <- function(tb) {
        r <- pa_run(d, vars = paste0("r", 1:4), consensus = TRUE, tie_breaking = tb)
        df <- r$consensusSummary$asDF
        v <- df$value[df$metric == "Ties resolved by overall most common rating"]
        if (length(v)) as.numeric(v) else 0
    }
    expect_gt(resolved("global_mode"), 0)
    expect_equal(resolved("exclude"), 0)
})

test_that("ordinal Krippendorff's alpha and Gwet's AC2 use the factor level order", {
    lv <- c("Benign", "Atypical", "Malignant")
    r1 <- c(rep("Benign", 20), rep("Atypical", 10), rep("Malignant", 10))
    r2 <- c(rep("Atypical", 10), rep("Malignant", 10), rep("Atypical", 10), rep("Malignant", 10))
    d <- data.frame(r1 = factor(r1, lv, ordered = TRUE), r2 = factor(r2, lv, ordered = TRUE))

    r <- pa_run(d, vars = c("r1", "r2"), kripp = TRUE, krippMethod = "ordinal", gwetAC = TRUE)
    # canonical ordinal Krippendorff: irr on codes in level order (see the published-example test)
    expected_alpha <- irr::kripp.alpha(rbind(as.integer(d$r1), as.integer(d$r2)), method = "ordinal")$value
    expected_ac2 <- irrCAC::gwet.ac1.raw(d, weights = "ordinal", categ.labels = lv)$est$coeff.val

    expect_equal(r$krippTable$asDF$alpha, expected_alpha, tolerance = 1e-4)
    gw <- r$gwetACTable$asDF
    expect_equal(gw$value[grepl("AC2", gw$coefficient)], expected_ac2, tolerance = 1e-4)
})

test_that("Krippendorff's alpha reproduces Krippendorff's (2011) published worked example", {
    # 4 observers x 12 units with missing data. irrCAC gives .834 / .800 for
    # ordinal / interval, which is why the module computes alpha with irr.
    obs <- rbind(
        c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
        c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, 3),
        c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, NA),
        c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA)
    )
    dm <- as.data.frame(lapply(seq_len(nrow(obs)), function(i) factor(obs[i, ], levels = 1:5)))
    helper <- pa_private(pa_ratings(), vars = paste0("r", 1:3))$private$.krippendorffAlpha
    published <- c(nominal = 0.743, ordinal = 0.815, interval = 0.849, ratio = 0.797)
    for (level in names(published)) {
        expect_equal(round(helper(level, dm)$alpha, 3), published[[level]], label = level)
    }
})

test_that("Krippendorff bootstrap produces a real interval", {
    r <- pa_run(pa_ratings(n = 80), vars = paste0("r", 1:3), kripp = TRUE,
                bootstrap = TRUE, bootstrapSamples = 200)
    k <- r$krippTable$asDF
    expect_true(is.finite(k$ci_lower) && is.finite(k$ci_upper))
    expect_lt(k$ci_lower, k$alpha)
    expect_gt(k$ci_upper, k$alpha)
})

test_that("trend, bias and difficulty plots show the analysed data", {
    d <- pa_ratings(n = 60, raters = 7, noise = 0.05)
    p <- pa_private(d, vars = paste0("r", 1:7), raterBiasAnalysis = TRUE,
                    agreementTrendAnalysis = TRUE, caseDifficultyScoring = TRUE)
    th <- ggplot2::theme_grey()

    bias <- p$private$.createBiasVisualization(th)$data
    tbl <- p$analysis$results$raterBiasTable$asDF
    expect_equal(as.character(bias$rater), paste0("r", 1:7))
    expect_equal(unname(bias$bias), unname(tbl$bias_score), tolerance = 1e-8)

    trend <- p$private$.createTrendVisualization(th)$data
    expect_equal(unname(trend$agreement),
                 unname(as.numeric(p$analysis$results$agreementTrendTable$asDF$agreement_percent)),
                 tolerance = 1e-8)

    # runif() made this differ on every render
    diff1 <- p$private$.createDifficultyVisualization(th)$data
    diff2 <- p$private$.createDifficultyVisualization(th)$data
    expect_identical(diff1, diff2)
    expect_equal(nrow(diff1), 60)
})

test_that("META_ rows supply rater characteristics without leaking into categories", {
    set.seed(3)
    lv <- c("Benign", "EIN", "Adenocarcinoma")
    cases <- as.data.frame(lapply(1:5, function(i) sample(lv, 40, TRUE)), stringsAsFactors = FALSE)
    names(cases) <- paste0("r", 1:5)
    meta <- data.frame(t(c("5", "10", "15", "20", "25")), stringsAsFactors = FALSE)
    names(meta) <- paste0("r", 1:5)
    d <- rbind(cases, meta)
    d$case_id <- c(paste0("C", 1:40), "META_experience")
    d <- as.data.frame(lapply(d, factor))   # as a CSV read with stringsAsFactors = TRUE

    expect_no_error(r <- pa_run(d, vars = paste0("r", 1:5), caseID = "case_id", useMetadataRows = TRUE,
                                performClustering = TRUE, raterCharacteristics = TRUE, nStyleGroups = 2,
                                categoryAnalysis = TRUE))
    expect_false(any(c("5", "10", "15", "20", "25") %in% r$categoryTable$asDF$category))
    expect_equal(r$diagnosticStyleTable$asDF$experience, c("5", "10", "15", "20", "25"))
})

test_that("reject() fills its placeholder", {
    # The R wrapper coerces vars with as.factor(), so a non-factor column only
    # reaches .validateData() through the class itself.
    cls <- getFromNamespace("pathagreementClass", "ClinicoPath")
    opts <- getFromNamespace("pathagreementOptions", "ClinicoPath")
    d <- data.frame(r1 = 1:10, r2 = factor(rep(c("a", "b"), 5)))
    a <- cls$new(options = opts$new(vars = c("r1", "r2")), data = d)
    expect_error(a$.__enclos_env__$private$.validateData(), "not factors: r1")
})

test_that("sensitivity/specificity show when a reference standard is given", {
    d <- pa_ratings()
    d$gold <- d$r1
    r <- pa_run(d, vars = paste0("r", 1:3), categoryAnalysis = TRUE, referenceStandard = "gold")
    expect_true(all(c("sensitivity", "specificity") %in% names(r$categoryTable$asDF)))
})

test_that("a 2-1-1 split among 4 raters is not a majority", {
    lv <- c("Benign", "EIN", "Carcinoma")
    d <- data.frame(r1 = factor(rep("Benign", 20), lv), r2 = factor(rep("Benign", 20), lv),
                    r3 = factor(rep("EIN", 20), lv), r4 = factor(rep("Carcinoma", 20), lv))
    d[1:3, ] <- lapply(d, function(x) factor(rep("EIN", 3), lv))
    r <- pa_run(d, vars = paste0("r", 1:4), consensus = TRUE, show_consensus_table = TRUE)
    expect_false(any(r$consensusTable$asDF$agreement_level == "Majority"))
    s <- r$consensusSummary$asDF
    expect_equal(as.numeric(s$value[s$metric == "Consensus Achieved"]), 3)
})

test_that("ICC uses factor-level codes and is actually reported", {
    lv <- c("Benign", "Atypical", "Malignant")
    set.seed(21)
    n <- 60
    truth <- sample(1:3, n, TRUE)
    jit <- function(p) pmin(3, pmax(1, truth + sample(c(-1, 0, 1), n, TRUE, prob = c(p / 2, 1 - p, p / 2))))
    d <- data.frame(r1 = factor(lv[jit(.2)], lv, ordered = TRUE),
                    r2 = factor(lv[jit(.3)], lv, ordered = TRUE),
                    r3 = factor(lv[jit(.4)], lv, ordered = TRUE))
    r <- pa_run(d, vars = paste0("r", 1:3), icc = TRUE)
    res <- psych::ICC(sapply(d, as.integer), lmer = FALSE)$results
    expect_equal(r$iccTable$asDF$icc_value, res$ICC[res$type == "ICC2"], tolerance = 1e-6)
})

test_that("ICC is not calculated for nominal ratings", {
    r <- pa_run(pa_ratings(ordered = FALSE), vars = paste0("r", 1:3), icc = TRUE)
    expect_equal(r$iccTable$rowCount, 0)
})

test_that("sample size planning matches kappaSize", {
    d <- pa_ratings(n = 100, raters = 3, levels = c("Negative", "Positive"))
    r <- pa_run(d, vars = paste0("r", 1:3), sampleSizePlanning = TRUE, targetKappa = 0.8, targetPrecision = 0.1)
    s <- r$sampleSizeTable$asDF
    prevalence <- mean(unlist(lapply(d, as.character)) == "Negative")
    expected <- kappaSize::CIBinary(kappa0 = 0.8, kappaL = 0.7, kappaU = 0.9, props = prevalence, raters = 3, alpha = 0.05)$n
    expect_equal(s$value[s$parameter == "3 raters"], paste(expected, "cases"))
})

test_that("one dissent among 10 raters is not labelled difficult", {
    base <- c(rep("Benign", 10), rep("EIN", 10))
    d <- as.data.frame(stats::setNames(lapply(1:10, function(i) {
        factor(if (i == 10) rev(base) else base, c("Benign", "EIN"))
    }), paste0("p", 1:10)))
    r <- pa_run(d, vars = paste0("p", 1:10), caseDifficultyScoring = TRUE)
    expect_equal(unique(r$caseDifficultyTable$asDF$difficulty_level), "Easy (high consensus)")
})

test_that("rater disagreement does not depend on rater or label order", {
    # the pair agrees on cases 1-10 and splits Benign/EIN on 11-20; each rater
    # disagrees with the other on half the cases
    lv <- c("Benign", "EIN")
    d <- data.frame(a = factor(c(rep(c("Benign", "EIN"), 5), rep("Benign", 10)), lv),
                    b = factor(c(rep(c("Benign", "EIN"), 5), rep("EIN", 10)), lv))
    r <- pa_run(d, vars = c("a", "b"), raterBiasAnalysis = TRUE)
    expect_equal(r$raterBiasTable$asDF$bias_score, c(0.5, 0.5))
})

test_that("bootstrap intervals are reproducible with a seed", {
    d <- pa_ratings(n = 60)
    ci <- function() {
        k <- pa_run(d, vars = paste0("r", 1:3), kripp = TRUE, bootstrap = TRUE,
                    bootstrapSamples = 100, seed = 7)$krippTable$asDF
        c(k$ci_lower, k$ci_upper)
    }
    expect_identical(ci(), ci())
})

test_that("arbitration flags tied cases in the consensus summary", {
    lv <- c("Benign", "Atypical", "Malignant")
    set.seed(9)
    A <- sample(lv, 60, TRUE)
    B <- sample(lv, 60, TRUE)
    d <- data.frame(r1 = factor(A, lv), r2 = factor(A, lv), r3 = factor(B, lv), r4 = factor(B, lv))
    s <- pa_run(d, vars = paste0("r", 1:4), consensus = TRUE, tie_breaking = "arbitration")$consensusSummary$asDF
    expect_true("Ties needing arbitration" %in% s$metric)
})

test_that("the exact option is labelled Conger's kappa and uses Conger's SE", {
    d <- pa_ratings(n = 80, raters = 4)
    k <- pa_run(d, vars = paste0("r", 1:4), exct = TRUE, fleissCI = TRUE)$kappaTable$asDF
    expect_match(k$method, "Conger")
    expect_equal(k$kappa, irr::kappam.fleiss(d, exact = TRUE)$value, tolerance = 1e-6)
    expect_equal(k$se, irrCAC::conger.kappa.raw(d)$est$coeff.se, tolerance = 1e-3)
})

test_that("Gwet's AC2 is not described as accounting for rater heterogeneity", {
    g <- pa_run(pa_ratings(), vars = paste0("r", 1:3), gwetAC = TRUE)$gwetACTable$asDF
    expect_false(any(grepl("heterogeneity", g$interpretation)))
})

test_that("a categorical rater characteristic is tested, reproducibly", {
    set.seed(5)
    lv <- c("Benign", "EIN", "Adenocarcinoma")
    cases <- as.data.frame(lapply(1:8, function(i) sample(lv, 40, TRUE)), stringsAsFactors = FALSE)
    names(cases) <- paste0("r", 1:8)
    meta <- as.data.frame(t(c("A", "A", "B", "B", "C", "C", "A", "B")), stringsAsFactors = FALSE)
    names(meta) <- paste0("r", 1:8)
    d <- rbind(cases, meta)
    d$case_id <- c(paste0("C", 1:40), "META_institution")
    d <- as.data.frame(lapply(d, factor))
    run <- function() pa_run(d, vars = paste0("r", 1:8), caseID = "case_id", useMetadataRows = TRUE,
                             performClustering = TRUE, raterCharacteristics = TRUE, nStyleGroups = 2,
                             seed = 11)$characteristicAssociations$asDF
    first <- run()
    expect_true("Institution" %in% first$characteristic)
    expect_identical(first$p_value, run()$p_value)
})

test_that("stability kappa covers all raters", {
    d <- pa_ratings(n = 60, raters = 4)
    st <- pa_run(d, vars = paste0("r", 1:4), agreementStabilityAnalysis = TRUE, bootstrapSamples = 100)$stabilityTable$asDF
    expect_equal(st$original_value[st$statistic == "Fleiss' kappa"], irr::kappam.fleiss(d)$value, tolerance = 1e-6)
    expect_false(any(grepl("stable", st$interpretation)))
})

test_that("clinical summary and report are generated without clinical-suitability claims", {
    # Table$getCell() returns a Cell object; reading it without $value meant the
    # summary was never generated and the report never carried its CI.
    r <- pa_run(pa_ratings(n = 60, raters = 2), vars = c("r1", "r2"), showClinicalSummary = TRUE)
    summary_html <- r$clinicalSummary$content
    report_html <- r$reportTemplate$content
    expect_false(grepl("could not be generated", summary_html))
    expect_false(grepl("suitable for all clinical applications", paste(summary_html, report_html)))
    expect_match(paste(summary_html, report_html), "Landis and Koch")
    expect_match(report_html, "95% CI")
})

test_that("the report sentence names the estimate and raters it actually reports", {
    d <- pa_ratings(n = 60, raters = 3)
    # Krippendorff's alpha is not kappa
    rk <- pa_run(d, vars = paste0("r", 1:3), multiraterMethod = "krippendorff", krippMethod = "ordinal",
                 showClinicalSummary = TRUE)$reportTemplate$content
    expect_false(grepl("κ =", rk))
    expect_match(rk, "Krippendorff")
    # forced pairwise Cohen: the sentence describes one pair, not all 3 raters
    rp <- pa_run(d, vars = paste0("r", 1:3), multiraterMethod = "cohen", showClinicalSummary = TRUE)$reportTemplate$content
    expect_false(grepl("for the 3 raters", rp))
    expect_match(rp, "r1 vs r2")
})
