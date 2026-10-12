# Release review 2026-09-25, "panels" pass: Bland-Altman, hierarchical and
# mixed-effects models, bootstrap floor, paired comparison, guides and the
# continuous-data panel. Every expected value comes from an independent oracle
# (lme4 fits, t.test, base-R counts, the published formula), never the analysis.

# 0/1 calls from three raters in four institutions; institution D and six other
# cases have no rating at all.
panels_binary_clustered <- function() {
    set.seed(1)
    n <- 40
    truth <- rbinom(n, 1, 0.5)
    d <- data.frame(
        R1 = ifelse(runif(n) < 0.8, truth, 1 - truth),
        R2 = ifelse(runif(n) < 0.8, truth, 1 - truth),
        R3 = ifelse(runif(n) < 0.8, truth, 1 - truth),
        site = factor(rep(c("A", "B", "C", "D"), each = 10)))
    d[c(1:6, 31:40), c("R1", "R2", "R3")] <- NA
    d
}

# Continuous scores with a real institution effect.
panels_continuous_clustered <- function() {
    set.seed(11)
    site <- rep(c("A", "B", "C", "D"), each = 10)
    true <- rnorm(40, 50, 8) + c(A = 0, B = 4, C = -3, D = 6)[site]
    data.frame(R1 = true + rnorm(40, 0, 2), R2 = true + 1 + rnorm(40, 0, 2),
               R3 = true - 1 + rnorm(40, 0, 2), site = factor(site))
}

panels_long <- function(d, vars) {
    long <- data.frame(case = factor(rep(seq_len(nrow(d)), length(vars))),
                       rater = factor(rep(vars, each = nrow(d))),
                       cluster = factor(rep(d$site, length(vars))),
                       score = unlist(d[vars], use.names = FALSE))
    droplevels(long[complete.cases(long), ])
}

# Makes the full hierarchical model (the one with a rater term) fail, so the
# reduced-model fallback runs. It is otherwise unreachable on real data.
panels_fail_rater_models <- function(env = parent.frame()) {
    testthat::local_mocked_bindings(
        .quietly = function(expr, ...) {
            if (any(grepl("(1 | rater)", deparse(substitute(expr)), fixed = TRUE)))
                stop("forced failure of the rater model")
            suppressMessages(expr)
        },
        .package = "ClinicoPath", .env = env)
}

test_that("L441: whole-number codes in the hierarchical linear model are flagged", {
    d <- panels_binary_clustered()
    vars <- c("R1", "R2", "R3")
    v <- unlist(d[vars]); v <- v[!is.na(v)]
    expect_true(all(v == floor(v)) && max(abs(v)) <= 10)   # oracle: they look like codes

    a <- agreement_run(d, vars = vars, hierarchicalKappa = TRUE,
                       clusterVariable = "site", iccHierarchical = TRUE)
    for (tbl in list(a$results$hierarchicalOverallTable, a$results$hierarchicalICCTable)) {
        notes <- agreement_notes(tbl)
        expect_true("coded_numeric" %in% names(notes))
        expect_match(notes[["coded_numeric"]], "R1, R2, R3", fixed = TRUE)
    }
})

test_that("L443: hierarchical cases and clusters are counted from the model data", {
    d <- panels_binary_clustered()
    vars <- c("R1", "R2", "R3")
    rated <- rowSums(!is.na(d[vars])) > 0
    n_cases <- sum(rated)                                   # 24
    n_clusters <- length(unique(d$site[rated]))             # 3
    expect_equal(c(n_cases, n_clusters), c(24, 3))

    a <- agreement_run(d, vars = vars, hierarchicalKappa = TRUE, clusterVariable = "site")
    row <- a$results$hierarchicalOverallTable$asDF
    expect_equal(row$cases, n_cases)
    expect_equal(row$clusters, n_clusters)
    expect_match(a$results$homogeneityTestTable$asDF$conclusion,
                 sprintf("%d institutions", n_clusters), fixed = TRUE)
})

test_that("L442: a singular fit is disclosed; the fallback leaves the rater variance empty", {
    d <- panels_binary_clustered()
    long <- panels_long(d, c("R1", "R2", "R3"))
    oracle <- suppressMessages(lme4::lmer(score ~ (1 | case) + (1 | rater) + (1 | cluster), long))
    expect_true(lme4::isSingular(oracle))
    a <- agreement_run(d, vars = c("R1", "R2", "R3"), hierarchicalKappa = TRUE,
                       clusterVariable = "site")
    expect_true("singular" %in% names(agreement_notes(a$results$hierarchicalOverallTable)))

    # Fallback: the full model fails outright, which is not a singular fit.
    panels_fail_rater_models()
    b <- agreement_run(panels_continuous_clustered(), vars = c("R1", "R2", "R3"),
                       hierarchicalKappa = TRUE, clusterVariable = "site")
    vt <- b$results$varianceDecompositionTable$asDF
    expect_true(is.na(vt["rater", "variance"]))
    note <- agreement_notes(b$results$hierarchicalOverallTable)[["model_note"]]
    expect_match(note, "could not be fitted", fixed = TRUE)
    expect_no_match(note, "singular", fixed = TRUE)
})

test_that("L444: both variance tables say the cut-points are the module's own", {
    a <- agreement_run(panels_continuous_clustered(), vars = c("R1", "R2", "R3"),
                       hierarchicalKappa = TRUE, clusterVariable = "site")
    expect_match(agreement_notes(a$results$varianceDecompositionTable)[["cutpoints"]],
                 "not a published standard", fixed = TRUE)

    set.seed(4)
    d <- data.frame(R1 = rnorm(40, 50, 8), cond = factor(rep(c("M", "AI"), 20)))
    d$R2 <- d$R1 + rnorm(40, 0, 2); d$R3 <- d$R1 + rnorm(40, 1, 2)
    b <- agreement_run(d, vars = c("R1", "R2", "R3"), mixedEffectsComparison = TRUE,
                       conditionVariable = "cond")
    expect_match(agreement_notes(b$results$mixedEffectsVarianceTable)[["cutpoints"]],
                 "not a published standard", fixed = TRUE)
})

test_that("L445: the homogeneity LRT drops only the cluster term from the model fitted", {
    d <- panels_continuous_clustered()
    long <- panels_long(d, c("R1", "R2", "R3"))
    # Oracle for the fallback model (case + cluster) against case alone, both ML.
    full <- lme4::lmer(score ~ (1 | case) + (1 | cluster), long, REML = FALSE)
    red <- lme4::lmer(score ~ (1 | case), long, REML = FALSE)
    chi <- as.numeric(2 * (logLik(full) - logLik(red)))
    expect_gt(chi, 1)

    panels_fail_rater_models()
    a <- agreement_run(d, vars = c("R1", "R2", "R3"), hierarchicalKappa = TRUE,
                       clusterVariable = "site")
    row <- a$results$homogeneityTestTable$asDF
    expect_equal(row$statistic, chi, tolerance = 1e-4)
    expect_equal(row$p_value, 0.5 * pchisq(chi, 1, lower.tail = FALSE), tolerance = 1e-4)
})

test_that("L447: a failed lmerTest step is disclosed and p comes from z", {
    skip_if_not_installed("lmerTest")
    local_mocked_bindings(as_lmerModLmerTest = function(...) stop("forced"),
                          .package = "lmerTest")
    set.seed(4)
    d <- data.frame(R1 = rnorm(40, 50, 8), cond = factor(rep(c("M", "AI"), 20)))
    d$R2 <- d$R1 + rnorm(40, 0, 2); d$R3 <- d$R1 + rnorm(40, 1, 2)
    a <- agreement_run(d, vars = c("R1", "R2", "R3"), mixedEffectsComparison = TRUE,
                       conditionVariable = "cond")
    tbl <- a$results$mixedEffectsTable$asDF
    expect_true(all(is.na(tbl$df)))
    expect_equal(tbl$p_value, 2 * pnorm(-abs(tbl$t_value)))
    expect_true("df_method" %in% names(agreement_notes(a$results$mixedEffectsTable)))
})

test_that("L436: constant differences get no proportional-bias p-value, and say why", {
    set.seed(2)
    x <- round(rnorm(30, 50, 10), 2)
    for (offset in c(0, 2.5)) {
        expect_equal(sd(x - (x + offset)), 0)                # oracle: constant
        a <- agreement_run(data.frame(A = x, B = x + offset), vars = c("A", "B"),
                           blandAltmanPlot = TRUE, proportionalBias = TRUE)
        expect_true(is.na(a$results$blandAltmanStats$asDF$propBiasP))
        notes <- agreement_notes(a$results$blandAltmanStats)
        expect_match(notes[["prop_bias_failed"]], "constant", fixed = TRUE)
        expect_false("prop_bias_assumption" %in% names(notes))
    }
})

test_that("R8: the bootstrap floor counts cases rated at least twice", {
    lv <- c("Neg", "Pos")
    d <- data.frame(
        R1 = factor(c("Neg", "Pos", "Neg", "Pos", "Neg", "Pos", rep("Neg", 6)), lv),
        R2 = factor(c("Neg", "Pos", "Pos", "Pos", "Neg", "Neg", rep(NA, 6)), lv))
    n_usable <- sum(complete.cases(d))
    expect_equal(n_usable, 6)
    a <- agreement_run(d, vars = c("R1", "R2"), bootstrapCI = TRUE, nBoot = 100)
    expect_equal(nrow(a$results$bootstrapCITable$asDF), 0)
    expect_match(agreement_notes(a$results$bootstrapCITable)[["insufficient"]],
                 sprintf("; %d were available", n_usable), fixed = TRUE)
})

paired_panel_data <- function() {
    set.seed(3)
    tr <- sample(c("G1", "G2", "G3"), 40, TRUE)
    flip <- function(v, p) ifelse(runif(length(v)) < p, sample(c("G1", "G2", "G3"), length(v), TRUE), v)
    data.frame(A1 = flip(tr, .6), A2 = flip(tr, .6), B1 = flip(tr, .05), B2 = flip(tr, .05))
}

test_that("L462: a significant paired difference names its direction, not an improvement", {
    d <- paired_panel_data()
    diff_oracle <- mean(d$B1 == d$B2) - mean(d$A1 == d$A2)
    expect_gt(diff_oracle, 0)
    a <- agreement_run(d, vars = c("A1", "A2"), pairedAgreementTest = TRUE,
                       conditionBVars = c("B1", "B2"), pairedBootN = 500)
    row <- a$results$pairedAgreementTable$asDF[1, ]
    expect_equal(row$difference, diff_oracle)
    expect_equal(row$interpretation, "Significantly higher under Condition B")
})

test_that("L463: the paired agreement row is labelled as the proportion it holds", {
    d <- paired_panel_data()
    a <- agreement_run(d, vars = c("A1", "A2"), pairedAgreementTest = TRUE,
                       conditionBVars = c("B1", "B2"), pairedBootN = 500)
    row <- a$results$pairedAgreementTable$asDF[1, ]
    expect_equal(row$condition_a, mean(d$A1 == d$A2))       # a proportion, not a percent
    expect_equal(row$metric, "Proportion of exact agreement")
})

test_that("L466: the Bland-Altman guide ties the limits to the chosen coverage", {
    a <- agreement_run(data.frame(A = c(1.5, 2.5, 3.1, 4.2, 5.3), B = c(1.4, 2.7, 3.0, 4.5, 5.1)),
                       vars = c("A", "B"), showBlandAltmanGuide = TRUE)
    html <- a$results$blandAltmanExplanation$content
    expect_true(nzchar(html))
    expect_no_match(html, "1.96 SD; 95% of differences", fixed = TRUE)
    expect_match(html, "z comes from the coverage you choose (1.96 at the default 95%)", fixed = TRUE)
    expect_equal(round(qnorm(0.975), 2), 1.96)
})

test_that("L467: the PABAK guide gives the q-category formula the table computes", {
    lv <- c("G1", "G2", "G3")
    set.seed(8)
    r1 <- factor(sample(lv, 50, TRUE, prob = c(.7, .2, .1)), lv)
    r2 <- r1; flip <- sample(50, 12); r2[flip] <- sample(lv, 12, TRUE)
    a <- agreement_run(data.frame(R1 = r1, R2 = r2), vars = c("R1", "R2"),
                       pabak = TRUE, showPABAKGuide = TRUE)
    po <- mean(r1 == r2); q <- 3
    expect_equal(a$results$pabakTable$asDF$pabak[1], (q * po - 1) / (q - 1))
    html <- a$results$pabakExplanation$content
    expect_match(html, "(qP<sub>o</sub> − 1) / (q − 1)", fixed = TRUE)
    expect_match(html, "2P<sub>o</sub> − 1 with two categories", fixed = TRUE)
})

test_that("L472/L471: the continuous-data panel stays away when a continuous analysis ran", {
    set.seed(2)
    x <- rnorm(30, 50, 10)
    d <- data.frame(x, x + rnorm(30)); names(d) <- c("A&B", "C")
    expect_true(any(d[[1]] != floor(d[[1]])))                # oracle: continuous
    for (opt in c("kendallW", "robinsonA", "meanSpearman")) {
        args <- list(d, vars = names(d)); args[[opt]] <- TRUE
        a <- do.call(agreement_run, args)
        expect_no_match(a$results$welcome$content, "Continuous Data Detected", fixed = TRUE)
    }
    expect_match(agreement_notes(a$results$irrtable)[["continuous_vars"]],
                 jmvcore::htmlEscape("A&B"), fixed = TRUE)
})

test_that("L484/L485: the six categorical guides render on continuous data", {
    set.seed(2)
    x <- rnorm(30, 50, 10)
    d <- data.frame(A = x, B = x + rnorm(30))
    expect_true(any(d$A != floor(d$A)))                      # oracle: continuous
    a <- agreement_run(d, vars = c("A", "B"), icc = TRUE,
                       showRaterBiasGuide = TRUE, showBhapkarGuide = TRUE,
                       showStuartMaxwellGuide = TRUE, showPairwiseKappaGuide = TRUE,
                       showGwetGuide = TRUE, showPABAKGuide = TRUE)
    for (nm in c("raterBiasExplanation", "bhapkarExplanation", "stuartMaxwellExplanation",
                 "pairwiseKappaExplanation", "gwetExplanation", "pabakExplanation"))
        expect_true(nzchar(a$results[[nm]]$content), info = nm)
})

mixed_linked_data <- function() {
    set.seed(5); nc <- 30; ce <- rnorm(nc, 50, 10); cc <- matrix(rnorm(nc * 2, 0, 3), nc)
    w <- data.frame(case = factor(rep(sprintf("C%02d", 1:nc), 2)),
                    cond = factor(rep(c("Manual", "AI"), each = nc), levels = c("Manual", "AI")))
    for (j in 1:3)
        w[[paste0("R", j)]] <- ce[as.integer(w$case)] + c(-1, 0, 1)[j] +
            cc[cbind(as.integer(w$case), as.integer(w$cond))] + rnorm(2 * nc, 0, 1)
    w
}

test_that("ME-A: linked cases get a case-by-condition term (paired t-test oracle)", {
    w <- mixed_linked_data()
    m <- tapply(unlist(w[c("R1", "R2", "R3")]), list(rep(w$case, 3), rep(w$cond, 3)), mean)
    p_oracle <- t.test(m[, "AI"], m[, "Manual"], paired = TRUE)$p.value   # 0.207
    a <- agreement_run(w, vars = c("R1", "R2", "R3"), conditionVariable = "cond",
                       caseIdVariable = "case", mixedEffectsComparison = TRUE)
    expect_equal(a$results$mixedEffectsTable$asDF$p_value[2], p_oracle, tolerance = 0.02)
    expect_match(agreement_notes(a$results$mixedEffectsVarianceTable)[["case_condition"]],
                 "case-by-condition variance", fixed = TRUE)
})

test_that("ME-B: the mixed-effects model note counts distinct case IDs", {
    w <- mixed_linked_data()
    a <- agreement_run(w, vars = c("R1", "R2", "R3"), conditionVariable = "cond",
                       caseIdVariable = "case", mixedEffectsComparison = TRUE)
    expect_match(agreement_notes(a$results$mixedEffectsTable)[["model"]],
                 sprintf("; %d cases;", length(unique(w$case))), fixed = TRUE)
})
