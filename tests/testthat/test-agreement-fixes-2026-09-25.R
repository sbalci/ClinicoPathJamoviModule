# Regression tests for the 2026-09-25 /fix-function pass, which applied the findings of
# development-ideas/agreement-review-function-2026-09-24.md. Finding ids (A1, B1, C2, ...)
# are that review's. Every expectation is checked against an independent oracle (vcd, irr,
# DescTools, or the formula written out) - never against agreement itself. Each test was
# run against the unfixed code first and failed there. Helpers are in helper-agreement.R.

of <- function(x, lv) factor(x, levels = lv, ordered = TRUE)
vcd_kappa <- function(x, y, lv, w = "unweighted") {
    k <- vcd::Kappa(table(factor(x, lv), factor(y, lv)),
                    weights = if (w == "squared") "Fleiss-Cohen" else "Equal-Spacing")
    if (w == "unweighted") k$Unweighted[["value"]] else k$Weighted[["value"]]
}

# --- Slice 1: category order ---------------------------------------------------

test_that("A1 weighted kappa keeps the declared order when the raters used different grades", {
    skip_if_not_installed("vcd")
    # R1 uses G1/G2/G4 and Ref uses G1/G2/G3: irr::kappa2 without sort.levels built the
    # scale as G1, G2, G4, G3 and printed 0.370 instead of 0.213.
    LV <- c("G1", "G2", "G3", "G4")
    r1 <- c(rep("G1", 10), rep("G2", 10), rep("G4", 10))
    r2 <- c(rep("G1", 9), "G2", rep("G2", 8), "G3", "G3", rep("G3", 3), rep("G2", 2), rep("G1", 5))
    oracle <- vcd_kappa(r1, r2, LV, "squared")
    expect_equal(oracle, 0.2133333, tolerance = 1e-6)

    d <- data.frame(R1 = of(r1, LV), Ref = of(r2, LV))
    a <- agreement_run(d, vars = c("R1", "Ref"), wght = "squared")
    expect_equal(a$results$irrtable$asDF$kappa, oracle, tolerance = 1e-6)

    d3 <- data.frame(R1 = of(r1, LV), R2 = of(r1, LV), Ref = of(r2, LV))
    p <- agreement_run(d3, vars = c("R1", "R2"), pairwiseKappa = TRUE, referenceRater = "Ref",
                       wght = "squared")$results$pairwiseKappaTable$asDF
    expect_equal(p$kappa, rep(oracle, 2), tolerance = 1e-6)
})

test_that("B1 intra-rater weighted kappa merges the two time points' levels in declared order", {
    skip_if_not_installed("irr")
    withr::local_seed(14)
    lv <- c("Low", "Moderate", "High")
    g1 <- sample(c("Low", "High"), 40, TRUE)                  # time 1 never uses Moderate
    g2 <- ifelse(stats::runif(40) < 0.7, g1, sample(lv, 40, TRUE))
    d <- data.frame(R1_T1 = factor(g1, levels = c("Low", "High"), ordered = TRUE),
                    R1_T2 = factor(g2, levels = lv, ordered = TRUE))
    oracle <- irr::kappa2(data.frame(match(g1, lv), match(g2, lv)), "squared")$value
    t <- agreement_run(d, vars = names(d), interIntraRater = TRUE, interIntraSeparator = "_",
                       wght = "squared")$results$interIntraRaterIntraTable
    expect_equal(t$asDF$value[1], oracle, tolerance = 1e-6)          # was 0.388 "Fair"
    expect_false("scale_ambiguous" %in% names(agreement_notes(t)))
})

test_that("C2 subgroup and per-institution weighted kappa use the declared scale", {
    skip_if_not_installed("vcd")
    withr::local_seed(3)
    n <- 150
    lv <- c("G1", "G2", "G3", "G4")
    inst <- rep(c("A", "B", "C"), each = 50)
    tru <- sample(1:4, n, TRUE)
    tru[inst == "C" & tru == 3] <- sample(c(2, 4), sum(inst == "C" & tru == 3), TRUE)
    jit <- function(t) pmin(4, pmax(1, t + sample(c(-1, 0, 0, 0, 1), length(t), TRUE)))
    x1 <- jit(tru); x2 <- jit(tru)
    x1[inst == "C" & x1 == 3] <- 2; x2[inst == "C" & x2 == 3] <- 4   # site C never uses G3
    d <- data.frame(R1 = of(lv[x1], lv), R2 = of(lv[x2], lv), inst = factor(inst))
    oracle <- vapply(c("A", "B", "C"), function(i) {
        s <- d[d$inst == i, ]
        vcd_kappa(as.character(s$R1), as.character(s$R2), lv, "squared")
    }, 0)

    a <- agreement_run(d, vars = c("R1", "R2"), wght = "squared",
                       agreementBySubgroup = TRUE, subgroupVariable = "inst",
                       hierarchicalKappa = TRUE, clusterVariable = "inst",
                       clusterSpecificKappa = TRUE)$results
    sg <- a$subgroupAgreementTable$asDF
    expect_equal(sg$agreement_stat[match(c("A", "B", "C"), sg$subgroup)], unname(oracle),
                 tolerance = 1e-6)                                        # C was 0.463
    cs <- a$clusterSpecificTable$asDF
    expect_equal(cs$kappa[match(c("A", "B", "C"), cs$cluster)], unname(oracle), tolerance = 1e-6)
})

test_that("A12 All-Pairs and Pairwise do not lay weights over an unordered scale", {
    skip_if_not_installed("vcd")
    withr::local_seed(12)
    lv <- c("Adeno", "Squamous", "Small cell", "Large cell")
    base <- sample(lv, 60, TRUE)
    flip <- function(x) ifelse(stats::runif(60) < 0.3, sample(lv, 60, TRUE), x)
    d <- data.frame(R1 = factor(flip(base)), R2 = factor(flip(base)), R3 = factor(flip(base)))
    oracle <- vcd_kappa(as.character(d$R1), as.character(d$R2), levels(d$R1))

    ap <- agreement_run(d, vars = c("R1", "R2", "R3"), allPairsKappa = TRUE,
                        wght = "squared")$results$allPairsKappaTable
    expect_equal(ap$asDF$kappa[1], oracle, tolerance = 1e-6)
    expect_true("weight_override" %in% names(agreement_notes(ap)))
    expect_false(grepl("squared", agreement_notes(ap)[["summary"]], fixed = TRUE))   # A15

    pw <- agreement_run(d, vars = c("R1", "R3"), pairwiseKappa = TRUE, referenceRater = "R2",
                        wght = "squared")$results$pairwiseKappaTable
    expect_equal(pw$asDF$kappa[pw$asDF$rater == "R1"], oracle, tolerance = 1e-6)
    expect_true("weight_override" %in% names(agreement_notes(pw)))
})

test_that("D7 two raters with weights on unordered factors, or Exact kappa, report kappa instead of stopping", {
    skip_if_not_installed("vcd")
    withr::local_seed(7)
    lv <- c("a", "b", "c")
    x <- sample(lv, 50, TRUE)
    y <- ifelse(stats::runif(50) < 0.7, x, sample(lv, 50, TRUE))
    oracle <- vcd_kappa(x, y, lv)
    d <- data.frame(r1 = factor(x), r2 = factor(y))

    w <- agreement_run(d, vars = c("r1", "r2"), wght = "equal", icc = TRUE)$results
    expect_equal(w$irrtable$asDF$kappa, oracle, tolerance = 1e-6)
    expect_true("weight_override" %in% names(agreement_notes(w$irrtable)))

    e <- agreement_run(d, vars = c("r1", "r2"), exct = TRUE)$results
    expect_equal(e$irrtable$asDF$kappa, oracle, tolerance = 1e-6)    # Conger = Cohen for 2 raters
    expect_true("exact_two_raters" %in% names(agreement_notes(e$irrtable)))
})

# --- Slice 2: marginal homogeneity ---------------------------------------------

from_counts <- function(tab, lv = rownames(tab)) {
    idx <- which(tab > 0, arr.ind = TRUE)
    data.frame(R1 = factor(rep(rownames(tab)[idx[, 1]], tab[idx]), levels = lv),
               R2 = factor(rep(colnames(tab)[idx[, 2]], tab[idx]), levels = lv))
}
# Stuart-Maxwell written out (Agresti 2013, sec. 11.3), on the square table after
# dropping only categories with no discordant case.
sm_oracle <- function(tab) {
    off <- rowSums(tab) + colSums(tab) - 2 * diag(tab)
    tab <- tab[off > 0, off > 0, drop = FALSE]
    k <- nrow(tab)
    d <- (rowSums(tab) - colSums(tab))[-k]
    V <- -(tab + t(tab)); diag(V) <- rowSums(tab) + colSums(tab) - 2 * diag(tab)
    s <- drop(t(d) %*% solve(V[-k, -k, drop = FALSE], d))
    c(sm = s, df = k - 1)
}

test_that("A2 Stuart-Maxwell keeps a category whose marginals happen to be equal", {
    skip_if_not_installed("DescTools")
    lv <- c("a", "b", "c")
    tab <- matrix(c(14, 1, 4, 4, 12, 2, 0, 5, 12), 3, byrow = TRUE, dimnames = list(lv, lv))
    dt <- DescTools::StuartMaxwellTest(tab)
    r <- agreement_run(from_counts(tab), vars = c("R1", "R2"), stuartMaxwell = TRUE,
                       bhapkar = TRUE)$results
    sm <- r$stuartMaxwellTable$asDF
    expect_equal(sm$chisq, unname(dt$statistic[1]), tolerance = 1e-6)    # was 4.00
    expect_equal(sm$df, 2)                                                # was 1
    expect_equal(sm$p, unname(dt$p.value[1]), tolerance = 1e-6)          # was 0.046
    bh <- r$bhapkarTable$asDF
    n <- sum(tab)
    expect_equal(bh$chisq, sm$chisq / (1 - sm$chisq / n), tolerance = 1e-6)
    expect_gte(bh$chisq, sm$chisq)
})

test_that("A3 Stuart-Maxwell pairs categories by name when the raters used different ones", {
    # R1 uses a/b/c and R2 uses a/b/d. irr built a non-square table and matched R1's
    # "c" with R2's "d" by position: X2 = 5.00, p = 0.025, where the square table over
    # a/b/c/d gives 11.0 on 3 df.
    r1 <- c(rep("a", 12), rep("b", 14), rep("c", 11))
    r2 <- c(rep("a", 10), rep("b", 2), rep("a", 2), rep("b", 12), rep("d", 6), rep("b", 5))
    all_lv <- c("a", "b", "c", "d")
    o <- sm_oracle(table(factor(r1, all_lv), factor(r2, all_lv)))
    d <- data.frame(R1 = factor(r1), R2 = factor(r2))
    sm <- agreement_run(d, vars = c("R1", "R2"), stuartMaxwell = TRUE)$results$stuartMaxwellTable
    expect_equal(sm$asDF$chisq, unname(o["sm"]), tolerance = 1e-6)
    expect_equal(sm$asDF$df, unname(o["df"]))
    expect_false("error" %in% names(agreement_notes(sm)))
})

test_that("A4 Bhapkar drops a category with no discordant case instead of failing", {
    lv <- c("a", "b", "c")
    # Category "a" is used only in agreement: irr::bhapkar hit a singular system.
    tab <- matrix(c(9, 0, 0, 0, 10, 6, 0, 1, 11), 3, byrow = TRUE, dimnames = list(lv, lv))
    o <- sm_oracle(tab)
    n <- sum(tab)
    r <- agreement_run(from_counts(tab), vars = c("R1", "R2"), bhapkar = TRUE)$results
    bh <- r$bhapkarTable
    expect_equal(bh$asDF$chisq, unname(o["sm"] / (1 - o["sm"] / n)), tolerance = 1e-6)
    expect_equal(bh$asDF$df, 1)
    expect_false("error" %in% names(agreement_notes(bh)))
})

# --- Slice 3: continuous measurements are not routed to categorical outputs ------

ki67 <- function(n = 60, seed = 25) {
    withr::with_seed(seed, {
        truth <- stats::runif(n, 2, 90)
        data.frame(A1 = round(truth + stats::rnorm(n, 0, 3), 1),
                   A2 = round(truth + stats::rnorm(n, 0, 3), 1),
                   B1 = round(truth + stats::rnorm(n, 0, 1), 1),
                   B2 = round(truth + stats::rnorm(n, 0, 1), 1))
    })
}

test_that("C1 paired agreement comparison refuses continuous measurements", {
    d <- ki67()
    t <- agreement_run(d, vars = c("A1", "A2"), pairedAgreementTest = TRUE,
                       conditionBVars = c("B1", "B2"))$results$pairedAgreementTable
    expect_equal(t$rowCount, 0)                                   # was 0.000 vs 0.017, "no difference"
    expect_match(agreement_notes(t)[["error"]], "continuous measurements", fixed = TRUE)
})

test_that("A6 nominal Krippendorff's alpha is refused on measurements; interval alpha still runs", {
    skip_if_not_installed("irr")
    d <- ki67()
    k <- agreement_run(d, vars = c("A1", "A2"), kripp = TRUE)$results$krippTable
    expect_true(is.na(k$asDF$alpha))                               # was about 0, "below chance"
    expect_match(agreement_notes(k)[["method_choice"]], "not computed", fixed = TRUE)
    ki <- agreement_run(d, vars = c("A1", "A2"), kripp = TRUE, krippMethod = "interval")$results$krippTable
    expect_equal(ki$asDF$alpha, irr::kripp.alpha(t(as.matrix(d[, c("A1", "A2")])), "interval")$value,
                 tolerance = 1e-8)
})

test_that("D2 D3 D4 continuous data: bar-plot request draws, summary is written, category-only outputs say why", {
    d <- ki67()
    a <- agreement_run(d, vars = c("A1", "A2"), raterProfiles = TRUE, raterProfileType = "barplot",
                       showSummary = TRUE, sft = TRUE, loaVariable = TRUE,
                       bootstrapCI = TRUE, nBoot = 100, outputs = "consensusVar")
    r <- a$results
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    drawn <- a$.__enclos_env__$private$.raterProfilePlot(r$raterProfilePlot,
                                                         ggtheme = ggplot2::theme_grey(), theme = NULL)
    expect_true(drawn)                                              # was "object 'Category' not found"
    expect_true(nzchar(r$summary$content))
    expect_match(r$summary$content, "continuous measurements", fixed = TRUE)
    expect_false("pct_agreement" %in% r$bootstrapCITable$rowKeys)  # was 0.008 beside ICC 0.9
    expect_true("continuous" %in% names(agreement_notes(r$contingencyTable)))
    expect_true("continuous" %in% names(agreement_notes(r$loaDetailTable)))
    expect_equal(r$loaDetailTable$rowCount, 0)                      # was 59 of 60 "Poor"
    expect_true("continuous" %in% names(agreement_notes(r$consensusTable)))
})

test_that("B14 intra-rater table reads 0/1/2 grade codes as categories, like the headline", {
    skip_if_not_installed("irr")
    withr::local_seed(8)
    t1 <- sample(0:2, 40, TRUE)
    t2 <- ifelse(stats::runif(40) < 0.7, t1, sample(0:2, 40, TRUE))
    d <- data.frame(R1_T1 = t1, R1_T2 = t2)
    t <- agreement_run(d, vars = names(d), interIntraRater = TRUE,
                       interIntraSeparator = "_")$results$interIntraRaterIntraTable$asDF
    expect_match(t$statistic_name[1], "κ", fixed = TRUE)     # was ICC(2,1)
    expect_equal(t$value[1], irr::kappa2(data.frame(t1, t2))$value, tolerance = 1e-8)
})

# --- Slice 5a: level of agreement and consensus --------------------------------

test_that("A8 a case with one rating gets no agreement level and no consensus label", {
    lv <- c("Benign", "Atypical", "Malignant")
    d <- data.frame(R1 = factor(c("Benign", "Malignant", "Atypical", "Benign", NA, "Benign"), lv),
                    R2 = factor(c("Benign", "Malignant", "Benign", NA, NA, "Benign"), lv),
                    R3 = factor(c("Benign", "Malignant", "Atypical", NA, "Malignant", "Benign"), lv))
    r <- agreement_run(d, vars = c("R1", "R2", "R3"), loaVariable = TRUE, detailLevel = "detailed",
                       consensusRule = "unanimous", outputs = "consensusVar")$results
    # Rows 4 and 5 have one rating each: no agreement level (was "Absolute", 100%).
    det <- r$loaDetailTable$asDF
    expect_true(all(is.na(det$agreement_pct[det$case_id %in% c(4, 5)]) |
                    !(c(4, 5) %in% det$case_id)))
    expect_match(agreement_notes(r$loaDetailTable)[["single_rating"]], "2 case(s)", fixed = TRUE)
    # Unanimous needs every rater: only rows 1, 2 and 6 qualify (was 1, 2, 4, 5, 6).
    cons <- r$consensusTable$asDF
    expect_equal(sum(cons$consensus_count), 3)
    expect_true("too_few_ratings" %in% names(agreement_notes(r$consensusTable)))
})

test_that("A9 simple mode: a two-rater disagreement is No Agreement, not a majority", {
    d <- data.frame(R1 = factor(c("Benign", "Malignant", "Benign", "Benign", "Malignant", "Benign")),
                    R2 = factor(c("Malignant", "Benign", "Benign", "Malignant", "Malignant", "Benign")))
    r <- agreement_run(d, vars = c("R1", "R2"), loaVariable = TRUE, showLoaTable = TRUE,
                       detailLevel = "simple")$results
    lt <- r$loaTable$asDF
    expect_equal(lt$count[lt$loa_category == "Majority Agreed"], 0)
    expect_equal(lt$count[lt$loa_category == "No Agreement"], 3)   # was 0: all three "Majority"
})

# --- Slice 5b: other majors -----------------------------------------------------

test_that("A5 D6 a class the raters never matched has F1 = 0 and counts in the macro average", {
    pairs <- rbind(cbind(rep("A", 18), rep("A", 18)), cbind(rep("B", 18), rep("B", 18)),
                   cbind(rep("C", 5), rep("A", 5)), cbind(rep("A", 3), rep("C", 3)),
                   cbind(rep("A", 2), rep("B", 2)), cbind(rep("B", 2), rep("A", 2)))
    d <- data.frame(R1 = factor(pairs[, 1]), R2 = factor(pairs[, 2]))
    cm <- table(d$R1, d$R2)
    f1 <- vapply(rownames(cm), function(k) {
        tp <- cm[k, k]; fp <- sum(cm[, k]) - tp; fn <- sum(cm[k, ]) - tp
        2 * tp / (2 * tp + fp + fn)
    }, 0)
    expect_equal(unname(f1["C"]), 0)
    t <- agreement_run(d, vars = c("R1", "R2"), confusionMatrix = TRUE)$results$perClassMetricsTable
    df <- t$asDF
    expect_equal(df$f1[df$class_label == "C"], 0)                    # was blank
    expect_equal(df$f1[nrow(df)], mean(f1), tolerance = 1e-10)       # was the mean of A and B only
    expect_identical(t$getColumn("n")$title, "Agreements (TP)")      # the column holds TP, was "N"
})

test_that("C4 a prediction matching any annotator is never a concordance miss", {
    withr::local_seed(4)
    cls <- c("0", "1+", "2+")
    ann <- replicate(3, sample(cls, 80, TRUE))
    pred <- ann[cbind(seq_len(80), sample(1:3, 80, TRUE))]           # always one of the three
    d <- data.frame(AI = factor(pred, cls), P1 = factor(ann[, 1], cls),
                    P2 = factor(ann[, 2], cls), P3 = factor(ann[, 3], cls))
    t <- agreement_run(d, vars = names(d), multiAnnotatorConcordance = TRUE,
                       predictionColumn = 1)$results$concordanceF1PerClassTable$asDF
    expect_equal(t$concordance_f1, rep(1, nrow(t)))                 # was 0.59-0.68
})

test_that("B6 TDI upper bound is the order-statistic bound, and too few pairs give no verdict", {
    withr::local_seed(6)
    d60 <- data.frame(a = stats::rnorm(60, 10, 2)); d60$b <- d60$a + stats::rnorm(60, 0, 0.3)
    t60 <- agreement_run(d60, vars = c("a", "b"), tdi = TRUE, tdiLimit = 5)$results$tdiTable$asDF
    v <- sort(abs(d60$a - d60$b))
    expect_equal(t60$ci_upper, v[which(stats::pbinom(0:60, 60, 0.9) >= 0.975)[1]])
    expect_match(t60$meets_criteria, "Yes", fixed = TRUE)

    d20 <- d60[1:20, ]
    t20 <- agreement_run(d20, vars = c("a", "b"), tdi = TRUE, tdiLimit = 5)$results$tdiTable$asDF
    expect_true(is.na(t20$ci_upper))                                 # 20 pairs cannot bound P90
    expect_match(t20$meets_criteria, "too few pairs", fixed = TRUE)  # was "Yes (criterion met)"
})

test_that("D1 the confidence level no longer rewrites the Bland-Altman coverage", {
    js <- readLines(file.path(testthat::test_path(), "..", "..", "jamovi", "js", "agreement.events.js"))
    expect_false(any(grepl("onChange_confLevel", js, fixed = TRUE)))
    ui <- readLines(file.path(testthat::test_path(), "..", "..", "jamovi", "agreement.u.yaml"))
    expect_false(any(grepl("onChange_confLevel", ui, fixed = TRUE)))
})

test_that("D5 rater dendrogram leaves carry the rater names", {
    withr::local_seed(5)
    lv <- c("G1", "G2", "G3")
    base <- sample(lv, 50, TRUE)
    mk <- function() factor(ifelse(stats::runif(50) < 0.8, base, sample(lv, 50, TRUE)), lv, ordered = TRUE)
    d <- data.frame(Ann = mk(), Ben = mk(), Cem = mk(), Deniz = mk())
    r <- agreement_run(d, vars = names(d), raterClustering = TRUE, showDendrogram = TRUE)$results
    expect_setequal(r$raterDendrogram$state$labels[[1]], names(d))  # was NULL, drawn as 1..4
})

test_that("A7 item-modal agreement needs three raters", {
    d <- data.frame(R1 = factor(rep(c("a", "b"), 20)), R2 = factor(rep(c("a", "a", "b", "b"), 10)))
    t <- agreement_run(d, vars = c("R1", "R2"), itemModalCategoryAgreement = TRUE)$results$itemModalAgreementTable
    expect_equal(t$rowCount, 0)                                      # was 1.000 for every category
    expect_match(agreement_notes(t)[["n_raters"]], "at least 3 raters", fixed = TRUE)
})

test_that("A10 A11 per-category rows on a 3-level scale are not labelled Negative; the PSA/NSA note speaks of prevalence", {
    lv <- c("Absent", "Focal", "Diffuse")
    withr::local_seed(10)
    x <- sample(lv, 60, TRUE, prob = c(.6, .25, .15))
    y <- ifelse(stats::runif(60) < 0.8, x, sample(lv, 60, TRUE))
    d <- data.frame(R1 = factor(x, lv), R2 = factor(y, lv))
    t <- agreement_run(d, vars = c("R1", "R2"), specificAgreement = TRUE,
                       specificPositiveCategory = "Diffuse", specificAllCategories = TRUE)$results$specificAgreementTable
    expect_false(any(grepl("(Negative)", t$asDF$category, fixed = TRUE)))
    expect_false("info" %in% names(agreement_notes(t)))

    b <- data.frame(R1 = factor(ifelse(x == "Diffuse", "Pos", "Neg")), R2 = factor(ifelse(y == "Diffuse", "Pos", "Neg")))
    tb <- agreement_run(b, vars = c("R1", "R2"), specificAgreement = TRUE,
                        specificPositiveCategory = "Pos")$results$specificAgreementTable
    expect_match(agreement_notes(tb)[["info"]], "prevalence", fixed = TRUE)
    expect_false(grepl("diagnostic bias", agreement_notes(tb)[["info"]], fixed = TRUE))
})

test_that("C3 the mixed-effects comparison says it treats each row as a case", {
    skip_if_not_installed("lme4")
    withr::local_seed(3)
    n <- 40
    d <- data.frame(r1 = stats::rnorm(n, 50, 10), cond = factor(rep(c("AI", "Manual"), each = n / 2)))
    d$r2 <- d$r1 + stats::rnorm(n, 0, 3); d$r3 <- d$r1 + stats::rnorm(n, 1, 3)
    t <- agreement_run(d, vars = c("r1", "r2", "r3"), mixedEffectsComparison = TRUE,
                       conditionVariable = "cond")$results$mixedEffectsTable
    expect_match(agreement_notes(t)[["design"]], "separate case", fixed = TRUE)
})

# --- Slice 4: interpretation bands ---------------------------------------------

test_that("B2 Robinson's A is graded on the chance-corrected value, so independent raters read Poor", {
    withr::local_seed(2)
    d <- data.frame(r1 = stats::rnorm(200, 50, 10), r2 = stats::rnorm(200, 50, 10))
    m <- as.matrix(d)
    a_hand <- 1 - sum((m - rowMeans(m))^2) / sum((m - mean(m))^2)
    expect_gt(a_hand, 0.45)                                         # chance sits near 1/m = 0.5
    t <- agreement_run(d, vars = c("r1", "r2"), robinsonA = TRUE, nBoot = 100)$results$robinsonATable
    expect_equal(t$asDF$robinsonA, a_hand, tolerance = 1e-10)
    expect_identical(t$asDF$interpretation, "Poor agreement")      # was "Moderate agreement"
    expect_match(agreement_notes(t)[["interpretation"]], "chance-corrected", fixed = TRUE)
})

test_that("B3 Kendall's W is graded on the mean Spearman correlation, so independent raters read Very weak", {
    withr::local_seed(3)
    d <- data.frame(r1 = sample(1:5, 60, TRUE), r2 = sample(1:5, 60, TRUE))
    t <- agreement_run(d, vars = c("r1", "r2"), kendallW = TRUE)$results$kendallWTable
    w <- t$asDF$w
    expect_gt(w, 0.3)                                               # W near 1/2 at chance
    expect_match(agreement_notes(t)[["interpretation"]], "Very weak", fixed = TRUE)
})

test_that("B4 B5 a perfect pair does not set the mean correlation; a negative mean is not Negligible", {
    withr::local_seed(4)
    x <- stats::rnorm(40, 50, 10)
    d <- data.frame(r1 = x, r2 = x, r3 = x + stats::rnorm(40, 0, 12))
    cm <- stats::cor(d)
    t <- agreement_run(d, vars = names(d), meanPearson = TRUE)$results$meanPearsonTable
    expect_equal(t$asDF$meanR, mean(cm[upper.tri(cm)]), tolerance = 1e-10)   # was ~0.99
    expect_match(agreement_notes(t)[["averaging"]], "arithmetic mean", fixed = TRUE)

    n <- data.frame(r1 = x, r2 = 100 - x + stats::rnorm(40, 0, 2))
    tn <- agreement_run(n, vars = c("r1", "r2"), meanPearson = TRUE)$results$meanPearsonTable
    expect_match(tn$asDF$interpretation, "Negative", fixed = TRUE)         # was "Negligible"
})

test_that("B7 B8 ICC note names the bands the confidence interval spans; constant ratings do not crash the label", {
    skip_if_not_installed("irr")
    withr::local_seed(7)
    t8 <- stats::rnorm(8, 50, 10)
    d <- data.frame(r1 = t8 + stats::rnorm(8, 0, 3), r2 = t8 + stats::rnorm(8, 0, 3))
    o <- irr::icc(d, model = "twoway", type = "agreement", unit = "single")
    expect_lt(o$lbound, 0.75)                                        # CI crosses a band edge
    t <- agreement_run(d, vars = c("r1", "r2"), icc = TRUE)$results$iccTable
    expect_match(agreement_notes(t)[["interpretation"]], "grade the confidence interval", fixed = TRUE)

    k <- data.frame(r1 = rep(20, 12), r2 = rep(20, 12))
    tk <- agreement_run(k, vars = c("r1", "r2"), icc = TRUE)$results$iccTable
    expect_false(any(grepl("missing value where TRUE/FALSE", agreement_notes(tk), fixed = TRUE)))
})

# --- Slice 6: minors ------------------------------------------------------------

test_that("B9 B10 two-way Finn uses the residual df, and a tiny p is not printed as 0.000", {
    skip_if_not_installed("irr")
    withr::local_seed(9)
    truth <- sample(1:5, 20, TRUE)
    d <- as.data.frame(replicate(3, pmin(5, pmax(1, truth + sample(c(-1, 0, 0, 1), 20, TRUE)))))
    names(d) <- c("r1", "r2", "r3")
    t <- agreement_run(d, vars = names(d), finn = TRUE, finnModel = "twoway",
                       finnLevels = 5)$results$finnTable
    o <- irr::finn(d, 5, "twoway")
    df2 <- (20 - 1) * (3 - 1)                                        # was 20 * (3 - 1) = 40
    expect_identical(as.character(t$asDF$df), "F(Inf,38)")
    expect_equal(t$asDF$p, stats::pf(o$statistic, Inf, df2, lower.tail = FALSE), tolerance = 1e-12)
    expect_false(grepl("p = 0.000", agreement_notes(t)[["interpretation"]], fixed = TRUE))
})

test_that("B13 Bland-Altman needs three complete pairs", {
    d <- data.frame(a = c(10.2, 11.5, NA, 12.1), b = c(10.0, 11.9, 12.4, NA))
    r <- agreement_run(d, vars = c("a", "b"), blandAltmanPlot = TRUE)$results
    expect_match(agreement_notes(r$blandAltmanStats)[["error"]], "at least 3 cases", fixed = TRUE)
    expect_true(all(is.na(unlist(r$blandAltmanStats$asDF[, -1]))))
})

test_that("C5 a case no annotator rated is not a concordance miss", {
    cls <- c("A", "B")
    d <- data.frame(AI = factor(c(rep("A", 10), rep("B", 10), "A", "B"), cls),
                    P1 = factor(c(rep("A", 10), rep("B", 10), NA, NA), cls),
                    P2 = factor(c(rep("A", 10), rep("B", 10), NA, NA), cls))
    r <- agreement_run(d, vars = names(d), multiAnnotatorConcordance = TRUE, predictionColumn = 1)$results
    t <- r$concordanceF1PerClassTable$asDF
    expect_equal(t$concordance_f1, c(1, 1))                         # was below 1 on the two unrated rows
})

test_that("C6 a weighting request on a 3+ rater subgroup table is disclosed", {
    withr::local_seed(6)
    lv <- c("G1", "G2", "G3")
    b <- sample(lv, 60, TRUE)
    mk <- function() factor(ifelse(stats::runif(60) < 0.8, b, sample(lv, 60, TRUE)), lv, ordered = TRUE)
    d <- data.frame(R1 = mk(), R2 = mk(), R3 = mk(), site = factor(rep(c("S1", "S2"), 30)))
    t <- agreement_run(d, vars = c("R1", "R2", "R3"), wght = "squared", agreementBySubgroup = TRUE,
                       subgroupVariable = "site")$results$subgroupAgreementTable
    expect_match(agreement_notes(t)[["weights_fleiss"]], "no weighted form", fixed = TRUE)
})

test_that("D16 the All-Pairs and Item-Modal guides are filled before raters are chosen", {
    r <- agreement_run(data.frame(x = 1:5), vars = "x", showAllPairsKappaGuide = TRUE,
                       showItemModalGuide = TRUE)$results
    expect_true(nzchar(r$allPairsKappaExplanation$content))
    expect_true(nzchar(r$itemModalAgreementExplanation$content))
})

# --- Codex review of this pass (P2 findings) ------------------------------------

agr_object25 <- function(d, ..., outputs = character()) {
    ns <- asNamespace("ClinicoPath")
    options <- ns$agreementOptions$new(...)
    for (nm in outputs) {
        opt <- options$option(nm)
        opt$value <- list(value = TRUE, synced = FALSE)
    }
    ns$agreementClass$new(options = options, data = d)
}
rerun25 <- function(a, d) {
    a$.__enclos_env__$private$.data <- d
    suppressWarnings(suppressMessages(a$run()))
    a
}
output_values <- function(out) unlist(out$.__enclos_env__$private$.values)

test_that("CX1 a rerun on measurements blanks the consensus column a categorical run wrote", {
    lv <- c("Benign", "Malignant")
    cat_d <- data.frame(R1 = factor(rep(lv, 10), lv), R2 = factor(rep(lv, 10), lv), R3 = factor(rep(lv, 10), lv))
    a <- agr_object25(cat_d, vars = c("R1", "R2", "R3"), outputs = "consensusVar")
    rerun25(a, cat_d)
    expect_true(all(output_values(a$results$consensusVar) %in% lv))
    cont_d <- data.frame(R1 = seq(10.5, 29.5, 1), R2 = seq(10.7, 29.7, 1), R3 = seq(10.2, 29.2, 1))
    rerun25(a, cont_d)
    expect_true(all(is.na(output_values(a$results$consensusVar))))   # was the old labels
    expect_match(a$results$computedVariablesInfo$content, "continuous", fixed = TRUE)
})

test_that("CX2 a Bland-Altman rerun with too few pairs does not keep the previous limits or plot", {
    withr::local_seed(12)
    d <- data.frame(a = stats::rnorm(60, 10, 2)); d$b <- d$a + stats::rnorm(60, 0, 1)
    a <- agr_object25(d, vars = c("a", "b"), blandAltmanPlot = TRUE)
    rerun25(a, d)
    expect_true(is.finite(a$results$blandAltmanStats$asDF$lowerLoA))
    d2 <- d; d2$b[3:60] <- NA
    rerun25(a, d2)
    expect_true(is.na(a$results$blandAltmanStats$asDF$lowerLoA))          # was the 60-pair limit
    expect_null(a$results$blandAltman$state)                                # was the 60-point plot
})

test_that("CX3 intra-rater table drops the weights when one time point is nominal", {
    skip_if_not_installed("vcd")
    withr::local_seed(13)
    lv <- c("Low", "Mid", "High")
    t1 <- sample(lv, 60, TRUE)
    t2 <- ifelse(stats::runif(60) < 0.6, t1, sample(lv, 60, TRUE))
    d <- data.frame(R1_T1 = factor(t1, lv, ordered = TRUE), R1_T2 = factor(t2, lv))   # second is nominal
    t <- agreement_run(d, vars = names(d), interIntraRater = TRUE, interIntraSeparator = "_",
                       wght = "squared")$results$interIntraRaterIntraTable
    expect_equal(t$asDF$value[1], vcd_kappa(t1, t2, lv), tolerance = 1e-8)   # was quadratic-weighted
    expect_true("weight_override" %in% names(agreement_notes(t)))
})

test_that("CX4 Kendall's W label uses the actual mean Spearman correlation when grades are tied", {
    d <- data.frame(r1 = rep(1:5, each = 20), r2 = c(rep(1, 98), 5, 5))
    rho <- stats::cor(d$r1, d$r2, method = "spearman")
    t <- agreement_run(d, vars = c("r1", "r2"), kendallW = TRUE)$results$kendallWTable
    expect_match(agreement_notes(t)[["scale"]], sprintf("%.3f", rho), fixed = TRUE)   # was 0.094
})

# --- Round 2: maintainer decisions of 2026-09-25 ---------------------------------

test_that("C3 a case ID links a case's rows, so the condition effect is estimated within case", {
    skip_if_not_installed("lme4")
    withr::local_seed(33)
    n <- 40
    truth <- stats::rnorm(n, 50, 10)
    one <- function(shift) {
        base <- truth + shift
        data.frame(r1 = base + stats::rnorm(n, 0, 1.5), r2 = base + 1 + stats::rnorm(n, 0, 1.5),
                   r3 = base - 1 + stats::rnorm(n, 0, 1.5))
    }
    d <- rbind(cbind(one(0), cond = "A", case = paste0("c", 1:n)),
               cbind(one(2), cond = "B", case = paste0("c", 1:n)))
    d$cond <- factor(d$cond); d$case <- factor(d$case)
    long <- data.frame(score = c(d$r1, d$r2, d$r3), rater = factor(rep(c("r1", "r2", "r3"), each = 2 * n)),
                       condition = factor(rep(d$cond, 3)), case_id = factor(rep(d$case, 3)))
    m <- lme4::lmer(score ~ condition + (1 | case_id) + (1 | rater), data = long,
                    control = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
    oracle <- summary(m)$coefficients["conditionB", ]

    t <- agreement_run(d, vars = c("r1", "r2", "r3"), mixedEffectsComparison = TRUE,
                       conditionVariable = "cond", caseIdVariable = "case")$results$mixedEffectsTable
    row <- t$asDF[grepl("B", t$asDF$term), ]
    expect_equal(row$estimate, unname(oracle["Estimate"]), tolerance = 1e-4)
    expect_equal(row$se, unname(oracle["Std. Error"]), tolerance = 1e-4)   # was 2.09 unlinked
    expect_match(agreement_notes(t)[["design"]], "40 case(s)", fixed = TRUE)
})

test_that("B11 Lin's CCC uses every case both raters scored (pairwise)", {
    skip_if_not_installed("DescTools")
    withr::local_seed(11)
    truth <- stats::rnorm(40, 50, 10)
    d <- data.frame(A = truth + stats::rnorm(40, 0, 3), B = truth + 2 + stats::rnorm(40, 0, 3),
                    C = truth + stats::rnorm(40, 0, 3))
    d$C[sample(40, 12)] <- NA
    t <- agreement_run(d, vars = c("A", "B", "C"), linCCC = TRUE)$results$linCCCTable
    ab <- t$asDF[t$asDF$comparison == "A vs B", ]
    expect_equal(ab$subjects, 40)                                            # was 28
    expect_equal(ab$ccc, DescTools::CCC(d$A, d$B)$rho.c$est, tolerance = 1e-8)
    expect_equal(t$asDF$subjects[t$asDF$comparison == "A vs C"], 28)
    expect_true("pairwise" %in% names(agreement_notes(t)))
})

test_that("A13 A14 Gwet's AC1 counts the declared scale and keeps case-different labels apart", {
    skip_if_not_installed("irrCAC")
    withr::local_seed(13)
    lv <- c("0", "1+", "2+", "3+")                                           # 3+ never used
    b <- sample(lv[1:3], 60, TRUE)
    mk <- function() factor(ifelse(stats::runif(60) < 0.75, b, sample(lv[1:3], 60, TRUE)), lv, ordered = TRUE)
    d <- data.frame(R1 = mk(), R2 = mk(), R3 = mk())
    codes <- sapply(d, function(x) match(as.character(x), lv))
    decl <- irrCAC::gwet.ac1.raw(codes, categ.labels = 1:4)$est$coeff.val
    obs <- irrCAC::gwet.ac1.raw(codes, categ.labels = 1:3)$est$coeff.val
    expect_false(isTRUE(all.equal(decl, obs)))                               # the fixture discriminates
    g <- agreement_run(d, vars = names(d), gwet = TRUE)$results$gwetTable$asDF
    expect_equal(g$coefficient, decl, tolerance = 1e-5)                      # was the observed-q value

    lv2 <- c("neg", "pos", "Pos")
    x <- sample(lv2, 50, TRUE)
    e <- data.frame(R1 = factor(x, lv2), R2 = factor(ifelse(stats::runif(50) < 0.7, x, sample(lv2, 50, TRUE)), lv2))
    c2 <- sapply(e, function(v) match(as.character(v), lv2))
    g2 <- agreement_run(e, vars = c("R1", "R2"), gwet = TRUE)$results$gwetTable$asDF
    expect_equal(g2$coefficient, irrCAC::gwet.ac1.raw(c2, categ.labels = 1:3)$est$coeff.val,
                 tolerance = 1e-5)                                          # was pos/Pos merged
})

test_that("C9 the continuous hierarchical ICC names its estimand", {
    skip_if_not_installed("lme4")
    withr::local_seed(21)
    n <- 120
    lab <- rep(paste0("Lab", 1:4), each = 30)
    base <- stats::rnorm(n, 50, 15) + c(Lab1 = -8, Lab2 = 0, Lab3 = 4, Lab4 = 10)[lab]
    d <- data.frame(r1 = base + stats::rnorm(n, 0, 5), r2 = base + 2 + stats::rnorm(n, 0, 5),
                    r3 = base - 1 + stats::rnorm(n, 0, 5), lab = factor(lab))
    r <- agreement_run(d, vars = c("r1", "r2", "r3"), hierarchicalKappa = TRUE,
                       clusterVariable = "lab", iccHierarchical = TRUE)$results
    expect_match(r$hierarchicalOverallTable$getColumn("overall_kappa")$title, "within institution", fixed = TRUE)
    expect_true(all(grepl("within institution", r$hierarchicalICCTable$asDF$icc_type, fixed = TRUE)))
})

test_that("A16 first-occurring tie-break and declared row order", {
    d <- data.frame(R1 = factor(c("Malignant", "Benign", "Benign", "Malignant", "Benign", "Benign")),
                    R2 = factor(c("Benign", "Benign", "Malignant", "Malignant", "Benign", "Benign")))
    det <- agreement_run(d, vars = c("R1", "R2"), loaVariable = TRUE, tieBreaker = "first",
                         detailLevel = "detailed")$results$loaDetailTable$asDF
    expect_identical(as.character(det$modal_rating[1]), "Malignant")        # was "Benign" (alphabetical)
    expect_identical(as.character(det$modal_rating[3]), "Benign")

    lv <- c("Low", "Mid", "High")
    withr::local_seed(16)
    b <- sample(lv, 60, TRUE)
    mk <- function() factor(ifelse(stats::runif(60) < 0.8, b, sample(lv, 60, TRUE)), lv, ordered = TRUE)
    d3 <- data.frame(R1 = mk(), R2 = mk(), R3 = mk())
    im <- agreement_run(d3, vars = names(d3), itemModalCategoryAgreement = TRUE)$results$itemModalAgreementTable$asDF
    expect_identical(as.character(im$category), lv)                         # was High, Low, Mid
    pc <- agreement_run(d3[, 1:2], vars = c("R1", "R2"), confusionMatrix = TRUE)$results$perClassMetricsTable$asDF
    expect_identical(as.character(pc$class_label[1:3]), lv)
})

test_that("D9 heatmap and profile plots follow the declared order, skip NA, and keep small state", {
    lv <- c("Low", "Intermediate", "High")
    withr::local_seed(19)
    b <- sample(lv, 40, TRUE)
    mk <- function() factor(ifelse(stats::runif(40) < 0.8, b, sample(lv, 40, TRUE)), lv, ordered = TRUE)
    d <- data.frame(R1 = mk(), R2 = mk(), R3 = mk())
    d$R3[1:6] <- NA
    r <- agreement_run(d, vars = names(d), agreementHeatmap = TRUE, raterProfiles = TRUE)$results
    hs <- r$agreementHeatmapPlot$state
    expect_null(hs$ratings_list)                                             # was every raw rating
    expect_identical(rownames(hs$tables[[1]]), lv)                           # was High, Intermediate, Low
    expect_equal(hs$total_pairs, 3)
    ps <- r$raterProfilePlot$state
    expect_identical(ps$category_levels, lv)
    expect_false(anyNA(ps$plot_data$Category))                               # was 6 NA bars
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    a <- agreement_run(d, vars = names(d), agreementHeatmap = TRUE)
    expect_true(a$.__enclos_env__$private$.agreementHeatmap(a$results$agreementHeatmapPlot,
                                                            ggtheme = NULL, theme = NULL))
})

# --- Codex review, round 2 (P2) --------------------------------------------------

test_that("CX5 constant raters do not let Kendall's W be graded on the pairs that remain", {
    withr::local_seed(5)
    x <- sample(1:5, 30, TRUE)
    d <- data.frame(a = x, b = x, c = 3, d = 3, e = 3, f = 3)
    t <- agreement_run(d, vars = names(d), kendallW = TRUE)$results$kendallWTable
    note <- agreement_notes(t)[["interpretation"]]
    expect_false(grepl("Very strong", note, fixed = TRUE))            # was graded on 1 of 15 pairs
    expect_match(note, "same rating", fixed = TRUE)
})

test_that("CX6 refusing nominal alpha on measurements also clears a previous bootstrap interval", {
    skip_if_not_installed("irr")
    lv <- c("a", "b", "c")
    withr::local_seed(6)
    x <- sample(lv, 40, TRUE)
    cat_d <- data.frame(R1 = factor(x, lv), R2 = factor(ifelse(stats::runif(40) < 0.7, x, sample(lv, 40, TRUE)), lv))
    a <- agr_object25(cat_d, vars = c("R1", "R2"), kripp = TRUE, bootstrap = TRUE, nBoot = 100)
    rerun25(a, cat_d)
    expect_true(is.finite(a$results$krippTable$asDF$ci_lower))
    cont_d <- data.frame(R1 = stats::rnorm(40, 50, 10)); cont_d$R2 <- cont_d$R1 + stats::rnorm(40, 0, 2)
    rerun25(a, cont_d)
    k <- a$results$krippTable
    expect_true(is.na(k$asDF$alpha))
    expect_true(is.na(k$asDF$ci_lower) && is.na(k$asDF$ci_upper))   # was the old interval
    expect_false("seed" %in% names(agreement_notes(k)))
})

test_that("CX7 numeric grade codes keep their weights in the test-retest table, as in the headline", {
    skip_if_not_installed("vcd")
    withr::local_seed(7)
    t1 <- sample(0:3, 50, TRUE)
    t2 <- pmin(3, pmax(0, t1 + sample(c(-1, 0, 0, 1), 50, TRUE)))
    oracle <- vcd_kappa(as.character(t1), as.character(t2), as.character(0:3), "squared")
    d <- data.frame(R1_T1 = t1, R1_T2 = t2)
    r <- agreement_run(d, vars = names(d), interIntraRater = TRUE, interIntraSeparator = "_",
                       wght = "squared")$results
    expect_equal(r$interIntraRaterIntraTable$asDF$value[1], oracle, tolerance = 1e-8)   # was unweighted
    expect_equal(r$irrtable$asDF$kappa, oracle, tolerance = 1e-8)
})

test_that("CX8 the test-retest statistic is chosen on the whole columns, not only the complete pairs", {
    skip_if_not_installed("irr")
    d <- data.frame(P_T1 = c(rep(1:5, 4), 10.5), P_T2 = c(rep(c(1, 2, 3, 4, 4), 4), NA))
    t <- agreement_run(d, vars = names(d), interIntraRater = TRUE,
                       interIntraSeparator = "_")$results$interIntraRaterIntraTable$asDF
    expect_match(t$statistic_name[1], "ICC", fixed = TRUE)          # was Cohen's kappa 0.75
    cc <- stats::complete.cases(d)
    expect_equal(t$value[1], irr::icc(d[cc, ], model = "twoway", type = "agreement")$value, tolerance = 1e-8)
})

# --- Codex review, round 4 (P2) --------------------------------------------------

test_that("CX9 numeric grade codes keep their weights in the paired comparison, as in the headline", {
    skip_if_not_installed("vcd")
    withr::local_seed(9)
    g <- sample(0:3, 60, TRUE)
    jit <- function() pmin(3, pmax(0, g + sample(c(-1, 0, 0, 0, 1), 60, TRUE)))
    d <- data.frame(r1 = jit(), r2 = jit(), s1 = jit(), s2 = jit())
    oracle <- vcd_kappa(as.character(d$r1), as.character(d$r2), as.character(0:3), "squared")
    r <- agreement_run(d, vars = c("r1", "r2"), wght = "squared", pairedAgreementTest = TRUE,
                       conditionBVars = c("s1", "s2"), pairedBootN = 500)$results
    expect_equal(r$irrtable$asDF$kappa, oracle, tolerance = 1e-8)
    pt <- r$pairedAgreementTable$asDF
    k_row <- pt[grepl("κ", pt$metric), ]
    expect_equal(k_row$condition_a, oracle, tolerance = 1e-8)                # was unweighted 0.42
    expect_match(k_row$metric, "Quadratic", fixed = TRUE)
})

test_that("CX10 a rerun on measurements drops the previous run's consensus and agreement-level notes", {
    lv <- c("Benign", "Atypical", "Malignant")
    cat_d <- data.frame(R1 = factor(c("Benign", "Malignant", "Atypical", "Benign", NA, "Benign"), lv),
                        R2 = factor(c("Benign", "Malignant", "Benign", NA, NA, "Benign"), lv),
                        R3 = factor(c("Benign", "Malignant", "Atypical", NA, "Malignant", "Benign"), lv))
    a <- agr_object25(cat_d, vars = c("R1", "R2", "R3"), loaVariable = TRUE, consensusRule = "unanimous",
                      outputs = "consensusVar")
    rerun25(a, cat_d)
    expect_true("too_few_ratings" %in% names(agreement_notes(a$results$consensusTable)))
    expect_true("single_rating" %in% names(agreement_notes(a$results$loaDetailTable)))
    cont_d <- data.frame(R1 = c(10.2, 11.5, 12.1, 13.3, 14.8, 15.1), R2 = c(10.4, 11.1, 12.6, 13.0, 14.2, 15.9),
                         R3 = c(10.1, 11.8, 12.2, 13.9, 14.4, 15.3))
    rerun25(a, cont_d)
    expect_identical(names(agreement_notes(a$results$consensusTable)), "continuous")   # was + too_few_ratings
    expect_identical(names(agreement_notes(a$results$loaDetailTable)), "continuous")   # was + single_rating
})

# --- Codex review, round 5 (P2) --------------------------------------------------

test_that("CX11 numeric grade codes that miss different grades keep one weighted scale for kappa and its CI", {
    skip_if_not_installed("vcd")
    x <- c(rep(c(0, 0, 2, 3), 10), 1, NA)
    y <- c(rep(c(0, 1, 3, 3), 10), NA, 2)
    d <- data.frame(x = x, y = y)
    cc <- stats::complete.cases(d)
    lv <- as.character(0:3)
    kw <- vcd::Kappa(table(factor(x[cc], 0:3), factor(y[cc], 0:3)), weights = "Fleiss-Cohen")$Weighted
    h <- agreement_run(d, vars = c("x", "y"), wght = "squared")$results$irrtable$asDF
    expect_equal(h$kappa, kw[["value"]], tolerance = 1e-8)
    expect_equal(h$ci_lower, kw[["value"]] - stats::qnorm(0.975) * kw[["ASE"]], tolerance = 1e-8)  # was 0.196
    expect_equal(h$ci_upper, min(1, kw[["value"]] + stats::qnorm(0.975) * kw[["ASE"]]), tolerance = 1e-8)
})

test_that("CX12 numeric grade codes and the same grades as ordered factors give the same paired bootstrap", {
    withr::local_seed(12)
    g <- sample(0:3, 20, TRUE, prob = c(.4, .3, .2, .1))
    jit <- function() pmin(3, pmax(0, g + sample(c(-1, 0, 0, 1), 20, TRUE)))
    num <- data.frame(r1 = jit(), r2 = jit(), s1 = jit(), s2 = jit())
    ord <- as.data.frame(lapply(num, function(v) factor(v, levels = 0:3, ordered = TRUE)))
    run <- function(d) agreement_run(d, vars = c("r1", "r2"), wght = "squared", pairedAgreementTest = TRUE,
                                     conditionBVars = c("s1", "s2"), pairedBootN = 500,
                                     seed = 7)$results$pairedAgreementTable$asDF
    pn <- run(num); po <- run(ord)
    kn <- pn[grepl("κ", pn$metric), ]; ko <- po[grepl("κ", po$metric), ]
    expect_equal(kn$condition_a, ko$condition_a, tolerance = 1e-10)
    expect_equal(kn$p_value, ko$p_value, tolerance = 1e-10)                # was 0.232 vs 0.140
    expect_equal(c(kn$ci_lower, kn$ci_upper), c(ko$ci_lower, ko$ci_upper), tolerance = 1e-10)
})

test_that("CX13 test-retest numeric codes that miss different grades keep the weights", {
    skip_if_not_installed("vcd")
    t1 <- rep(c(0, 0, 2, 3), 10); t2 <- rep(c(0, 1, 3, 3), 10)          # {0,2,3} vs {0,1,3}
    d <- data.frame(R1_T1 = t1, R1_T2 = t2)
    oracle <- vcd_kappa(as.character(t1), as.character(t2), as.character(0:3), "squared")
    t <- agreement_run(d, vars = names(d), interIntraRater = TRUE, interIntraSeparator = "_",
                       wght = "squared")$results$interIntraRaterIntraTable
    expect_equal(t$asDF$value[1], oracle, tolerance = 1e-8)
    expect_false("scale_ambiguous" %in% names(agreement_notes(t)))
})
