# Regression tests for the 2026-09-24 /check-function-full agreement pass.
# Every expectation is checked against an independent oracle (irr, vcd, irrCAC,
# lme4 called directly, or the formula written out) - never against agreement
# itself. Helpers are in helper-agreement.R.

# --- Hierarchical, continuous ------------------------------------------------

test_that("FIX-01 continuous ICC(2,1) and ICC(2,k) condition on institution; G row and shrinkage gone", {
    skip_if_not_installed("lme4")
    withr::local_seed(21)
    n <- 120
    lab <- rep(paste0("Lab", 1:4), each = 30)
    base <- stats::rnorm(n, 50, 15) + c(Lab1 = -8, Lab2 = 0, Lab3 = 4, Lab4 = 10)[lab]
    d <- data.frame(r1 = base + stats::rnorm(n, 0, 5), r2 = base + 2 + stats::rnorm(n, 0, 5),
                    r3 = base - 1 + stats::rnorm(n, 0, 5), lab = factor(lab))
    r <- agreement_run(d, vars = c("r1", "r2", "r3"), hierarchicalKappa = TRUE,
                       clusterVariable = "lab", iccHierarchical = TRUE,
                       clusterSpecificKappa = TRUE)$results

    long <- data.frame(case_id = factor(rep(seq_len(n), 3)),
                       rater = factor(rep(c("r1", "r2", "r3"), each = n)),
                       cluster = factor(rep(d$lab, 3)), score = c(d$r1, d$r2, d$r3))
    m <- lme4::lmer(score ~ 1 + (1 | case_id) + (1 | rater) + (1 | cluster), data = long,
                    control = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
    vc <- as.data.frame(lme4::VarCorr(m))
    g <- function(x) vc$vcov[vc$grp == x]
    icc21 <- g("case_id") / (g("case_id") + g("rater") + g("Residual"))
    icc2k <- g("case_id") / (g("case_id") + (g("rater") + g("Residual")) / 3)

    expect_equal(r$hierarchicalOverallTable$asDF$overall_kappa[1], icc21, tolerance = 1e-4)
    it <- r$hierarchicalICCTable
    expect_identical(vapply(it$rowKeys, as.character, ""), c("icc1", "icc2"))
    expect_equal(it$asDF$icc_value, c(icc21, icc2k), tolerance = 1e-4)
    expect_match(r$hierarchicalOverallTable$getColumn("overall_kappa")$title, "ICC(2,1)", fixed = TRUE)

    # The overall value is now the pooled counterpart of the per-cluster ICCs. The old
    # sigma2_case / sigma2_total put the institution effect in the denominator only and
    # sat BELOW every per-cluster value when institutions differ in level.
    per <- r$clusterSpecificTable$asDF$kappa
    expect_true(all(is.finite(per)))
    expect_gt(r$hierarchicalOverallTable$asDF$overall_kappa[1], min(per) - 0.05)

    expect_false("shrinkage_kappa" %in% agreement_column_names(r$clusterSpecificTable))
    expect_false("shrinkageEstimates" %in% names(formals(ClinicoPath::agreement)))
})

# --- Hierarchical, categorical -----------------------------------------------

test_that("FIX-02 categorical hierarchical: stratified Fleiss kappa on the shipped example dataset", {
    skip_if_not_installed("irr")
    data("agreement_hierarchical", package = "ClinicoPath", envir = environment())
    d <- as.data.frame(agreement_hierarchical)
    rc <- grep("Rater", names(d), value = TRUE)
    r <- agreement_run(d, vars = rc, hierarchicalKappa = TRUE, clusterVariable = "institution",
                       clusterSpecificKappa = TRUE, clusterRankings = TRUE,
                       varianceDecomposition = TRUE, iccHierarchical = TRUE,
                       testClusterHomogeneity = TRUE)$results

    chr <- function(x) as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
    per <- vapply(split(d[rc], d$institution), function(x) irr::kappam.fleiss(chr(x))$value, 0)
    nn <- as.numeric(table(d$institution)[names(per)])
    oracle <- sum(nn * per) / sum(nn)

    ov <- r$hierarchicalOverallTable
    expect_equal(ov$asDF$overall_kappa[1], oracle, tolerance = 1e-10)
    expect_match(ov$title, "Stratified", fixed = TRUE)
    pooled <- irr::kappam.fleiss(chr(d[rc]))$value
    expect_true(any(grepl(sprintf("%.3f", pooled), agreement_notes(ov), fixed = TRUE)))

    cs <- r$clusterSpecificTable$asDF
    expect_equal(unname(cs$kappa), unname(per[cs$cluster]), tolerance = 1e-10)
    expect_identical(as.integer(cs$rank), as.integer(rank(-per[cs$cluster], ties.method = "min")))

    # Categorical ratings: no decomposition in this block (contract changed 2026-09-24,
    # when the binary glmer route was retired); each table points to the ordinal engine.
    for (nm in c("varianceDecompositionTable", "hierarchicalICCTable", "homogeneityTestTable")) {
        n <- agreement_notes(r[[nm]])
        expect_true(length(n) > 0 && grepl("Model-based agreement", n[["error"]], fixed = TRUE), info = nm)
    }
})

test_that("FIX-03 categorical hierarchical, two raters: per-cluster weighted Cohen kappa matches vcd", {
    skip_if_not_installed("vcd")
    withr::local_seed(5)
    lv <- c("G1", "G2", "G3"); n <- 150
    d <- data.frame(a = factor(sample(lv, n, TRUE), levels = lv, ordered = TRUE))
    d$b <- factor(ifelse(stats::runif(n) < 0.7, as.character(d$a), sample(lv, n, TRUE)),
                  levels = lv, ordered = TRUE)
    d$site <- sample(c("X", "Y", "Z"), n, TRUE)
    r <- agreement_run(d, vars = c("a", "b"), wght = "equal", hierarchicalKappa = TRUE,
                       clusterVariable = "site", clusterSpecificKappa = TRUE)$results
    cs <- r$clusterSpecificTable$asDF
    vk <- vapply(cs$cluster, function(s) {
        x <- d[d$site == s, ]
        vcd::Kappa(table(x$a, x$b), weights = "Equal-Spacing")$Weighted[["value"]]
    }, 0)
    expect_equal(unname(cs$kappa), unname(vk), tolerance = 1e-8)
    expect_true(all(is.finite(cs$ci_lower)) && all(cs$ci_lower < cs$kappa))
    nn <- as.numeric(table(d$site)[cs$cluster])
    expect_equal(r$hierarchicalOverallTable$asDF$overall_kappa[1], sum(nn * vk) / sum(nn), tolerance = 1e-8)
    expect_true(grepl("confidence interval is", agreement_notes(r$hierarchicalOverallTable)[["model"]], fixed = TRUE))
})

# FIX-04 used to pin the binary lme4::glmer latent-scale route (ICC vs a direct glmer
# fit, withheld below 5 raters). That route was retired 2026-09-24 - its Laplace fit
# ran a third too low with 2 raters (development-scripts/validate_agreement_latent_icc_bias.R)
# - and the decomposition of categorical ratings moved to latentModel (ordinal::clmm,
# validated in test-agreement-latent-helpers.R). New contract: binary ratings keep the
# kappa tables, the three decomposition tables stay empty and point to Model-based
# agreement, and Model-based agreement fits the institution variance from the same
# cluster variable.
test_that("FIX-04 binary hierarchical: decomposition tables point to Model-based agreement, which fits the institution variance", {
    skip_if_not_installed("ordinal")
    skip_if_not_installed("irr")
    withr::local_seed(11)
    # Binary calls: the latent gate refuses them at every design (grid v2); k = 6 raters.
    nc <- 300; ncl <- 10; k <- 6
    cl <- sample(paste0("H", 1:ncl), nc, TRUE)
    uc <- stats::rnorm(nc, 0, sqrt(1.5)); ucl <- stats::rnorm(ncl, 0, sqrt(0.3))
    names(ucl) <- paste0("H", 1:ncl); ur <- stats::rnorm(k, 0, sqrt(0.2))
    b <- as.data.frame(lapply(seq_len(k), function(j)
        factor(ifelse(uc + ucl[cl] + ur[j] + stats::rnorm(nc) > 0, "Pos", "Neg"), levels = c("Neg", "Pos"))))
    names(b) <- paste0("R", seq_len(k)); b$site <- cl
    rv <- paste0("R", 1:k)
    r <- agreement_run(b, vars = rv, hierarchicalKappa = TRUE, clusterVariable = "site",
                       varianceDecomposition = TRUE, iccHierarchical = TRUE,
                       testClusterHomogeneity = TRUE, latentModel = TRUE)$results

    # Kappa side unchanged: stratified Fleiss kappa against irr.
    chr <- function(x) as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
    per <- vapply(split(b[rv], b$site), function(x) irr::kappam.fleiss(chr(x))$value, 0)
    nn <- as.numeric(table(b$site)[names(per)])
    expect_equal(r$hierarchicalOverallTable$asDF$overall_kappa[1], sum(nn * per) / sum(nn), tolerance = 1e-10)

    # Retired route: no numbers, one pointer note per table.
    expect_true(all(is.na(r$hierarchicalICCTable$asDF$icc_value)))
    expect_true(all(is.na(r$varianceDecompositionTable$asDF$variance)))
    expect_true(all(is.na(r$homogeneityTestTable$asDF$statistic)))
    for (nm in c("varianceDecompositionTable", "hierarchicalICCTable", "homogeneityTestTable"))
        expect_true(grepl("Model-based agreement", agreement_notes(r[[nm]])[["error"]], fixed = TRUE), info = nm)

    # Binary calls are outside the latent model's validated range (grid v2, 2026-09-24: biased at every
    # tested design), so the pointer says so and the target refuses with a sentence - never a number.
    expect_true(grepl("binary calls", agreement_notes(r$varianceDecompositionTable)[["error"]], fixed = TRUE))
    lt <- r$latentModelTable
    expect_true(all(is.na(lt$asDF$estimate[vapply(lt$rowKeys, as.character, "") %in% c("rho", "kappa_m")])))
    expect_true(grepl("With two categories the latent model is not computed",
                      agreement_notes(lt)[["refuse_model"]], fixed = TRUE))

    # Ordered grades: the pointer's target delivers an institution variance from the same cluster variable.
    thr <- c(-0.5, 0.6, 1.4, 2.2)                      # 5 grades, largest category under the skew limit
    g <- as.data.frame(lapply(seq_len(k), function(j) {
        z <- uc + ucl[cl] + ur[j] + stats::rnorm(nc)
        factor(paste0("G", findInterval(z, thr) + 1), levels = paste0("G", 1:5), ordered = TRUE)
    }))
    names(g) <- rv; g$site <- cl
    ro <- agreement_run(g, vars = rv, hierarchicalKappa = TRUE, clusterVariable = "site",
                        varianceDecomposition = TRUE, latentModel = TRUE)$results
    lo <- ro$latentModelTable
    expect_true("cluster_var" %in% vapply(lo$rowKeys, as.character, ""))
    expect_true(is.finite(lo$getCell(rowKey = "cluster_var", "estimate")$value))
    expect_true(is.finite(lo$getCell(rowKey = "rho", "estimate")$value))
})

# --- Scale ordering ------------------------------------------------------------

scrambled_pair <- function() {
    # Rater a declares (G1, G3) and is selected first; rater b declares the full scale.
    # A column-order union gives G1 < G3 < G2; the declared order is G1 < G2 < G3.
    withr::with_seed(8, {
        n <- 80
        b <- sample(c("G1", "G2", "G3"), n, TRUE, prob = c(.4, .3, .3))
        a <- ifelse(b == "G2", sample(c("G1", "G3"), n, TRUE), b)
        a[sample(n, 12)] <- sample(c("G1", "G3"), 12, TRUE)
        data.frame(a = factor(a, levels = c("G1", "G3"), ordered = TRUE),
                   b = factor(b, levels = c("G1", "G2", "G3"), ordered = TRUE))
    })
}

test_that("FIX-05 Krippendorff ordinal alpha uses the merged declared order, not the column-order union", {
    skip_if_not_installed("irr")
    d <- scrambled_pair()
    r <- agreement_run(d, vars = c("a", "b"), kripp = TRUE, krippMethod = "ordinal")$results
    codes <- sapply(d, function(x) match(as.character(x), c("G1", "G2", "G3")))
    oracle <- irr::kripp.alpha(t(codes), method = "ordinal")$value
    expect_equal(r$krippTable$asDF$alpha[1], oracle, tolerance = 1e-10)
    expect_match(r$krippTable$asDF$method[1], "ordinal", fixed = TRUE)
})

test_that("FIX-06 Krippendorff refuses an order the declarations do not determine, only for ordered metrics", {
    d <- data.frame(a = factor(rep(c("Absent", "Diffuse"), 20), levels = c("Absent", "Diffuse"), ordered = TRUE),
                    b = factor(rep(c("Absent", "Focal"), 20), levels = c("Absent", "Focal"), ordered = TRUE))
    r <- agreement_run(d, vars = c("a", "b"), kripp = TRUE, krippMethod = "ordinal")$results
    expect_true(is.na(r$krippTable$asDF$alpha[1]))
    expect_true("scale_order" %in% names(agreement_notes(r$krippTable)))
    r2 <- agreement_run(d, vars = c("a", "b"), kripp = TRUE, krippMethod = "nominal")$results
    expect_true(is.finite(r2$krippTable$asDF$alpha[1]))
    expect_false("scale_order" %in% names(agreement_notes(r2$krippTable)))
})

test_that("FIX-07 the bootstrap table's Krippendorff row is the headline statistic, not an auto-selected one", {
    skip_if_not_installed("irr")
    withr::local_seed(3)
    lv <- c("L", "M", "H"); n <- 60
    a <- sample(lv, n, TRUE); b <- ifelse(stats::runif(n) < .7, a, sample(lv, n, TRUE))
    d <- data.frame(a = factor(a, levels = lv, ordered = TRUE), b = factor(b, levels = lv, ordered = TRUE))
    r <- agreement_run(d, vars = c("a", "b"), kripp = TRUE, bootstrapCI = TRUE, nBoot = 200)$results
    head_alpha <- r$krippTable$asDF$alpha[1]
    bt <- r$bootstrapCITable$asDF
    row <- bt[grepl("Krippendorff", bt$metric, fixed = TRUE), ]
    expect_equal(nrow(row), 1L)
    expect_equal(row$estimate, head_alpha, tolerance = 1e-10)
    expect_identical(row$metric, r$krippTable$asDF$method[1])
    expect_true("method_choice" %in% names(agreement_notes(r$krippTable)))   # ordered raters, nominal method
})

test_that("FIX-08 Gwet AC2 weights sit on the merged declared order; an undetermined order drops to AC1", {
    skip_if_not_installed("irrCAC")
    d <- scrambled_pair()
    r <- agreement_run(d, vars = c("a", "b"), gwet = TRUE, gwetWeights = "linear")$results
    m <- sapply(d, as.character)
    oracle <- irrCAC::gwet.ac1.raw(m, weights = "linear", categ.labels = c("G1", "G2", "G3"),
                                   conflev = 0.95, N = Inf)$est$coeff.val
    expect_equal(r$gwetTable$asDF$coefficient[1], oracle, tolerance = 1e-8)

    amb <- data.frame(a = factor(rep(c("Absent", "Diffuse"), 20), levels = c("Absent", "Diffuse"), ordered = TRUE),
                      b = factor(rep(c("Absent", "Focal"), 20), levels = c("Absent", "Focal"), ordered = TRUE))
    r2 <- agreement_run(amb, vars = c("a", "b"), gwet = TRUE, gwetWeights = "linear")$results
    ac1 <- irrCAC::gwet.ac1.raw(sapply(amb, as.character), weights = "unweighted", conflev = 0.95, N = Inf)$est$coeff.val
    expect_equal(r2$gwetTable$asDF$coefficient[1], ac1, tolerance = 1e-8)
    expect_true("scale_ambiguous" %in% names(agreement_notes(r2$gwetTable)))
})

test_that("FIX-09 Robinson's A codes every rater on one declared order; refuses a mix and an undetermined order", {
    withr::local_seed(4)
    lv <- c("G1", "G2", "G3", "G4"); n <- 60
    x <- sample(lv[2:4], n, TRUE); y <- ifelse(stats::runif(n) < .8, x, sample(lv[2:4], n, TRUE))
    d <- data.frame(a = factor(x, levels = lv, ordered = TRUE),          # declares G1..G4
                    b = factor(y, levels = lv[2:4], ordered = TRUE))     # declares G2..G4
    r <- agreement_run(d, vars = c("a", "b"), robinsonA = TRUE, nBoot = 100)$results
    m <- sapply(d, function(v) match(as.character(v), lv))
    oracle <- 1 - sum((m - rowMeans(m))^2) / sum((m - mean(m))^2)
    expect_equal(r$robinsonATable$asDF$robinsonA[1], oracle, tolerance = 1e-10)
    # Per-column codes would have put a one-grade offset on every case:
    expect_lt(1 - sum((sapply(d, as.numeric) - rowMeans(sapply(d, as.numeric)))^2) /
                  sum((sapply(d, as.numeric) - mean(sapply(d, as.numeric)))^2), oracle)

    mix <- data.frame(a = factor(x, levels = lv, ordered = TRUE), b = stats::rnorm(n))
    rm <- agreement_run(mix, vars = c("a", "b"), robinsonA = TRUE, nBoot = 100)$results
    expect_true("scale_order" %in% names(agreement_notes(rm$robinsonATable)))
})

# --- Small correctness -----------------------------------------------------------

test_that("FIX-10 Finn discloses a declared scale that is too WIDE, not only one that is too narrow", {
    withr::local_seed(1)
    d <- data.frame(r1 = sample(1:3, 60, TRUE), r2 = sample(1:3, 60, TRUE))
    wide <- agreement_run(d, vars = c("r1", "r2"), finn = TRUE, finnLevels = 10)$results
    right <- agreement_run(d, vars = c("r1", "r2"), finn = TRUE, finnLevels = 3)$results
    expect_gt(wide$finnTable$asDF$finn_value[1], right$finnTable$asDF$finn_value[1] + 0.5)
    expect_true("warning" %in% names(agreement_notes(wide$finnTable)))
    expect_false("warning" %in% names(agreement_notes(right$finnTable)))
})

test_that("FIX-11 simple majority is strict: an exact half is a plurality, and the count is disclosed", {
    d <- data.frame(r1 = c("A", "A", "B", "C"), r2 = c("A", "A", "B", "C"),
                    r3 = c("B", "A", "B", "C"), r4 = c("C", "B", "A", "C"), stringsAsFactors = TRUE)
    # case 1: A,A,B,C (2 of 4) -> plurality; case 2: A,A,A,B (3 of 4) -> A;
    # case 3: B,B,B,A -> B; case 4: C x4 -> C
    a <- agreement_run(d, vars = c("r1", "r2", "r3", "r4"), consensusRule = "majority",
                       outputs = "consensusVar")
    ct <- a$results$consensusTable
    expect_equal(sum(ct$asDF$consensus_count), 3L)
    expect_true(grepl("In 1 case(s)", agreement_notes(ct)[["majority_strict"]], fixed = TRUE))
    b <- agreement_run(d, vars = c("r1", "r2", "r3", "r4"), consensusRule = "supermajority",
                       outputs = "consensusVar")
    expect_false("majority_strict" %in% names(agreement_notes(b$results$consensusTable)))
})

test_that("FIX-12 per-class precision with no denominator is empty, not 0, and leaves the macro average", {
    d <- data.frame(r1 = factor(c(rep("A", 10), rep("B", 10), rep("C", 5)), levels = c("A", "B", "C")),
                    r2 = factor(c(rep("A", 9), "B", rep("B", 10), rep("A", 5)), levels = c("A", "B", "C")))
    r <- agreement_run(d, vars = c("r1", "r2"), confusionMatrix = TRUE)$results
    pc <- r$perClassMetricsTable$asDF
    c_row <- pc[pc$class_label == "C", ]
    expect_true(is.na(c_row$precision))                       # rater 2 never assigned C
    expect_equal(c_row$recall, 0)                              # rater 1 did, rater 2 missed all 5
    prec <- c(A = 9 / 14, B = 10 / 11)
    macro <- pc[!pc$class_label %in% c("A", "B", "C"), ]
    expect_equal(macro$precision, mean(prec), tolerance = 1e-10)
    expect_true("undefined" %in% names(agreement_notes(r$perClassMetricsTable)))
})

test_that("FIX-13 an item-modal interval with no spread is left empty, not zero-width", {
    d <- data.frame(r1 = factor(c(rep("Benign", 6), rep("Malig", 6))),
                    r2 = factor(c(rep("Benign", 6), rep("Malig", 3), rep("Benign", 3))),
                    r3 = factor(c(rep("Benign", 6), rep("Malig", 6))))
    r <- agreement_run(d, vars = c("r1", "r2", "r3"), itemModalCategoryAgreement = TRUE)$results
    t <- r$itemModalAgreementTable$asDF
    ben <- t[t$category == "Benign", ]
    expect_equal(ben$mean_agreement, 1)                    # every Benign-modal case unanimous
    expect_true(is.na(ben$ci_lower) && is.na(ben$ci_upper))
    expect_true("no_ci" %in% names(agreement_notes(r$itemModalAgreementTable)))
})

test_that("FIX-14 test-retest and pooled inter-rater rows say what they are computed from", {
    withr::local_seed(9)
    n <- 40; base <- sample(c("L", "M", "H"), n, TRUE)
    jit <- function() factor(ifelse(stats::runif(n) < .85, base, sample(c("L", "M", "H"), n, TRUE)))
    d <- data.frame(R1_T1 = jit(), R1_T2 = jit(), R1_T3 = jit(), R2_T1 = jit(), R2_T2 = jit(), R2_T3 = jit())
    r <- agreement_run(d, vars = names(d), interIntraRater = TRUE, interIntraSeparator = "_")$results
    expect_true("pair_used" %in% names(agreement_notes(r$interIntraRaterIntraTable)))
    pool <- agreement_notes(r$interIntraRaterInterTable)[["pooling"]]
    expect_true(grepl("6 reads made by 2 raters, not 6 raters", pool, fixed = TRUE))
})

test_that("FIX-15 the small-sample note quotes the coverage of the interval method in use", {
    d <- data.frame(a = factor(c(rep("P", 12), rep("N", 13))),
                    b = factor(c(rep("P", 10), rep("N", 2), rep("N", 11), rep("P", 2))))
    w <- agreement_notes(agreement_run(d, vars = c("a", "b"))$results$irrtable)[["small_sample"]]
    f <- agreement_notes(agreement_run(d, vars = c("a", "b"), kappaCIMethod = "fisherz")$results$irrtable)[["small_sample"]]
    expect_true(grepl("Wald", w, fixed = TRUE) && grepl("Fisher z interval method", w, fixed = TRUE))
    expect_true(grepl("Fisher z interval covered", f, fixed = TRUE) && !grepl("Wald", f, fixed = TRUE))
})

test_that("FIX-16 the sample-size calculator runs with no rater variables selected", {
    # One variable is below the two-rater minimum, so .run() takes the welcome branch
    # (zero variables cannot be simulated here: jmvcore::select() fails on an empty
    # selection in the R harness before .run() is reached).
    d <- data.frame(x = factor(c("a", "b", "a", "b", "a")))
    r <- agreement_run(d, vars = "x", agreementSampleSize = TRUE)$results
    v <- r$agreementSampleSizeTable$asDF
    n_req <- v$value[v$parameter == "Required Sample Size (subjects)"]
    expect_length(n_req, 1L)
    expect_true(nzchar(as.character(n_req)) && !is.na(n_req))
})

# --- A cancel must restart the analysis, never become a footnote ------------------

cancel_inside <- function(analysis, fn_name) {
    priv <- analysis$.__enclos_env__$private
    unlockBinding(".checkpoint", priv)
    priv$.checkpoint <- function(...) {
        calls <- vapply(sys.calls(), function(cl) paste(deparse(cl[[1]]), collapse = ""), "")
        if (any(grepl(fn_name, calls, fixed = TRUE))) stop(agreement_restart_error())
        invisible(NULL)
    }
    lockBinding(".checkpoint", priv)
    priv
}

test_that("FIX-17 the paired-agreement and Robinson's A bootstraps are cancellable", {
    withr::local_seed(2)
    n <- 40; lv <- c("L", "M", "H")
    base <- sample(lv, n, TRUE)
    j <- function() factor(ifelse(stats::runif(n) < .8, base, sample(lv, n, TRUE)), levels = lv, ordered = TRUE)
    d <- data.frame(a1 = j(), a2 = j(), b1 = j(), b2 = j())
    ns <- asNamespace("ClinicoPath")

    o <- ns$agreementOptions$new(vars = c("a1", "a2"), pairedAgreementTest = TRUE,
                                 conditionBVars = c("b1", "b2"), pairedBootN = 500)
    a <- ns$agreementClass$new(options = o, data = d); a$init()
    priv <- cancel_inside(a, ".calculatePairedAgreementComparison")
    expect_error(priv$.calculatePairedAgreementComparison(d[c("a1", "a2")]), "restart")

    o2 <- ns$agreementOptions$new(vars = c("a1", "a2"), robinsonA = TRUE, nBoot = 200)
    a2 <- ns$agreementClass$new(options = o2, data = d); a2$init()
    priv2 <- cancel_inside(a2, ".calculateRobinsonA")
    expect_error(priv2$.calculateRobinsonA(d[c("a1", "a2")]), "restart")
})

# --- Schema wiring ------------------------------------------------------------------

test_that("FIX-18 clearWith, enable and refs carry the dependencies the backend reads", {
    rp <- function(...) test_path("..", "..", ...)
    skip_if_not(file.exists(rp("jamovi", "agreement.r.yaml")))
    ry <- yaml::read_yaml(rp("jamovi", "agreement.r.yaml"))
    item <- function(nm) Filter(function(it) identical(it$name, nm), ry$items)[[1]]
    needs <- list(pairwiseKappaTable = "wght", bootstrapCITable = c("wght", "krippMethod"),
                  interIntraRaterIntraTable = "wght", subgroupAgreementTable = "wght",
                  concordanceF1Table = "tieBreaker", concordanceF1PerClassTable = "tieBreaker",
                  loaDetailTable = "tieBreaker", summary = "kappaCIMethod",
                  computedVariablesInfo = c("consensusRule", "detailLevel", "loaThresholds"),
                  raterDendrogram = "clusterMethod", caseDendrogram = "caseClusterMethod",
                  hierarchicalOverallTable = c("wght", "exct", "kappaCIMethod"),
                  clusterSpecificTable = c("wght", "exct", "kappaCIMethod"))
    for (nm in names(needs))
        expect_true(all(needs[[nm]] %in% unlist(item(nm)$clearWith)), info = nm)
    # An Image with an incomplete clearWith keeps a stale PNG for good; this one has none.
    expect_null(item("subgroupForestPlotImage")$clearWith)
    expect_identical(item("irrtable")$columns[[which(vapply(item("irrtable")$columns, `[[`, "", "name") == "peragree")]]$type, "number")

    uy <- readLines(rp("jamovi", "agreement.u.yaml"))
    en <- function(nm) { i <- grep(paste0("name: ", nm, "$"), uy); sub(".*enable: ", "", uy[i + 1 + grepl("format:", uy[i + 1])]) }
    expect_match(en("nBoot"), "robinsonA", fixed = TRUE)
    expect_match(en("multipleTestCorrection"), "allPairsKappa", fixed = TRUE)
    expect_false(grepl("hierarchicalKappa", en("multipleTestCorrection"), fixed = TRUE))
    expect_match(en("iccType"), "bootstrapCI", fixed = TRUE)
})

# --- Second batch -----------------------------------------------------------------

test_that("FIX-19 a reference rater also selected under Raters is not compared with itself", {
    withr::local_seed(6)
    n <- 40; lv <- c("L", "M", "H")
    ref <- sample(lv, n, TRUE)
    j <- function() factor(ifelse(stats::runif(n) < .75, ref, sample(lv, n, TRUE)), levels = lv)
    d <- data.frame(ref = factor(ref, levels = lv), r1 = j(), r2 = j())
    r <- agreement_run(d, vars = c("ref", "r1", "r2"), pairwiseKappa = TRUE, referenceRater = "ref")$results
    pk <- r$pairwiseKappaTable$asDF
    expect_setequal(as.character(pk$rater), c("r1", "r2"))    # no "ref" row
    expect_false(any(pk$kappa == 1, na.rm = TRUE))
    expect_true("ref_in_raters" %in% names(agreement_notes(r$pairwiseKappaTable)))
})

test_that("FIX-20 ICC on category-code-looking columns says it read them as measurements", {
    withr::local_seed(7)
    n <- 50
    codes <- data.frame(a = sample(0:1, n, TRUE)); codes$b <- ifelse(stats::runif(n) < .8, codes$a, 1 - codes$a)
    r <- agreement_run(codes, vars = c("a", "b"), icc = TRUE)$results
    expect_true("coded_numeric" %in% names(agreement_notes(r$iccTable)))
    meas <- data.frame(a = stats::rnorm(n, 50, 10)); meas$b <- meas$a + stats::rnorm(n, 0, 3)
    r2 <- agreement_run(meas, vars = c("a", "b"), icc = TRUE)$results
    expect_false("coded_numeric" %in% names(agreement_notes(r2$iccTable)))
})

test_that("FIX-21 the level-of-agreement table keeps categories with zero cases", {
    d <- data.frame(r1 = factor(rep(c("A", "B"), 10)), r2 = factor(rep(c("A", "B"), 10)),
                    r3 = factor(rep(c("A", "B"), 10)))           # every case unanimous
    r <- agreement_run(d, vars = c("r1", "r2", "r3"), loaVariable = TRUE, showLoaTable = TRUE,
                       detailLevel = "simple")$results
    lt <- r$loaTable$asDF
    expect_setequal(as.character(lt$loa_category), c("All Agreed", "Majority Agreed", "No Agreement"))
    expect_equal(lt$count[lt$loa_category == "All Agreed"], 20L)
    expect_equal(lt$count[lt$loa_category == "No Agreement"], 0L)
})

test_that("FIX-22 the bootstrap guide states the chosen level and the correct reading of an interval", {
    d <- data.frame(a = factor(rep(c("P", "N"), 20)), b = factor(rep(c("P", "N"), 20)))
    r <- agreement_run(d, vars = c("a", "b"), showBootstrapCIGuide = TRUE, confLevel = 0.80)$results
    html <- r$bootstrapCIExplanation$content
    expect_true(nzchar(html))
    expect_match(html, "80% CI", fixed = TRUE)
    expect_false(grepl("with 95% confidence", html, fixed = TRUE))
    expect_match(html, "does not mean the true value lies in this particular interval", fixed = TRUE)
})

test_that("FIX-23 the case-cluster seed is shown whenever the heatmap subsample is drawn", {
    withr::local_seed(12)
    n <- 230; lv <- c("L", "M", "H")
    base <- sample(lv, n, TRUE)
    j <- function() factor(ifelse(stats::runif(n) < .8, base, sample(lv, n, TRUE)), levels = lv)
    d <- data.frame(r1 = j(), r2 = j(), r3 = j())
    r <- agreement_run(d, vars = c("r1", "r2", "r3"), caseClustering = TRUE,
                       caseClusterMethod = "hierarchical", showCaseClusterHeatmap = TRUE, seed = 99)$results
    expect_match(agreement_notes(r$caseClusterTable)[["seed"]], "99", fixed = TRUE)
})

# --- Second adversarial pass: one kappa for 3+ raters everywhere -----------------

agr_missing3 <- function(n = 60, seed = 11) {
    set.seed(seed)
    lv <- c("Neg", "Low", "High")
    t <- sample(lv, n, TRUE, prob = c(0.4, 0.35, 0.25))
    d <- as.data.frame(lapply(1:4, function(j)
        factor(ifelse(stats::runif(n) < 0.75, t, sample(lv, n, TRUE)), levels = lv)))
    names(d) <- paste0("R", 1:4)
    d$R3[c(2, 9, 17, 30)] <- NA
    d$R4[c(5, 9, 41)] <- NA
    d
}

test_that("bootstrap kappa for 3+ raters is the headline statistic (every rating, exct honoured)", {
    skip_if_not_installed("irrCAC")
    d <- agr_missing3()
    for (ex in c(FALSE, TRUE)) {
        r <- agreement_run(d, vars = names(d), exct = ex, bootstrapCI = TRUE, nBoot = 100)$results
        head_k <- r$irrtable$asDF$kappa[1]
        bt <- r$bootstrapCITable$asDF
        row <- grepl("κ", bt$metric)
        expect_equal(sum(row), 1L)
        expect_equal(bt$estimate[row], head_k, tolerance = 1e-10, info = paste("exct", ex))
        expect_identical(grepl("exact", bt$metric[row], fixed = TRUE), ex)
        # Percent agreement: complete cases, as the headline (irr::agree). A case with one
        # rating used to count as agreement.
        pct <- bt$estimate[bt$metric == "Percent Agreement"]
        expect_equal(100 * pct, r$irrtable$asDF$peragree[1], tolerance = 1e-10)
    }
})

test_that("hierarchical 3+ raters: per-institution kappa on every rating, with intervals", {
    skip_if_not_installed("irrCAC")
    d <- agr_missing3(n = 90)
    d$site <- rep(c("H1", "H2", "H3"), each = 30)
    rv <- paste0("R", 1:4)
    r <- agreement_run(d, vars = rv, hierarchicalKappa = TRUE, clusterVariable = "site",
                       clusterSpecificKappa = TRUE)$results
    oracle <- vapply(split(d[rv], d$site), function(x) {
        e <- irrCAC::fleiss.kappa.raw(as.matrix(as.data.frame(lapply(x, as.character))))$est
        (e$pa - e$pe) / (1 - e$pe)
    }, 0)
    cs <- r$clusterSpecificTable$asDF
    expect_equal(unname(cs$kappa), unname(oracle[cs$cluster]), tolerance = 1e-10)
    expect_true(all(is.finite(cs$ci_lower) & is.finite(cs$ci_upper)))
    expect_equal(cs$n_cases, rep(30L, 3))          # no case dropped for a missing rating
    expect_true(grepl("confidence interval is", agreement_notes(r$hierarchicalOverallTable)[["model"]], fixed = TRUE))
})
