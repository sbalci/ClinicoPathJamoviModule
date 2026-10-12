# Release review 2026-09-25, coefficient families: ICC guard, test-retest, rater bias,
# Bhapkar / Stuart-Maxwell, Finn, Kendall's W, Light's kappa, mean correlations, Lin's
# CCC, TDI, pairwise kappa and their guides. Every numeric check uses an independent
# oracle (irr with sort.levels = TRUE, DescTools, irrCAC, stats::cor, a hand formula),
# never the analysis itself.

priv <- function(a) a$.__enclos_env__$private
note_txt <- function(tbl) paste(agreement_notes(tbl), collapse = " ")
# Any finished analysis object, for calling a guide's populate method directly.
guide_host <- function() {
    d <- data.frame(a = factor(c("x", "y", "x", "y")), b = factor(c("x", "y", "y", "y")))
    agreement_run(d, vars = c("a", "b"))
}

test_that("L427 identical continuous ratings: ICC 1, F/p/CI blank, no sentinel note", {
    skip_if_not_installed("irr")
    x <- c(12, 18, 25, 31, 40, 47, 55, 63, 70, 85)
    t <- agreement_run(data.frame(r1 = x, r2 = x), vars = c("r1", "r2"), icc = TRUE)$results$iccTable
    df <- t$asDF
    expect_equal(df$icc_value, 1)
    expect_true(is.na(df$f_value) && is.na(df$p) && is.na(df$ci_lower) && is.na(df$ci_upper))
    txt <- note_txt(t)
    expect_true(nzchar(txt))
    expect_match(txt, "residual variance is zero", fixed = TRUE)
    expect_false(grepl("Numerically unreliable", txt, fixed = TRUE))

    # Same defect where irr returns F = -6.3e15 (rounding residue) instead of Inf: the
    # test-retest ICCs pass the ratings to the guard, so the DATA decide.
    y <- c(24.3, 74.8, 41.4, 36.9, 58.6, 58.7, 20.8, 34.3, 56.6, 60.8, 51.4, 50.9)
    expect_lt(irr::icc(cbind(y, y), model = "twoway", type = "agreement")$Fvalue, 0)  # fixture bites
    r <- agreement_run(data.frame(P1_T1 = y, P1_T2 = y, P2_T1 = y, P2_T2 = y),
                       vars = c("P1_T1", "P1_T2", "P2_T1", "P2_T2"),
                       interIntraRater = TRUE)$results
    for (tb in list(r$interIntraRaterIntraTable, r$interIntraRaterInterTable)) {
        expect_true(all(tb$asDF$value == 1))
        expect_true(all(is.na(tb$asDF$p)) && all(is.na(tb$asDF$ci_lower)))
        expect_match(agreement_notes(tb)[["numeric"]], "identical ratings to every case", fixed = TRUE)
        expect_false(grepl("Numerically unreliable", note_txt(tb), fixed = TRUE))
    }

    # Guard with data: a constant offset per rater is zero residual too; the sentinel
    # columns, whose irr output is IDENTICAL to identical ratings, keep the sentinel note.
    p <- priv(guide_host())
    off <- cbind(y, y + 5)
    g <- p$.guardICCResult(irr::icc(off, model = "twoway", type = "agreement"), ratings = off)
    expect_null(g$Fvalue)
    expect_match(g$guardNote, "constant offset", fixed = TRUE)
    sen <- cbind(c(1e-12, 1, 1e12, 5, 7, 9, 11, 13), c(1e-12, 1, 1e12, 5, 7, 9, 11, 14))
    gs <- p$.guardICCResult(irr::icc(sen, model = "twoway", type = "agreement"), ratings = sen)
    expect_match(gs$guardNote, "Numerically unreliable", fixed = TRUE)
})

test_that("L410 test-retest inter row reads numeric grade codes as categories (Fleiss, not ICC)", {
    skip_if_not_installed("irr")
    set.seed(3)
    base <- sample(0:2, 40, TRUE)
    j <- function(p) ifelse(runif(40) < p, base, sample(0:2, 40, TRUE))
    d <- data.frame(R1_T1 = j(.8), R2_T1 = j(.6), R1_T2 = j(.8), R2_T2 = j(.6))
    t <- agreement_run(d, vars = names(d), interIntraRater = TRUE)$results$interIntraRaterInterTable
    expect_match(t$asDF$statistic_name, "Fleiss", fixed = TRUE)
    expect_equal(t$asDF$value, irr::kappam.fleiss(d)$value, tolerance = 1e-10)
})

test_that("L333 an empty time-point separator is refused, not split into characters", {
    set.seed(5)
    lv <- c("L", "M", "H")
    b <- sample(lv, 40, TRUE)
    jit <- function(p) factor(ifelse(runif(40) < p, b, sample(lv, 40, TRUE)), lv)
    d <- data.frame(PathA_T1 = jit(.9), PathB_T1 = jit(.5), PathA_T2 = jit(.9), PathB_T2 = jit(.5))
    r <- agreement_run(d, vars = names(d), interIntraRater = TRUE, interIntraSeparator = "")$results
    expect_equal(nrow(r$interIntraRaterIntraTable$asDF), 0L)
    expect_match(agreement_notes(r$interIntraRaterIntraTable)[["error"]], "separator is empty", fixed = TRUE)
})

test_that("L411 the intra weight_override note survives a later ordinal rater", {
    skip_if_not_installed("irr")
    set.seed(7)
    lv <- c("L", "M", "H")
    b <- sample(lv, 40, TRUE)
    mk <- function(ord) factor(ifelse(runif(40) < .8, b, sample(lv, 40, TRUE)), lv, ordered = ord)
    d <- data.frame(A_T1 = mk(FALSE), B_T1 = mk(TRUE), A_T2 = mk(FALSE), B_T2 = mk(TRUE))
    t <- agreement_run(d, vars = names(d), interIntraRater = TRUE, wght = "squared")$results$interIntraRaterIntraTable
    nt <- agreement_notes(t)
    expect_true("weight_override" %in% names(nt))
    expect_match(nt[["weight_override"]], "reads of A ", fixed = TRUE)
    expect_equal(t$asDF$value[t$asDF$rater == "A"],
                 irr::kappa2(d[, c("A_T1", "A_T2")], weight = "unweighted", sort.levels = TRUE)$value,
                 tolerance = 1e-10)
})

test_that("L337 directional test on perfect agreement: a plain note, no NaN, no raw error", {
    g <- factor(rep(c("G1", "G2", "G3"), c(10, 12, 8)), c("G1", "G2", "G3"), ordered = TRUE)
    t <- agreement_run(data.frame(R1 = g, R2 = g), vars = c("R1", "R2"), raterBias = TRUE)$results$raterBiasTable
    nt <- agreement_notes(t)
    expect_match(nt[["undefined"]], "never disagreed", fixed = TRUE)
    expect_false(any(c("sparse", "error") %in% names(nt)))
    expect_false(isTRUE(is.nan(t$asDF$chisq)))
})

test_that("L417 the directional test says which rater is the higher scorer", {
    skip_if_not_installed("irr")
    lv <- c("G1", "G2", "G3")
    set.seed(17)
    r1 <- sample(lv, 60, TRUE)
    r2 <- r1
    up <- which(r1 != "G3")[1:14]
    r2[up] <- c(G1 = "G2", G2 = "G3")[r1[up]]
    dn <- which(r1 == "G3")[1:3]
    r2[dn] <- "G2"
    d <- data.frame(Ann = factor(r1, lv, ordered = TRUE), Bob = factor(r2, lv, ordered = TRUE))
    t <- agreement_run(d, vars = c("Ann", "Bob"), raterBias = TRUE)$results$raterBiasTable
    n_bob <- sum(as.integer(d$Bob) > as.integer(d$Ann))
    n_ann <- sum(as.integer(d$Ann) > as.integer(d$Bob))
    expect_equal(c(n_bob, n_ann), c(14L, 3L))
    # the counts are the ones the test used: (U - L)^2 / (U + L)
    expect_equal(t$asDF$chisq, (n_bob - n_ann)^2 / (n_bob + n_ann), tolerance = 1e-12)
    expect_equal(t$asDF$chisq, irr::rater.bias(d)$statistic, tolerance = 1e-12)
    expect_match(agreement_notes(t)[["direction"]],
                 "Ann scored higher than Bob on 3 discordant cases, and Bob scored higher than Ann on 14", fixed = TRUE)
})

test_that("L418 L419 L420 Bhapkar / Stuart-Maxwell: perfect agreement, tiny p, liberal wording", {
    skip_if_not_installed("DescTools")
    g <- factor(rep(c("G1", "G2", "G3"), c(10, 12, 8)), c("G1", "G2", "G3"))
    r <- agreement_run(data.frame(R1 = g, R2 = g), vars = c("R1", "R2"),
                       bhapkar = TRUE, stuartMaxwell = TRUE)$results
    for (tb in list(r$bhapkarTable, r$stuartMaxwellTable)) {
        nt <- agreement_notes(tb)
        expect_match(nt[["undefined"]], "no discordant cases", fixed = TRUE)
        expect_false(any(c("sparse", "error") %in% names(nt)))
        expect_false(grepl("Error", note_txt(tb), fixed = TRUE))
    }

    set.seed(2)
    x <- factor(sample(c("G1", "G2", "G3"), 300, TRUE), c("G1", "G2", "G3"))
    y <- x
    up <- which(x != "G3")[1:80]
    y[up] <- c(G1 = "G2", G2 = "G3")[as.character(x[up])]
    r <- agreement_run(data.frame(R1 = x, R2 = y), vars = c("R1", "R2"),
                       bhapkar = TRUE, stuartMaxwell = TRUE)$results
    sm <- as.numeric(DescTools::StuartMaxwellTest(table(x, y))$statistic)
    expect_equal(r$stuartMaxwellTable$asDF$chisq, sm, tolerance = 1e-8)
    expect_equal(r$bhapkarTable$asDF$chisq, sm / (1 - sm / 300), tolerance = 1e-8)  # always >= SM
    for (tb in list(r$bhapkarTable, r$stuartMaxwellTable)) {
        expect_match(agreement_notes(tb)[["interpretation"]], "(p < .001)", fixed = TRUE)
        expect_false(grepl("0.0000", note_txt(tb), fixed = TRUE))
    }
    smn <- agreement_notes(r$stuartMaxwellTable)[["note"]]
    expect_match(smn, "more liberal", fixed = TRUE)
    expect_false(grepl("better asymptotic", smn, fixed = TRUE))

    a <- guide_host()
    priv(a)$.populateBhapkarExplanation()
    priv(a)$.populateStuartMaxwellExplanation()
    guides <- paste(a$results$bhapkarExplanation$content, a$results$stuartMaxwellExplanation$content)
    expect_false(grepl("more powerful|better statistical properties|Less powerful", guides))
    expect_match(guides, "more liberal", fixed = TRUE)
})

test_that("L380 Finn gives the same answer whatever the variable order", {
    set.seed(8)
    d <- data.frame(A = factor(sample(c("G1", "G3", "G4"), 30, TRUE), c("G1", "G3", "G4")),
                    B = factor(sample(c("G1", "G2", "G3"), 30, TRUE), c("G1", "G2", "G3")))
    ab <- agreement_run(d, vars = c("A", "B"), finn = TRUE, finnLevels = 4)$results$finnTable
    ba <- agreement_run(d, vars = c("B", "A"), finn = TRUE, finnLevels = 4)$results$finnTable
    expect_true(is.na(ab$asDF$finn_value) && is.na(ba$asDF$finn_value))
    expect_identical(agreement_notes(ab)[["error"]], agreement_notes(ba)[["error"]])
    expect_match(agreement_notes(ba)[["error"]], "do not determine one", fixed = TRUE)
})

test_that("L320 Finn does not warn for 0-based codes at the right number of categories", {
    skip_if_not_installed("irr")
    set.seed(9)
    r1 <- rbinom(30, 1, .5)
    r2 <- ifelse(runif(30) < .85, r1, 1 - r1)
    d <- data.frame(r1 = r1, r2 = r2)
    t <- agreement_run(d, vars = c("r1", "r2"), finn = TRUE, finnLevels = 2)$results$finnTable
    expect_equal(t$asDF$finn_value, irr::finn(d, s.levels = 2, model = "oneway")$value, tolerance = 1e-10)
    expect_false("warning" %in% names(agreement_notes(t)))
    t3 <- agreement_run(d, vars = c("r1", "r2"), finn = TRUE, finnLevels = 3)$results$finnTable
    expect_true("warning" %in% names(agreement_notes(t3)))   # a real mismatch still warns
})

test_that("L379 a negative Finn coefficient has its own label, and the guide allows it", {
    skip_if_not_installed("irr")
    r1 <- rep(1:3, 10)
    d <- data.frame(r1 = r1, r2 = 4 - r1)
    t <- agreement_run(d, vars = c("r1", "r2"), finn = TRUE, finnLevels = 3)$results$finnTable
    oracle <- irr::finn(d, s.levels = 3, model = "oneway")$value
    expect_lt(oracle, 0)
    expect_equal(t$asDF$finn_value, oracle, tolerance = 1e-10)
    expect_equal(t$asDF$interpretation, "Worse than random rating")
    a <- guide_host()
    priv(a)$.populateFinnExplanation()
    g <- a$results$finnExplanation$content
    expect_false(grepl("ranges from 0 to 1", g, fixed = TRUE))
    expect_match(g, "can be negative", fixed = TRUE)
})

test_that("L377 Kendall's W and Light's kappa count complete cases and disclose the drop", {
    skip_if_not_installed("irr")
    one <- data.frame(a = c(1, 2, NA, NA), b = c(1, NA, 2, NA), c = c(2, 3, 1, 3))
    r <- agreement_run(one, vars = c("a", "b", "c"), kendallW = TRUE, lightKappa = TRUE)$results
    expect_match(agreement_notes(r$kendallWTable)[["error"]], "Insufficient complete cases", fixed = TRUE)
    expect_false(grepl("incorrect number of dimensions", note_txt(r$kendallWTable), fixed = TRUE))
    expect_match(agreement_notes(r$lightKappaTable)[["error"]], "Insufficient complete cases", fixed = TRUE)
    expect_false(isTRUE(is.nan(r$lightKappaTable$asDF$kappa)))

    set.seed(11)
    m <- matrix(sample(1:3, 36, TRUE), 12)
    m[cbind(1:4, c(1, 2, 3, 1))] <- NA
    d <- as.data.frame(m)
    lt <- agreement_run(d, vars = names(d), lightKappa = TRUE)$results$lightKappaTable
    expect_equal(lt$asDF$subjects, 8L)
    expect_equal(lt$asDF$kappa, irr::kappam.light(stats::na.omit(d))$value, tolerance = 1e-10)
    expect_match(agreement_notes(lt)[["listwise"]], "4 of 12 cases were dropped", fixed = TRUE)
})

test_that("L388 Lin's CCC at perfect concordance: no zero-width interval, no mixed-method claim", {
    skip_if_not_installed("DescTools")
    x <- c(12, 18, 25, 31, 40, 47, 55, 63, 70, 85, 90, 95)
    expect_true(is.nan(DescTools::CCC(x, x, ci = "z-transform")$rho.c$lwr.ci))   # oracle: undefined
    t <- agreement_run(data.frame(x = x, y = x), vars = c("x", "y"), linCCC = TRUE)$results$linCCCTable
    expect_equal(t$asDF$ccc, 1)
    expect_true(is.na(t$asDF$ci_lower) && is.na(t$asDF$ci_upper))
    nt <- agreement_notes(t)
    expect_true("ci_undefined" %in% names(nt))
    expect_false(grepl("Some rows use", note_txt(t), fixed = TRUE))
})

test_that("L385 L386 correlation and TDI notes disclose the 0.20 rule and say Cases", {
    set.seed(13)
    z <- rnorm(20, 50, 10)
    d <- data.frame(a = z + rnorm(20), b = z + rnorm(20), c = z + rnorm(20))
    r <- agreement_run(d, vars = names(d), meanSpearman = TRUE, meanPearson = TRUE, tdi = TRUE)$results
    for (tb in list(r$meanSpearmanTable, r$meanPearsonTable)) {
        expect_match(agreement_notes(tb)[["interpretation"]], "span less than 0.20", fixed = TRUE)
        expect_match(agreement_notes(tb)[["interpretation"]], "display convention", fixed = TRUE)
    }
    for (tb in list(r$meanSpearmanTable, r$meanPearsonTable, r$tdiTable)) {
        expect_true(nzchar(note_txt(tb)))
        expect_false(grepl("Subjects", note_txt(tb), fixed = TRUE))
    }
})

test_that("L422 L423 pairwise kappa explains a non-estimable rater; All-Pairs guide is accurate", {
    skip_if_not_installed("irr")
    lv <- c("N", "P")
    set.seed(14)
    d <- data.frame(ref = factor(rep("P", 30), lv), ra = factor(rep("P", 30), lv),
                    rb = factor(sample(lv, 30, TRUE), lv))
    expect_true(is.nan(irr::kappa2(d[, c("ra", "ref")])$value))            # oracle: 0/0
    t <- agreement_run(d, vars = c("ra", "rb"), pairwiseKappa = TRUE, referenceRater = "ref")$results$pairwiseKappaTable
    df <- t$asDF
    expect_false(any(is.nan(c(df$kappa, df$z, df$p))))
    expect_true(is.na(df$kappa[df$rater == "ra"]))
    fn <- t$getCell(col = "kappa", rowKey = "ra")$footnotes
    expect_true(any(grepl("Not estimable", fn, fixed = TRUE)))
    expect_match(agreement_notes(t)[["summary"]], "1 of the 2 raters have no estimable kappa", fixed = TRUE)

    a <- guide_host()
    priv(a)$.populateAllPairsKappaExplanation()
    g <- a$results$allPairsKappaExplanation$content
    expect_false(grepl("95% confidence", g, fixed = TRUE))
    expect_false(grepl("psych::cohen.kappa", g, fixed = TRUE))
})

test_that("L407 the Maxwell RE guide lists the four bands the code uses", {
    a <- guide_host()
    priv(a)$.populateMaxwellREExplanation()
    g <- a$results$maxwellREExplanation$content
    expect_match(g, "0.30 to below 0.50", fixed = TRUE)
    expect_match(g, "0.50 to below 0.70", fixed = TRUE)
    expect_false(grepl(">0.30 to 0.70<", g, fixed = TRUE))
})

test_that("L408 L409 the test-retest guide matches the pooled inter-rater row and the CI methods", {
    a <- guide_host()
    priv(a)$.populateInterIntraRaterExplanation()
    g <- a$results$interIntraRaterExplanation$content
    expect_false(grepl("Hour 0 &#x3BA;=0.84", g, fixed = TRUE))
    expect_false(grepl("Baseline &#x3BA;=0.72", g, fixed = TRUE))
    expect_false(grepl("fair consistency", g, fixed = TRUE))
    expect_false(grepl("Fisher's Z transformation", g, fixed = TRUE))
    expect_false(grepl("weighted kappa automatically applied", g, fixed = TRUE))
    expect_match(g, "McGraw", fixed = TRUE)
    expect_match(g, "pooled", fixed = TRUE)
})

test_that("L373 L374 L375 Gwet and use-case guides: neutral wording, ordinal examples, real option title", {
    skip_if_not_installed("irr"); skip_if_not_installed("irrCAC")
    a <- guide_host()
    priv(a)$.populateGwetExplanation()
    priv(a)$.populateClinicalUseCases()
    gw <- a$results$gwetExplanation$content
    uc <- a$results$clinicalUseCases$content
    expect_false(grepl("more accurate measure", gw, fixed = TRUE))
    expect_false(grepl("reflects the true level", gw, fixed = TRUE))
    # the worked example's numbers: 100 cases, 2 calls each, 1 shared
    r1 <- c(1, 1, rep(0, 98)); r2 <- c(1, 0, 1, rep(0, 97))
    expect_equal(round(irr::kappa2(data.frame(r1, r2))$value, 2), 0.49)
    expect_equal(round(irrCAC::gwet.ac1.raw(cbind(r1, r2))$est$coeff.val, 2), 0.98)
    expect_match(gw, "kappa is 0.49 and Gwet's AC1 is 0.98", fixed = TRUE)
    expect_false(grepl("Show Level Ordering Information", uc, fixed = TRUE))
    expect_match(uc, "Level ordering information", fixed = TRUE)
    unweighted_block <- sub("(?s).*Cohen's/Fleiss' Kappa \\(Standard Method\\)(.*?)Weighted Kappa \\(Ordinal Data\\).*", "\\1", uc, perl = TRUE)
    expect_lt(nchar(unweighted_block), nchar(uc))                           # the block was found
    expect_false(grepl("Tumor grading|TNM staging|Biomarker scoring", unweighted_block))
})

test_that("LA mean Spearman / Pearson is the arithmetic mean of the pairwise coefficients", {
    x <- 1:50
    set.seed(1)
    R3 <- x + rnorm(50, 0, 22)
    R2 <- x
    R2[10:11] <- R2[11:10]
    d <- data.frame(R1 = x, R2 = R2, R3 = R3)
    r <- agreement_run(d, vars = names(d), meanSpearman = TRUE, meanPearson = TRUE)$results
    cs <- stats::cor(d, method = "spearman")
    cp <- stats::cor(d)
    expect_equal(r$meanSpearmanTable$asDF$meanRho, mean(cs[upper.tri(cs)]), tolerance = 1e-12)
    expect_equal(r$meanPearsonTable$asDF$meanR, mean(cp[upper.tri(cp)]), tolerance = 1e-12)
    expect_false(grepl("Fisher", note_txt(r$meanSpearmanTable), fixed = TRUE))
})

test_that("LB Kendall's W flags a reversed rater, as the mean Spearman table does", {
    set.seed(15)
    b <- 1:30
    d <- data.frame(R1 = b + rnorm(30, 0, 2), R2 = b + rnorm(30, 0, 2), R3 = 31 - b + rnorm(30, 0, 2))
    cs <- stats::cor(d, method = "spearman")
    expect_lte(mean(cs[upper.tri(cs)]), -0.30)                              # oracle
    r <- agreement_run(d, vars = names(d), kendallW = TRUE, meanSpearman = TRUE)$results
    expect_match(agreement_notes(r$kendallWTable)[["interpretation"]], "Negative rank correlation", fixed = TRUE)
    expect_match(r$meanSpearmanTable$asDF$interpretation, "Negative rank correlation", fixed = TRUE)
})

test_that("LC TDI states the order-statistic minimum n, not a bootstrap or a fixed 30", {
    # oracle: the fewest pairs for which P(Bin(n, 0.9) <= n - 1) >= 0.975
    n_min <- which(vapply(1:200, function(n) stats::pbinom(n - 1, n, 0.9) >= 0.975, logical(1)))[1]
    expect_equal(n_min, 36L)
    set.seed(3)
    z <- rnorm(36, 50, 10)
    d <- data.frame(a = z + rnorm(36), b = z + rnorm(36))
    t35 <- agreement_run(d[1:35, ], vars = c("a", "b"), tdi = TRUE, tdiLimit = 10)$results$tdiTable
    t36 <- agreement_run(d, vars = c("a", "b"), tdi = TRUE, tdiLimit = 10)$results$tdiTable
    expect_true(is.na(t35$asDF$ci_upper))
    expect_false(is.na(t36$asDF$ci_upper))
    expect_match(agreement_notes(t35)[["warning"]], "needs at least 36 pairs", fixed = TRUE)
    expect_false("warning" %in% names(agreement_notes(t36)))
    a <- guide_host()
    priv(a)$.populateTDIExplanation()
    g <- a$results$tdiExplanation$content
    expect_false(grepl("bootstrap", g, fixed = TRUE))
    expect_match(g, "36 pairs", fixed = TRUE)
})
