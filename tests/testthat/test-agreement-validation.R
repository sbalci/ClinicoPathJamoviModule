# Validation regression tests for `agreement` (Interrater Reliability).
# Produced by /validate-function agreement depth=exhaustive, 2026-09-21.
# Every expectation below was verified against an INDEPENDENT oracle (a hand calculation, a
# different package, or the method author's own published worked example) in
# development-scripts/validate_agreement.R. Report:
# development-ideas/agreement-validation-2026-09-21.md

# The package under test provides the wrapper. Do NOT source() R/agreement.*.R here: a
# source() preamble detaches the package namespace, so jmvcore's .() translation helper
# leaves scope and any code path reaching a .() string dies with
# `could not find function "."` (project reference test-source-preamble-breaks-dot-translate).
agreement <- ClinicoPath::agreement

test_that("VAL-agreement-C01 Cohen's kappa matches a hand calculation and vcd::Kappa", {
  # TP=9 FN=1 FP=2 TN=8, n=20. Po = 17/20 = .85; Pe = .5*.55 + .5*.45 = .5;
  # kappa = (.85-.5)/(1-.5) = .70  -- arithmetic, not a package.
  skip_if_not_installed("vcd")
  d <- data.frame(
    R1 = factor(rep(c("pos","neg","pos","neg"), c(9,1,2,8)), levels = c("pos","neg")),
    R2 = factor(rep(c("pos","pos","neg","neg"), c(9,1,2,8)), levels = c("pos","neg")))
  r <- suppressWarnings(suppressMessages(agreement(data = d, vars = c("R1","R2"))))
  tb <- r$irrtable$asDF
  expect_equal(as.numeric(tb$peragree[1]) / 100, 0.85, tolerance = 1e-10)
  expect_equal(as.numeric(tb$kappa[1]), 0.70, tolerance = 1e-10)
  expect_equal(as.numeric(tb$kappa[1]),
               unname(vcd::Kappa(table(d$R1, d$R2))$Unweighted["value"]), tolerance = 1e-10)
})

test_that("VAL-agreement-C03 the kappa interval uses the NON-NULL ASE", {
  # irr::kappa2()$statistic is the z for H0: kappa = 0, so kappa/z is the NULL SE and an
  # interval built on it does not describe this estimate. The correct ASE comes from
  # vcd::Kappa, which agrees with psych::cohen.kappa's sqrt(var.kappa) to ~1e-16.
  # psych clamps the upper limit at 1, and so does the module: kappa is bounded.
  skip_if_not_installed("vcd"); skip_if_not_installed("psych")
  d <- data.frame(
    R1 = factor(rep(c("pos","neg","pos","neg"), c(9,1,2,8)), levels = c("pos","neg")),
    R2 = factor(rep(c("pos","pos","neg","neg"), c(9,1,2,8)), levels = c("pos","neg")))
  tab <- table(d$R1, d$R2)
  vk  <- vcd::Kappa(tab)
  ase <- unname(vk$Unweighted["ASE"])
  ck  <- suppressWarnings(psych::cohen.kappa(tab))
  expect_equal(ase, sqrt(ck$var.kappa), tolerance = 1e-8)   # two independent oracles agree
  z <- stats::qnorm(0.975)
  r  <- suppressWarnings(suppressMessages(agreement(data = d, vars = c("R1","R2"))))
  tb <- r$irrtable$asDF
  expect_equal(as.numeric(tb$ci_lower[1]), 0.70 - z * ase, tolerance = 1e-6)
  expect_equal(as.numeric(tb$ci_upper[1]), min(1, 0.70 + z * ase), tolerance = 1e-6)
})

test_that("VAL-agreement-C06 Krippendorff's alpha reproduces the published worked example", {
  # Krippendorff's own "C" data, 4 observers x 12 units, as shipped in ?irr::kripp.alpha.
  # Published (Krippendorff 2011; Hayes & Krippendorff 2007):
  #   nominal .743   ordinal .815   interval .849   ratio .797
  skip_if_not_installed("irr")
  nmm <- matrix(c(1,1,NA,1,2,2,3,2,3,3,3,3,3,3,3,3,2,2,2,2,1,2,3,4,4,4,4,4,
                  1,1,2,1,2,2,2,2,NA,5,5,5,NA,NA,1,1,NA,NA,3,NA), nrow = 4)
  published <- c(nominal = 0.743, ordinal = 0.815, interval = 0.849, ratio = 0.797)
  d <- as.data.frame(t(nmm)); names(d) <- paste0("Obs", 1:4)
  for (m in names(published)) {
    r <- suppressWarnings(suppressMessages(
      agreement(data = d, vars = names(d), kripp = TRUE, krippMethod = m)))
    expect_equal(round(as.numeric(r$krippTable$asDF$alpha[1]), 3), unname(published[[m]]),
                 tolerance = 1e-9, info = m)
  }
})

test_that("VAL-agreement-C07 Gwet's AC2 uses the factor level order, not alphabetical", {
  # irrCAC defaults categ.labels to sort(unique(values)). On a clinical scale whose order is
  # not alphabetical (Benign < Atypical < Malignant) that corrupts every weighted distance.
  # The module passes the declared factor levels (R/agreement.b.R ~:8550-8566).
  skip_if_not_installed("irrCAC")
  lvo <- c("Benign", "Atypical", "Malignant")
  set.seed(11); n <- 60
  g1 <- sample(lvo, n, TRUE, c(.5,.3,.2))
  g2 <- ifelse(stats::runif(n) < .75, g1, sample(lvo, n, TRUE))
  d  <- data.frame(R1 = factor(g1, levels = lvo, ordered = TRUE),
                   R2 = factor(g2, levels = lvo, ordered = TRUE))
  raw <- data.frame(R1 = as.character(d$R1), R2 = as.character(d$R2), stringsAsFactors = FALSE)
  correct <- irrCAC::gwet.ac1.raw(raw, weights = "linear", categ.labels = lvo)$est$coeff.val
  alpha_default <- irrCAC::gwet.ac1.raw(raw, weights = "linear")$est$coeff.val
  expect_false(isTRUE(all.equal(correct, alpha_default)))  # the trap is live for this fixture
  r <- suppressWarnings(suppressMessages(
    agreement(data = d, vars = c("R1","R2"), gwet = TRUE, gwetWeights = "linear")))
  expect_equal(round(as.numeric(r$gwetTable$asDF$coefficient[1]), 6),
               round(as.numeric(correct), 6), tolerance = 1e-4)
})

test_that("VAL-agreement-C08 ICC(2,1) matches psych::ICC", {
  # psych 2.x renamed the $results rows, so results["ICC2", ] is NA with no error. Index by
  # the `type` column instead.
  skip_if_not_installed("psych")
  set.seed(13); n <- 40
  tru <- stats::rnorm(n, 50, 10)
  d <- data.frame(R1 = tru + stats::rnorm(n, 0, 3),
                  R2 = tru + stats::rnorm(n, 0, 3),
                  R3 = tru + stats::rnorm(n, 0, 3))
  pi6 <- suppressWarnings(suppressMessages(psych::ICC(d)))$results
  icc21 <- pi6[pi6$type == "ICC2", "ICC"]
  expect_true(is.finite(icc21))
  r <- suppressWarnings(suppressMessages(
    agreement(data = d, vars = c("R1","R2","R3"), icc = TRUE, iccType = "icc21")))
  expect_equal(round(as.numeric(r$iccTable$asDF$icc_value[1]), 6), round(icc21, 6), tolerance = 1e-4)
})

test_that("VAL-agreement-C09 PABAK uses the scale category count, not the observed one", {
  # PABAK = (q*Po - 1)/(q - 1). With Po = .80 on a FOUR-level scale of which only three grades
  # are used, q = 4 gives .7333; taking q from the observed categories gives .70 - a shift that
  # can cross a Landis & Koch band.
  lv4 <- c("G1","G2","G3","G4")
  d <- data.frame(
    R1 = factor(c(rep("G1", 80), rep("G2", 20)), levels = lv4, ordered = TRUE),
    R2 = factor(c(rep("G1", 80), rep("G4", 20)), levels = lv4, ordered = TRUE))
  r <- suppressWarnings(suppressMessages(
    agreement(data = d, vars = c("R1","R2"), pabak = TRUE)))
  tb <- r$pabakTable$asDF
  expect_equal(as.numeric(tb$observedAgreement[1]), 0.80, tolerance = 1e-9)
  expect_equal(round(as.numeric(tb$pabak[1]), 6), round((4 * 0.80 - 1) / 3, 6), tolerance = 1e-5)
})

test_that("VAL-agreement-C02 weighted kappa is on the declared ordinal scale, and refuses nominal", {
  # vcd::Kappa(weights = "Fleiss-Cohen") is the independent oracle for squared weights.
  # The fixture is the realistic failure case: rater 2 never assigns the middle grade, so the
  # two columns' observed level sets differ.
  skip_if_not_installed("vcd")
  lv <- c("G1","G2","G3")
  set.seed(7); n <- 80
  a  <- factor(sample(lv, n, TRUE, c(.35,.30,.35)), levels = lv)
  bc <- ifelse(as.character(a) == "G2", sample(c("G1","G3"), n, TRUE), as.character(a))
  bc[sample(n, 8)] <- sample(c("G1","G3"), 8, TRUE)
  d  <- data.frame(R1 = factor(a, levels = lv, ordered = TRUE),
                   R2 = factor(bc, levels = lv, ordered = TRUE))
  expected <- unname(vcd::Kappa(table(d$R1, d$R2), weights = "Fleiss-Cohen")$Weighted["value"])
  r <- suppressWarnings(suppressMessages(
    agreement(data = d, vars = c("R1","R2"), wght = "squared")))
  expect_equal(as.numeric(r$irrtable$asDF$kappa[1]), expected, tolerance = 1e-6)

  # and weighted kappa must REFUSE unordered factors rather than rank level codes
  dn <- data.frame(R1 = factor(a, levels = lv), R2 = factor(bc, levels = lv))
  expect_error(
    suppressWarnings(suppressMessages(
      agreement(data = dn, vars = c("R1","R2"), wght = "squared"))),
    regexp = "[Oo]rdinal")
})

test_that("VAL-agreement-C01 metamorphic invariants hold", {
  d <- data.frame(
    R1 = factor(rep(c("pos","neg","pos","neg"), c(9,1,2,8)), levels = c("pos","neg")),
    R2 = factor(rep(c("pos","pos","neg","neg"), c(9,1,2,8)), levels = c("pos","neg")))
  # do.call, not `vars = v`: jmvcore resolves `vars` with non-standard evaluation, so a bare
  # symbol is taken as a COLUMN NAME and the call dies with "invalid 'row.names' length".
  k <- function(dd, v = c("R1","R2"))
    as.numeric(suppressWarnings(suppressMessages(
      do.call(agreement, list(data = dd, vars = v))))$irrtable$asDF$kappa[1])
  base <- k(d)
  set.seed(99)
  expect_equal(k(d[sample(nrow(d)), ]), base, tolerance = 1e-12)   # row order irrelevant
  expect_equal(k(d[, c("R2","R1")], c("R2","R1")), base, tolerance = 1e-12)  # raters swap
  dr <- d; levels(dr$R1) <- c("P","N"); levels(dr$R2) <- c("P","N")
  expect_equal(k(dr), base, tolerance = 1e-12)                     # relabelling, same order
  expect_equal(k(rbind(d, d)), base, tolerance = 1e-10)            # duplication
})

# ==============================================================================
# Added by /validate-function agreement depth=exhaustive, 2026-09-23.
# Evidence: development-scripts/validate_agreement.R
# Report:   development-ideas/agreement-validation-2026-09-23.md
# Every live expectation below was verified against an INDEPENDENT oracle (hand
# arithmetic written out in the comment, a different package, or a published
# worked example). Open defects carry skip() with their finding ID.
# ==============================================================================

.val_LV <- c("Absent", "Focal", "Diffuse")
.val_ff <- function(x) factor(x, levels = .val_LV, ordered = TRUE)
.val_q  <- function(e) suppressWarnings(suppressMessages(force(e)))
# Cohen kappa and its non-null ASE (Fleiss, Cohen & Everitt 1969, eq. 12-13)
.val_hand_kappa <- function(tab, w = NULL) {
  p <- tab / sum(tab); k <- nrow(p)
  if (is.null(w)) w <- diag(k)
  Po <- sum(w * p); Pe <- sum(w * outer(rowSums(p), colSums(p)))
  unname((Po - Pe) / (1 - Pe))
}
.val_hand_ase <- function(tab) {
  n <- sum(tab); p <- tab / n; k <- nrow(p)
  pi_ <- rowSums(p); p_j <- colSums(p)
  Po <- sum(diag(p)); Pe <- sum(pi_ * p_j)
  t1 <- sum(diag(p) * ((1 - Pe) - (pi_ + p_j) * (1 - Po))^2)
  t2 <- 0
  for (i in seq_len(k)) for (j in seq_len(k)) if (i != j)
    t2 <- t2 + p[i, j] * (p_j[i] + pi_[j])^2
  unname(sqrt((t1 + t2 * (1 - Po)^2 - (Po * Pe - 2 * Pe + Po)^2) / (n * (1 - Pe)^4)))
}
.val_F3 <- local({
  d <- data.frame(
    r1 = c("Absent","Absent","Focal","Diffuse","Focal","Absent","Focal","Diffuse","Absent","Focal",
           "Diffuse","Focal","Absent","Absent","Focal","Diffuse","Focal","Absent","Diffuse","Focal"),
    r2 = c("Absent","Focal","Focal","Diffuse","Focal","Absent","Absent","Diffuse","Absent","Focal",
           "Focal","Focal","Absent","Focal","Focal","Diffuse","Diffuse","Absent","Diffuse","Focal"),
    r3 = c("Absent","Absent","Diffuse","Diffuse","Focal","Focal","Focal","Diffuse","Absent","Absent",
           "Diffuse","Focal","Absent","Absent","Focal","Diffuse","Focal","Focal","Diffuse","Focal"),
    stringsAsFactors = FALSE)
  d[] <- lapply(d, .val_ff); d })

test_that("VAL-agreement-C10 Fleiss' kappa and its H0 test match the published formulas", {
  # Fleiss (1971) kappa; H0 variance from Fleiss, Levin & Paik (2003) eq. 18.17.
  # Verified separately to reduce exactly to Cohen's null ASE^2 at m = 2.
  d <- .val_F3
  lv <- sort(unique(unlist(lapply(d, as.character))))
  n <- nrow(d); m <- ncol(d)
  nij <- t(apply(d, 1, function(r) table(factor(as.character(r), levels = lv))))
  Pbar <- mean((rowSums(nij^2) - m) / (m * (m - 1)))
  pj <- colSums(nij) / (n * m); Pe <- sum(pj^2)
  kap <- (Pbar - Pe) / (1 - Pe)
  tb <- .val_q(agreement(data = d, vars = names(d)))$irrtable$asDF
  expect_equal(as.numeric(tb$kappa[1]), kap, tolerance = 1e-9)
  # CONTRACT CHANGED 2026-09-24: the headline 3+ rater kappa moved from
  # irr::kappam.fleiss to irrCAC (every observed rating used), so z is no longer
  # the Fleiss-Levin-Paik null-variance statistic but the Wald ratio kappa / SE,
  # with Gwet's (2008) subject-sampling SE, raters fixed:
  #   kappa_i* = (pa_i - pe)/(1 - pe) - 2 (1 - kappa)(pe_i - pe)/(1 - pe),
  #   pe_i = sum_k (n_ik / m) pj_k,  Var = sum_i (kappa_i* - kappa)^2 / (n (n - 1)).
  # irrCAC rounds the SE to 5 dp before the module sees it, hence tolerance 1e-4.
  pa_i <- (rowSums(nij^2) - m) / (m * (m - 1))
  pe_i <- as.vector((nij / m) %*% pj)
  k_i <- (pa_i - Pe) / (1 - Pe) - 2 * (1 - kap) * (pe_i - Pe) / (1 - Pe)
  se <- sqrt(sum((k_i - kap)^2) / (n * (n - 1)))
  expect_equal(as.numeric(tb$z[1]), kap / se, tolerance = 1e-4)
  expect_equal(as.numeric(tb$p[1]), 2 * stats::pt(-abs(kap / se), n - 1), tolerance = 1e-3)
})

test_that("VAL-agreement-C11 Agreement % with 3+ raters is the ALL-rater unanimity rate", {
  # irr::agree(tolerance = 0) counts only cases every rater matched on. A two-rater
  # fixture cannot distinguish this from the pairwise match rate, which is how the
  # 2026-09-21 run missed it; this one has three raters.
  d <- .val_F3
  expect_equal(as.numeric(.val_q(agreement(data = d, vars = names(d)))$irrtable$asDF$peragree[1]),
               100 * mean(apply(d, 1, function(r) length(unique(r)) == 1L)), tolerance = 1e-12)
})

test_that("VAL-agreement-C12 Gwet's AC1/AC2 match the hand formula on the declared scale", {
  skip_if_not_installed("irrCAC")
  # Gwet (2008; 2014 Handbook 4e ch.3): p_a = mean_i sum_k r_ik (r*_ik - 1)/(r_i(r_i-1)),
  # p_e = (T_w/(q(q-1))) sum_k pi_k (1 - pi_k), AC = (p_a - p_e)/(1 - p_e).
  d <- .val_F3; lv <- .val_LV; q <- 3L
  R <- t(apply(d, 1, function(r) table(factor(as.character(r), levels = lv))))
  ri <- rowSums(R); pik <- colMeans(R / ri)
  hand <- function(W) {
    pa <- mean(rowSums(R * (R %*% t(W) - 1)) / (ri * (ri - 1)))
    pe <- (sum(W) / (q * (q - 1))) * sum(pik * (1 - pik))
    (pa - pe) / (1 - pe)
  }
  Wl <- outer(1:q, 1:q, function(a, b) 1 - abs(a - b) / (q - 1))
  # irrCAC rounds coeff.val to 4 dp before the module sees it, hence tolerance 1e-4.
  expect_equal(as.numeric(.val_q(agreement(data = d, vars = names(d), gwet = TRUE,
                 gwetWeights = "unweighted"))$gwetTable$asDF$coefficient[1]),
               hand(diag(q)), tolerance = 1e-4)
  expect_equal(as.numeric(.val_q(agreement(data = d, vars = names(d), gwet = TRUE,
                 gwetWeights = "linear"))$gwetTable$asDF$coefficient[1]),
               hand(Wl), tolerance = 1e-4)
})

test_that("VAL-agreement-C13 Kendall's W is tie-corrected and its chi-square follows", {
  # W = 12S / (m^2(n^3 - n) - m*sum(T)), T = sum(t^3 - t) per rater (Kendall 1948);
  # chi2 = m(n-1)W on n-1 df.
  d <- .val_F3
  M <- vapply(d, as.numeric, numeric(nrow(d)))
  Rs <- rowSums(apply(M, 2, rank)); n <- nrow(M); m <- ncol(M)
  S <- sum((Rs - mean(Rs))^2)
  Tc <- sum(vapply(seq_len(m), function(j) { tt <- table(M[, j]); sum(tt^3 - tt) }, numeric(1)))
  tw <- .val_q(agreement(data = d, vars = names(d), kendallW = TRUE))$kendallWTable$asDF
  expect_equal(as.numeric(tw$w[1]), 12 * S / (m^2 * (n^3 - n) - m * Tc), tolerance = 1e-9)
  expect_equal(as.numeric(tw$chisq[1]), m * (n - 1) * as.numeric(tw$w[1]), tolerance = 1e-9)
  expect_equal(as.numeric(tw$df[1]), n - 1)
})

test_that("VAL-agreement-C14 specific agreement follows de Vet's definition", {
  # ps(k) = 2 * a_kk / (n_1k + n_2k)  (de Vet, Mokkink, Terwee et al. 2013)
  d <- .val_F3[, 1:2]
  tb <- table(d$r1, d$r2)
  sa <- .val_q(agreement(data = d, vars = names(d),
                         specificAgreement = TRUE))$specificAgreementTable$asDF
  for (i in seq_along(.val_LV)) {
    row <- sa[sa$category == .val_LV[i], , drop = FALSE]
    expect_equal(as.numeric(row$specific_agreement[1]),
                 2 * tb[i, i] / (sum(tb[i, ]) + sum(tb[, i])), tolerance = 1e-10)
    expect_equal(as.numeric(row$n_total_positive[1]), sum(tb[i, ]) + sum(tb[, i]))
  }
})

test_that("VAL-agreement-C15 per-class precision, recall and F1 follow from the 2x2", {
  d <- .val_F3[, 1:2]
  tb <- table(d$r1, d$r2)
  pc <- .val_q(agreement(data = d, vars = names(d),
                         confusionMatrix = TRUE))$perClassMetricsTable$asDF
  for (i in seq_along(.val_LV)) {
    row <- pc[pc$class_label == .val_LV[i], , drop = FALSE]
    prec <- tb[i, i] / sum(tb[, i]); rec <- tb[i, i] / sum(tb[i, ])
    expect_equal(as.numeric(row$precision[1]), prec, tolerance = 1e-10)
    expect_equal(as.numeric(row$recall[1]),    rec,  tolerance = 1e-10)
    expect_equal(as.numeric(row$f1[1]), 2 * prec * rec / (prec + rec), tolerance = 1e-10)
    expect_equal(as.numeric(row$support[1]), sum(tb[i, ]))
  }
})

test_that("VAL-agreement-C16 all-pairs kappa carries the non-null ASE interval", {
  d <- .val_F3
  ap <- .val_q(agreement(data = d, vars = names(d), allPairsKappa = TRUE))$allPairsKappaTable$asDF
  key <- function(k) ap[gsub('^"|"$', "", rownames(ap)) == k, , drop = FALSE]
  for (ij in utils::combn(3, 2, simplify = FALSE)) {
    tb <- table(factor(as.character(d[[ij[1]]]), levels = .val_LV),
                factor(as.character(d[[ij[2]]]), levels = .val_LV))
    row <- key(paste0(ij[1], "_", ij[2]))   # All-Pairs rows are keyed by column position
    expect_equal(as.numeric(row$kappa[1]), .val_hand_kappa(tb), tolerance = 1e-9)
    expect_equal(as.numeric(row$se[1]),    .val_hand_ase(tb),   tolerance = 1e-8)
    expect_equal(c(as.numeric(row$ci_lower[1]), as.numeric(row$ci_upper[1])),
                 .val_hand_kappa(tb) + c(-1, 1) * qnorm(0.975) * .val_hand_ase(tb),
                 tolerance = 1e-8)
  }
})

test_that("VAL-agreement-C17 Bland-Altman statistics are the hand values and mirror on swap", {
  d <- data.frame(a = c(5,12,30,45,8,22,60,15,35,70,3,50,28,18,40),
                  b = c(7,10,34,41,12,20,58,19,30,66,6,55,25,21,44))
  dif <- d$a - d$b
  ba <- .val_q(agreement(data = d, vars = c("a","b"), blandAltmanPlot = TRUE))$blandAltmanStats$asDF
  expect_equal(as.numeric(ba$meanDiff[1]), mean(dif), tolerance = 1e-10)
  expect_equal(as.numeric(ba$sdDiff[1]),   sd(dif),   tolerance = 1e-10)
  expect_equal(as.numeric(ba$lowerLoA[1]), mean(dif) - qnorm(0.975) * sd(dif), tolerance = 1e-8)
  expect_equal(as.numeric(ba$upperLoA[1]), mean(dif) + qnorm(0.975) * sd(dif), tolerance = 1e-8)
  ba2 <- .val_q(agreement(data = d[, c(2,1)], vars = c("b","a"),
                          blandAltmanPlot = TRUE))$blandAltmanStats$asDF
  expect_equal(as.numeric(ba2$meanDiff[1]), -as.numeric(ba$meanDiff[1]), tolerance = 1e-12)
  expect_equal(c(as.numeric(ba2$lowerLoA[1]), as.numeric(ba2$upperLoA[1])),
               c(-as.numeric(ba$upperLoA[1]), -as.numeric(ba$lowerLoA[1])), tolerance = 1e-12)
})

test_that("VAL-agreement-C18 Lin's CCC matches the 1989 definition", {
  # rho_c = 2 s_xy / (s_x^2 + s_y^2 + (mx - my)^2), n-divisor moments (Lin 1989 eq. 2)
  d <- data.frame(a = c(5,12,30,45,8,22,60,15,35,70,3,50,28,18,40),
                  b = c(7,10,34,41,12,20,58,19,30,66,6,55,25,21,44))
  x <- d$a; y <- d$b
  ccc <- 2 * mean((x - mean(x)) * (y - mean(y))) /
         (mean((x - mean(x))^2) + mean((y - mean(y))^2) + (mean(x) - mean(y))^2)
  tc <- .val_q(agreement(data = d, vars = c("a","b"), linCCC = TRUE))$linCCCTable$asDF
  expect_equal(as.numeric(tc$ccc[1]), ccc, tolerance = 1e-8)
  expect_equal(as.numeric(tc$pearson_r[1]), cor(x, y), tolerance = 1e-10)
  expect_equal(as.numeric(tc$ccc[1]),
               as.numeric(tc$pearson_r[1]) * as.numeric(tc$bias_factor[1]), tolerance = 1e-6)
})

test_that("VAL-agreement-C19 the kappa interval scales as the theory requires", {
  d <- .val_F3[, 1:2]
  k1 <- .val_q(agreement(data = d, vars = names(d)))$irrtable$asDF
  # duplicating every row leaves kappa alone and divides the half-width by sqrt(2)
  k2 <- .val_q(agreement(data = rbind(d, d), vars = names(d)))$irrtable$asDF
  expect_equal(as.numeric(k2$kappa[1]), as.numeric(k1$kappa[1]), tolerance = 1e-10)
  expect_equal((as.numeric(k1$ci_upper[1]) - as.numeric(k1$kappa[1])) /
               (as.numeric(k2$ci_upper[1]) - as.numeric(k2$kappa[1])), sqrt(2), tolerance = 1e-8)
  # 90 vs 95 half-widths are in the ratio qnorm(.95)/qnorm(.975)
  k90 <- .val_q(agreement(data = d, vars = names(d), confLevel = 0.90))$irrtable$asDF
  expect_equal((as.numeric(k90$ci_upper[1]) - as.numeric(k90$kappa[1])) /
               (as.numeric(k1$ci_upper[1])  - as.numeric(k1$kappa[1])),
               qnorm(.95) / qnorm(.975), tolerance = 1e-8)
})

test_that("VAL-agreement-C20 bootstraps are seeded and do not disturb the caller's RNG", {
  d <- .val_F3[, 1:2]
  g <- function(s) .val_q(agreement(data = d, vars = names(d), bootstrapCI = TRUE,
                                    nBoot = 100, seed = s))$bootstrapCITable$asDF
  a <- g(42); b <- g(42); cc <- g(43)
  expect_identical(a$ci_lower, b$ci_lower)
  expect_identical(as.numeric(a$estimate), as.numeric(cc$estimate))   # point estimates seed-free
  expect_false(isTRUE(all.equal(a$ci_lower, cc$ci_lower)))
  # a bare set.seed() inside the analysis would leak into the jamovi engine
  set.seed(99); before <- runif(1)
  set.seed(99); invisible(g(7)); after <- runif(1)
  expect_identical(after, before)
})

test_that("VAL-agreement-C21 case identity survives a jamovi filter", {
  # Output$setValues() carries no row numbers; the analysis calls setRowNums(rownames(ratings)),
  # and the same source drives the visible Case column.
  set.seed(5); n <- 40
  d <- data.frame(r1 = .val_ff(sample(.val_LV, n, TRUE)),
                  r2 = .val_ff(sample(.val_LV, n, TRUE)),
                  r3 = .val_ff(sample(.val_LV, n, TRUE)))
  det <- .val_q(agreement(data = d[-(1:20), ], vars = c("r1","r2","r3"),
                          loaVariable = TRUE, showLoaTable = TRUE))$loaDetailTable$asDF
  expect_identical(as.character(det$case_id), as.character(21:40))
})

# ------------------------- open defects (skip-guarded) ------------------------

test_that("VAL-agreement-03 whole-number measurements are not routed to Cohen's kappa", {
  # Fixed: the detector asks whether the VALUES could be a rating scale (integers
  # of magnitude <= 10) instead of counting distinct values. Ki-67 percentages
  # reach 70, so they route to ICC/CCC and no longer get kappa = -0.0090.
  d <- data.frame(pathA = c(5,12,30,45,8,22,60,15,35,70,3,50,28,18,40),
                  pathB = c(7,10,34,41,12,20,58,19,30,66,6,55,25,21,44))
  r <- .val_q(agreement(data = d, vars = names(d)))
  expect_false(grepl("Cohen's Kappa", r$irrtable$asDF$method[1]))
})

test_that("VAL-agreement-04 intra-rater kappa honours the Weighting option", {
  set.seed(31); n <- 50
  d <- data.frame(A_1 = .val_ff(sample(.val_LV, n, TRUE)), A_2 = .val_ff(sample(.val_LV, n, TRUE)),
                  B_1 = .val_ff(sample(.val_LV, n, TRUE)), B_2 = .val_ff(sample(.val_LV, n, TRUE)),
                  check.names = FALSE)
  tA <- table(d$A_1, d$A_2)
  it <- .val_q(agreement(data = d, vars = names(d), interIntraRater = TRUE,
                         interIntraSeparator = "_"))$interIntraRaterIntraTable$asDF
  expect_equal(as.numeric(it$value[1]), .val_hand_kappa(tA), tolerance = 1e-8)
  # The divergence the defect caused: this table returned the quadratic kappa while
  # the headline table returned whatever was selected. Pin the weighted branch too.
  wq <- outer(seq_along(.val_LV), seq_along(.val_LV),
              function(i, j) 1 - (i - j)^2 / (length(.val_LV) - 1)^2)
  itQ <- .val_q(agreement(data = d, vars = names(d), interIntraRater = TRUE,
                          interIntraSeparator = "_", wght = "squared"))$interIntraRaterIntraTable$asDF
  expect_equal(as.numeric(itQ$value[1]), .val_hand_kappa(tA, wq), tolerance = 1e-8)
})

test_that("VAL-agreement-05 the Landis-Koch band is applied to the unrounded kappa", {
  tb <- matrix(c(29, 12, 20, 339), 2, byrow = TRUE)   # kappa = 0.599774873366
  lv <- c("Neg", "Pos")
  d <- do.call(rbind, lapply(1:2, function(i) do.call(rbind, lapply(1:2, function(j)
    if (tb[i, j] > 0) data.frame(r1 = rep(lv[i], tb[i, j]), r2 = rep(lv[j], tb[i, j])) else NULL))))
  d[] <- lapply(d, function(x) factor(x, levels = lv, ordered = TRUE))
  s <- .val_q(agreement(data = d, vars = c("r1","r2"), showSummary = TRUE))$summary$content
  expect_true(nzchar(s))   # an empty panel would pass every expect_false below
  expect_false(grepl("substantial or higher", s, fixed = TRUE))
  expect_true(grepl("A kappa between 0.40 and 0.60", s, fixed = TRUE))
  expect_true(grepl("moderate agreement", s, fixed = TRUE))
})

test_that("VAL-agreement-06 hierarchical kappa fills what it can on categorical ratings and explains the rest", {
  skip_if_not_installed("vcd")
  set.seed(21); n <- 120
  d <- data.frame(r1 = .val_ff(sample(.val_LV, n, TRUE)), r2 = .val_ff(sample(.val_LV, n, TRUE)),
                  lab = factor(rep(paste0("Lab", 1:4), each = 30)))
  r <- .val_q(agreement(data = d, vars = c("r1","r2"), hierarchicalKappa = TRUE,
                        clusterVariable = "lab"))
  nn <- function(t) length(t$notes) > 0
  # Until 2026-09-24 categorical ratings filled none of the five tables and this test
  # only asked that each say why. The categorical route now FILLS the overall
  # (stratified kappa) and per-cluster tables; the three that need a variance
  # decomposition cannot be filled for a three-category scale and must still say why -
  # the full set, so a regression that drops one explanation cannot pass.
  cs <- r$clusterSpecificTable$asDF
  expect_equal(nrow(cs), 4L)
  per <- vapply(split(d, d$lab), function(x) vcd::Kappa(table(x$r1, x$r2))$Unweighted[["value"]], 0)
  expect_equal(unname(cs$kappa), unname(per[as.character(cs$cluster)]), tolerance = 1e-8)
  expect_true(is.finite(r$hierarchicalOverallTable$asDF$overall_kappa[1]))
  expect_true(nn(r$hierarchicalOverallTable))
  expect_true(nn(r$varianceDecompositionTable))
  expect_true(nn(r$hierarchicalICCTable))
  expect_true(nn(r$homogeneityTestTable))
})

test_that("VAL-agreement-07 the hierarchical overall value is labelled as the ICC it is", {
  set.seed(21); n <- 120
  d <- data.frame(r1 = rnorm(n, 50, 15), r2 = NA_real_,
                  lab = factor(rep(paste0("Lab", 1:4), each = 30)))
  d$r2 <- d$r1 + rnorm(n, 0, 5)
  r <- .val_q(agreement(data = d, vars = c("r1","r2"), hierarchicalKappa = TRUE,
                        clusterVariable = "lab"))
  tbl <- r$hierarchicalOverallTable
  # The cell is ICC(2,1) within institution, sigma2_case / (sigma2_case + sigma2_rater
  # + sigma2_resid) (changed 2026-09-24 from sigma2_case / sigma2_total; the formula
  # itself is pinned in test-agreement-fixes-2026-09-24.R). Neither the table title
  # nor the value's column title may call it a kappa.
  expect_false(is.na(tbl$asDF$overall_kappa[1]))
  expect_false(grepl("kappa", tbl$title, ignore.case = TRUE))
  expect_false(grepl("kappa", tbl$getColumn("overall_kappa")$title, ignore.case = TRUE))
  expect_true(grepl("ICC", tbl$getColumn("overall_kappa")$title, fixed = TRUE))
  # No interval is computed for this variance ratio, so the table declares no CI
  # column at all (they used to be declared and then hidden at run time).
  expect_false(any(c("ci_lower", "ci_upper") %in% names(tbl$columns)))  # declared, hidden or not
})

test_that("VAL-agreement-08 a dominating case is disclosed on every continuous measure", {
  # The remedy is disclosure, not suppression: ICC = 0.999999 IS the correct ICC of
  # data containing a 99999, so the number stays and the analysis says what produced
  # it. Screen: share of the between-case sum of squares carried by the most deviant
  # case, relative to its attainable maximum (n-1)/n, flagged above 0.95. Measured
  # 100% on this sentinel at every n from 10 to 300, and <= 1% on clean log-normal
  # markers from n = 20 up.
  a <- c(5,12,30,45,8,22,60,15,35,70,3,50,28,18,40,25,55,10,33,48)
  b <- c(30,45,8,60,35,5,12,50,15,22,40,18,70,3,25,55,10,48,28,33)
  d <- data.frame(pathA = a, pathB = b); d$pathA[1] <- 99999; d$pathB[1] <- 99999
  r <- .val_q(agreement(data = d, vars = names(d), icc = TRUE, linCCC = TRUE,
                        robinsonA = TRUE, meanPearson = TRUE))
  ntxt <- function(tb) paste(vapply(tb$notes, function(n) n$note, character(1)), collapse = " || ")
  for (tb in list(r$iccTable, r$linCCCTable, r$robinsonATable, r$meanPearsonTable)) {
    txt <- ntxt(tb)
    expect_true(nzchar(txt))                      # an unnoted table passes grepl() vacuously
    expect_true(grepl("between-case variance", txt, fixed = TRUE))
    expect_true(grepl("99999", txt, fixed = TRUE))   # it names the offending value
  }
  # ...and stays silent on clean data with the same raters.
  d2 <- data.frame(pathA = a, pathB = b)
  r2 <- .val_q(agreement(data = d2, vars = names(d2), icc = TRUE))
  expect_false(grepl("between-case variance", ntxt(r2$iccTable), fixed = TRUE))
})

test_that("VAL-agreement-09 ICC stays in [-1,1] and an impossible F is blanked, not shown", {
  # irr::icc loses the residual SS to cancellation at these magnitudes and returns
  # ICC = 1 + 2.22e-16 with F = -6.52e+15 and an inverted interval. .guardICCResult()
  # clamps the coefficient, blanks F and p, drops the inverted bounds and says so.
  skip_if_not_installed("irr")
  d <- data.frame(a = c(1e-12, 1, 1e12, 5, 7, 9, 11, 13),
                  b = c(1e-12, 1, 1e12, 5, 7, 9, 11, 14))
  r <- .val_q(agreement(data = d, vars = c("a","b"), icc = TRUE))$iccTable
  ti <- r$asDF
  expect_lte(as.numeric(ti$icc_value[1]), 1)
  expect_gte(as.numeric(ti$icc_value[1]), -1)
  # A negative F is not displayed at all, and neither is the p derived from it.
  expect_true(is.na(ti$f_value[1]))
  expect_true(is.na(ti$p[1]))
  # The clamp is disclosed: a silent clamp is indistinguishable from a real 1.000.
  nt <- r$notes[["numeric"]]
  expect_false(is.null(nt))
  # Table$notes holds Note R6 objects, not strings: nzchar()/grepl() on the object
  # itself errors with "environments cannot be coerced to other types".
  expect_true(nzchar(nt$note))
  expect_true(grepl("Numerically unreliable", nt$note, fixed = TRUE))
})

test_that("VAL-agreement-12 the headline CI note names the SE that was actually used", {
  skip_if_not_installed("vcd"); skip_if_not_installed("irr")
  lv <- c("Neg", "Pos")
  d <- data.frame(r1 = factor(rep(c("Neg","Pos"), c(1, 19)), levels = lv),
                  r2 = factor(rep(c("Neg","Pos"), c(1, 19)), levels = lv))
  r <- .val_q(agreement(data = d, vars = c("r1","r2")))
  tb <- r$irrtable$asDF
  # the interval is built from the NULL SE, not vcd's ASE (which is 0 here)
  null_se <- local({ k <- irr::kappa2(d); k$value / k$statistic })
  expect_equal((as.numeric(tb$kappa[1]) - as.numeric(tb$ci_lower[1])) / qnorm(0.975),
               null_se, tolerance = 1e-8)
  notes <- vapply(r$irrtable$notes, function(n) n$note, character(1))
  # Assert the note EXISTS and names the fallback before asserting what it does
  # not say: with no notes at all, expect_false() below passes vacuously and the
  # guard is dead.
  expect_true(any(grepl("irr::kappa2 fallback", notes, fixed = TRUE)))
  expect_false(any(grepl("non-null asymptotic standard error", notes)))
})

test_that("VAL-agreement-13 the 95% kappa interval attains nominal coverage at realistic n", {
  skip("VAL-agreement-13 open defect: measured coverage is 0.9275 at n = 50 (R = 2000) and 0.8810 at n = 20 (R = 1000); the only guard fires below n = 30. Full simulation in development-scripts/validate_agreement.R (claims F01, F02). Remove this skip when fixed.")
  # A short confirmatory run; the authoritative one is in the validation script.
  P <- matrix(c(.22,.05,.02, .05,.22,.05, .02,.05,.32), 3, byrow = TRUE); P <- P / sum(P)
  ktrue <- local({ Po <- sum(diag(P)); Pe <- sum(rowSums(P) * colSums(P)); (Po - Pe) / (1 - Pe) })
  lv <- paste0("C", 1:3); R <- 400
  withr::local_seed(20260923)
  hit <- vapply(seq_len(R), function(i) {
    cnt <- rmultinom(1, 50, as.vector(t(P)))
    cells <- expand.grid(j = lv, i = lv, KEEP.OUT.ATTRS = FALSE)[, c("i","j")]
    dd <- cells[rep(seq_len(nrow(cells)), cnt), , drop = FALSE]
    dd <- data.frame(r1 = factor(as.character(dd$i), levels = lv, ordered = TRUE),
                     r2 = factor(as.character(dd$j), levels = lv, ordered = TRUE))
    tt <- .val_q(agreement(data = dd, vars = c("r1","r2")))$irrtable$asDF
    !is.na(tt$ci_lower[1]) && tt$ci_lower[1] <= ktrue && ktrue <= tt$ci_upper[1]
  }, logical(1))
  expect_gt(mean(hit), 0.95 - 3 * sqrt(0.95 * 0.05 / R))
})
