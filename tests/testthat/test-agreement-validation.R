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
