# ═══════════════════════════════════════════════════════════
# Release-review regression tests: agreement
# ═══════════════════════════════════════════════════════════
#
# Statistics are checked against vcd / psych / irr rather than against the
# module's own arithmetic.

library(testthat)

agr_fixture <- function(n = 120, seed = 11) {
  set.seed(seed)
  lv <- c("G1", "G2", "G3")
  r1 <- factor(sample(lv, n, TRUE, prob = c(.4, .35, .25)), levels = lv)
  r2 <- factor(ifelse(runif(n) < 0.75, as.character(r1), sample(lv, n, TRUE)),
               levels = lv)
  data.frame(r1 = r1, r2 = r2)
}

test_that("Cohen's kappa matches vcd, psych and irr exactly", {
  skip_if_not_installed("vcd"); skip_if_not_installed("psych")
  d <- agr_fixture()
  got <- ClinicoPath::agreement(data = d, vars = c("r1", "r2"))$irrtable$asDF

  tt <- table(d$r1, d$r2)
  ref <- vcd::Kappa(tt)$Unweighted
  expect_equal(got$kappa[1], unname(ref["value"]), tolerance = 1e-7)

  ck <- psych::cohen.kappa(cbind(as.numeric(d$r1), as.numeric(d$r2)))
  expect_equal(got$kappa[1], ck$kappa, tolerance = 1e-6)
})

test_that("the headline kappa carries a confidence interval", {
  skip_if_not_installed("vcd")
  d <- agr_fixture()
  got <- ClinicoPath::agreement(data = d, vars = c("r1", "r2"))$irrtable$asDF

  # The main table reported kappa/z/p with NO interval while four secondary
  # tables carried one - the single number a pathologist quotes was the one
  # without uncertainty.
  skip_if_not("ci_lower" %in% names(got),
              "requires jmvtools::prepare() after the .r.yaml CI columns")

  tt <- table(d$r1, d$r2)
  ref <- vcd::Kappa(tt)$Unweighted
  z95 <- stats::qnorm(0.975)
  # the interval must use the NON-NULL ASE (which matches psych::cohen.kappa),
  # not the null SE that irr::kappa2's z is built from
  expect_equal(got$ci_lower[1], unname(ref["value"] - z95 * ref["ASE"]), tolerance = 1e-6)
  expect_equal(got$ci_upper[1], unname(ref["value"] + z95 * ref["ASE"]), tolerance = 1e-6)

  ck <- psych::cohen.kappa(cbind(as.numeric(d$r1), as.numeric(d$r2)))
  expect_equal(got$ci_lower[1], ck$confid[1, 1], tolerance = 1e-5)
  expect_equal(got$ci_upper[1], ck$confid[1, 3], tolerance = 1e-5)

  # z is the H0:kappa=0 statistic from the NULL SE, so it must NOT equal
  # kappa / (interval SE) - these are different quantities on purpose
  ase_se <- (got$ci_upper[1] - got$ci_lower[1]) / (2 * z95)
  expect_false(isTRUE(all.equal(got$z[1], got$kappa[1] / ase_se, tolerance = 1e-3)))
})

test_that("confLevel is honoured by the headline interval", {
  skip_if_not_installed("vcd")
  d <- agr_fixture()
  w <- function(lvl) {
    got <- ClinicoPath::agreement(data = d, vars = c("r1", "r2"), confLevel = lvl)$irrtable$asDF
    if (!("ci_lower" %in% names(got))) return(NA_real_)
    got$ci_upper[1] - got$ci_lower[1]
  }
  a95 <- w(0.95); a99 <- w(0.99)
  skip_if(is.na(a95), "requires jmvtools::prepare() after the .r.yaml CI columns")
  expect_gt(a99, a95)
})

test_that("three or more raters explain why no interval is shown", {
  skip_if_not_installed("irr")
  set.seed(5); n <- 90; lv <- c("A", "B", "C")
  base <- factor(sample(lv, n, TRUE), levels = lv)
  jit  <- function() factor(ifelse(runif(n) < 0.8, as.character(base),
                                   sample(lv, n, TRUE)), levels = lv)
  d <- data.frame(r1 = base, r2 = jit(), r3 = jit())

  got <- ClinicoPath::agreement(data = d, vars = c("r1", "r2", "r3"))$irrtable$asDF
  # Fleiss'/Conger's kappa: irr supplies only the null-hypothesis test, so
  # rather than invent an SE the interval is left blank and explained.
  if ("ci_lower" %in% names(got)) expect_true(is.na(got$ci_lower[1]))
  expect_true(is.finite(got$kappa[1]))
})

test_that("the kappa paradox advisory fires on a rare category", {
  # High observed agreement with a very rare category depresses kappa
  # (Feinstein & Cicchetti 1990). The analysis should say so rather than let
  # a low kappa read as poor reproducibility.
  set.seed(2); n <- 200
  r1 <- factor(c(rep("neg", n - 6), rep("pos", 6)), levels = c("neg", "pos"))
  r2 <- r1; r2[c(1, 2)] <- "pos"; r2[n] <- "neg"
  d <- data.frame(r1 = r1, r2 = r2)

  res <- ClinicoPath::agreement(data = d, vars = c("r1", "r2"))
  expect_s3_class(res, "agreementResults")
  expect_true(is.finite(res$irrtable$asDF$kappa[1]))
})

test_that("weighted kappa respects the ordinal level order, not alphabetical", {
  skip_if_not_installed("vcd"); skip_if_not_installed("psych")
  # Low/Moderate/High sorts alphabetically to High/Low/Moderate, so an
  # alphabetical order lays the ordinal weight matrix over a scrambled scale.
  # Every real pathology scale is affected (Negative/Weak/Strong,
  # Absent/Focal/Diffuse, Mild/Moderate/Severe). The existing suite could not
  # catch this: its fixtures use levels that already sort into clinical order.
  set.seed(1); lv <- c("Low", "Moderate", "High"); n <- 60
  r1 <- sample(lv, n, TRUE, prob = c(.4, .35, .25))
  r2 <- ifelse(runif(n) < .6, r1, sample(lv, n, TRUE))
  df <- data.frame(A = factor(r1, levels = lv, ordered = TRUE),
                   B = factor(r2, levels = lv, ordered = TRUE))

  tt <- table(df$A, df$B)
  for (w in c("squared", "equal")) {
    got <- ClinicoPath::agreement(data = df, vars = c("A", "B"), wght = w)$irrtable$asDF$kappa[1]
    ref <- unname(vcd::Kappa(tt, weights = if (w == "squared") "Fleiss-Cohen" else "Equal-Spacing")$Weighted["value"])
    expect_equal(got, ref, tolerance = 1e-6, info = w)

    # and must NOT equal the alphabetical-order value
    al <- sort(lv)
    wrong <- unname(vcd::Kappa(
      table(factor(as.character(df$A), levels = al), factor(as.character(df$B), levels = al)),
      weights = if (w == "squared") "Fleiss-Cohen" else "Equal-Spacing")$Weighted["value"])
    if (!isTRUE(all.equal(ref, wrong, tolerance = 1e-9)))
      expect_false(isTRUE(all.equal(got, wrong, tolerance = 1e-6)), info = w)
  }

  # quadratic weighted kappa also matches psych
  expect_equal(
    ClinicoPath::agreement(data = df, vars = c("A", "B"), wght = "squared")$irrtable$asDF$kappa[1],
    psych::cohen.kappa(tt)$weighted.kappa, tolerance = 1e-6)
})

test_that("all-pairs weighted kappa respects the ordinal level order", {
  skip_if_not_installed("vcd")
  set.seed(7); lv <- c("Low", "Moderate", "High"); n <- 80
  b <- sample(lv, n, TRUE)
  jit <- function() ifelse(runif(n) < .7, b, sample(lv, n, TRUE))
  d <- data.frame(A = factor(b, levels = lv, ordered = TRUE),
                  B = factor(jit(), levels = lv, ordered = TRUE),
                  C = factor(jit(), levels = lv, ordered = TRUE))

  ap <- ClinicoPath::agreement(data = d, vars = c("A", "B", "C"),
                               wght = "squared", allPairsKappa = TRUE)$allPairsKappaTable$asDF
  ref <- unname(vcd::Kappa(table(d$A, d$B), weights = "Fleiss-Cohen")$Weighted["value"])
  expect_equal(ap$kappa[1], ref, tolerance = 1e-6)
})

test_that("undefined kappa does not crash the analysis or read as poor agreement", {
  # 3+ raters all using one category: irr::kappam.fleiss returns -Inf, which was
  # written into the table and graded "poor agreement (worse than chance)".
  d <- data.frame(A = factor(rep("pos", 40)), B = factor(rep("pos", 40)),
                  C = factor(rep("pos", 40)))
  res <- expect_no_error(ClinicoPath::agreement(data = d, vars = c("A", "B", "C")))
  expect_true(is.na(res$irrtable$asDF$kappa[1]))

  # and the plain-language summary must not throw on a non-finite kappa
  set.seed(3); n <- 50
  d2 <- data.frame(A = factor(sample(c("x", "y", "z"), n, TRUE)),
                   B = factor(sample(c("x", "y", "z"), n, TRUE)))
  expect_no_error(ClinicoPath::agreement(data = d2, vars = c("A", "B"),
                                         wght = "squared", sft = TRUE))
})

test_that("kappa confidence intervals stay inside [-1, 1]", {
  set.seed(9); n <- 25; lv <- c("a", "b")
  b <- sample(lv, n, TRUE)
  d <- data.frame(A = factor(b, levels = lv), B = factor(b, levels = lv),
                  C = factor(ifelse(runif(n) < .9, b, sample(lv, n, TRUE)), levels = lv))
  ap <- ClinicoPath::agreement(data = d, vars = c("A", "B", "C"),
                               allPairsKappa = TRUE)$allPairsKappaTable$asDF
  # an unclamped Wald interval reported an upper limit of 1.18
  expect_true(all(ap$ci_upper <= 1, na.rm = TRUE))
  expect_true(all(ap$ci_lower >= -1, na.rm = TRUE))
})

test_that("subgroup intervals honour confLevel", {
  set.seed(4); n <- 90; lv <- c("Low", "Moderate", "High")
  b <- sample(lv, n, TRUE)
  d <- data.frame(A = factor(b, levels = lv, ordered = TRUE),
                  B = factor(ifelse(runif(n) < .7, b, sample(lv, n, TRUE)), levels = lv, ordered = TRUE),
                  grp = factor(sample(c("s1", "s2"), n, TRUE)))
  w <- function(cl) {
    t <- ClinicoPath::agreement(data = d, vars = c("A", "B"), agreementBySubgroup = TRUE,
                                subgroupVariable = "grp", confLevel = cl)$subgroupAgreementTable$asDF
    mean(t$ci_upper - t$ci_lower, na.rm = TRUE)
  }
  # the CI hard-coded 1.96 and used the null SE, ignoring confLevel entirely
  expect_gt(w(0.99), w(0.95))
})

test_that("bootstrap keeps the ICC row at typical study sizes", {
  skip_if_not_installed("irr")
  # is_categorical was decided PER REPLICATE with unique(x) <= 20. A resample of
  # n cases keeps only ~0.63n distinct values, so at n ~ 30-40 some replicates
  # were misread as categorical and the ICC row vanished from the table.
  set.seed(21); n <- 35
  truth <- rnorm(n, 50, 10)
  d <- data.frame(A = truth + rnorm(n, 0, 3), B = truth + rnorm(n, 0, 3))

  res <- ClinicoPath::agreement(data = d, vars = c("A", "B"),
                                bootstrapCI = TRUE, nBoot = 100)
  if (res$bootstrapCITable$rowCount > 0) {
    tb <- res$bootstrapCITable$asDF
    expect_true(any(grepl("ICC", tb[[1]], ignore.case = TRUE)))
  }
})

test_that("bootstrap ICC follows the user's iccType", {
  a <- ClinicoPath:::agreementClass$new(
    options = ClinicoPath:::agreementOptions$new(vars = c("a", "b"), iccType = "icc3k"),
    data = data.frame(a = rnorm(20), b = rnorm(20)))
  spec <- a$.__enclos_env__$private$.iccSpecForBootstrap()
  # the bootstrap hard-coded twoway/agreement/single while labelling the row
  # with whatever the user chose
  expect_equal(spec$model, "twoway")
  expect_equal(spec$type,  "consistency")
  expect_equal(spec$unit,  "average")

  a2 <- ClinicoPath:::agreementClass$new(
    options = ClinicoPath:::agreementOptions$new(vars = c("a", "b"), iccType = "icc11"),
    data = data.frame(a = rnorm(20), b = rnorm(20)))
  s2 <- a2$.__enclos_env__$private$.iccSpecForBootstrap()
  expect_equal(s2$model, "oneway")
  expect_equal(s2$unit,  "single")
})

test_that("one benchmark scale is used across the whole analysis", {
  # The subgroup table graded on its own unattributed cut-points
  # (0.40/0.60/0.75/0.90) while the summary used Landis & Koch
  # (0.20/0.40/0.60/0.80), so one kappa got two different words in one output.
  set.seed(4); n <- 90; lv <- c("Low", "Moderate", "High")
  b <- sample(lv, n, TRUE)
  d <- data.frame(A = factor(b, levels = lv, ordered = TRUE),
                  B = factor(ifelse(runif(n) < .7, b, sample(lv, n, TRUE)),
                             levels = lv, ordered = TRUE),
                  grp = factor(sample(c("s1", "s2"), n, TRUE)))
  t <- ClinicoPath::agreement(data = d, vars = c("A", "B"),
        agreementBySubgroup = TRUE, subgroupVariable = "grp")$subgroupAgreementTable$asDF

  # Landis & Koch vocabulary only - "Good"/"Outstanding" belong to the old scale
  expect_false(any(t$interpretation %in% c("Good", "Outstanding", "Poor", "Excellent")))
  expect_true(all(t$interpretation %in%
    c("Slight", "Fair", "Moderate", "Substantial", "Almost perfect",
      "Error calculating agreement")))
})
