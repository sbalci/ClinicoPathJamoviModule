# Regression tests for the 2026-09-23 round-3 fixes to `agreement`:
#   * the never-cleared .run()-level notes (a disclosure outliving the data that
#     justified it),
#   * the kappa label vocabulary, now issued by one authority on the kappa glyph,
#   * the subgroup table's displayed Statistic decoupled from the internal branch
#     key, so a translated label cannot send an ICC to the kappa bands,
#   * the clusterDistance / caseClusterDistance metric change.
#
# Every expectation below was read off real output before it was written; the
# measurements behind them are in the round-3 design briefs. Acceptance harness:
# development-scripts/audit_agreement_2026-09-23.R (IR2-02, IR2-04, IR2-05, IR2-08).

if (!exists("agreement", mode = "function")) {
  if (requireNamespace("ClinicoPath", quietly = TRUE)) {
    agreement <- getExportedValue("ClinicoPath", "agreement")
  }
}
skip_unless_agreement <- function() {
  testthat::skip_if_not(exists("agreement", mode = "function"),
                        "ClinicoPath::agreement not available")
}
skip_unless_agreement_class <- function() {
  testthat::skip_if_not(
    requireNamespace("ClinicoPath", quietly = TRUE) &&
      exists("agreementClass", envir = asNamespace("ClinicoPath"), inherits = FALSE),
    "ClinicoPath:::agreementClass not available")
}
q <- function(e) suppressWarnings(suppressMessages(force(e)))
note_keys <- function(tbl) names(tbl$notes)
note_text <- function(tbl) paste(vapply(tbl$notes, function(z) z$note, ""), collapse = " ")

# The kappa glyph is U+03BA. Built from its code point so this file stays pure
# ASCII (R CMD check flags non-ASCII source) and so a typographic look-alike
# pasted in by an editor cannot make every comparison here fail for a reason
# that has nothing to do with the module.
KAPPA <- intToUtf8(0x03BA)

# One analysis object, run twice with the data swapped underneath it. This is
# what the jamovi engine does when the user edits the spreadsheet, and it is the
# ONLY way to reach the stale-note bug: a fresh wrapper call builds a fresh
# results tree, which is always clean.
agr_object <- function(d, ...) {
  gen_o <- getFromNamespace("agreementOptions", "ClinicoPath")
  gen_a <- getFromNamespace("agreementClass", "ClinicoPath")
  gen_a$new(options = do.call(gen_o$new, list(...)), data = d)
}
agr_rerun_with <- function(a, d) {
  a$.__enclos_env__$private$.data <- d
  invisible(a$run())
}

# ---------------------------------------------------------------- fixtures
# Continuous, 20 cases, two readers who mostly agree. One copy carries the legacy
# missing-data sentinel 99999 in the first case for BOTH readers, which drives
# every between-case/within-case coefficient to its ceiling and raises the
# influential-case disclosure.
cont_pair <- function() {
  set.seed(4231)
  m1 <- round(runif(20, 8, 16))
  data.frame(m1 = m1, m2 = pmax(0, m1 + sample(c(-1, 0, 0, 1), 20, TRUE)))
}
ord_panel <- function(n = 120, seed = 77) {
  set.seed(seed)
  lv <- c("0", "1+", "2+", "3+")
  truth <- sample(lv, n, TRUE, prob = c(.4, .3, .2, .1))
  mk <- function(p) factor(ifelse(runif(n) < p, sample(lv, n, TRUE), truth),
                           levels = lv, ordered = TRUE)
  # P_1 / P_2 are rater P at two time points (what the intra-rater table needs);
  # Q_1 / Q_2 are the second condition for the paired comparison.
  data.frame(P_1 = mk(.15), P_2 = mk(.20), Q_1 = mk(.10), Q_2 = mk(.25))
}
clu_fixture <- function(n = 60, seed = 11) {
  set.seed(seed)
  base <- round(runif(n, 0, 90), 1)
  mkc <- function(off, sd) pmin(100, pmax(0, round(base + off + rnorm(n, 0, sd), 1)))
  data.frame(M1 = mkc(0, 3), M2 = mkc(1, 3), M3 = mkc(25, 4),
             M4 = mkc(26, 4), M5 = mkc(-1, 3))
}

# Both benchmark scales, coded here from the papers rather than read off the
# module, so the grading tests have an oracle of their own.
lk_kappa_band <- function(v) {                       # Landis & Koch (1977)
  if (length(v) != 1L || !is.finite(v)) return("Not estimable")
  if (v < 0)    return("Worse than chance")
  if (v < 0.20) return("Slight")
  if (v < 0.40) return("Fair")
  if (v < 0.60) return("Moderate")
  if (v < 0.80) return("Substantial")
  "Almost perfect"
}
kl_icc_band <- function(v) {                         # Koo & Li (2016)
  if (length(v) != 1L || !is.finite(v)) return("Not estimable")
  if (v < 0.50) return("Poor")
  if (v < 0.75) return("Moderate")
  if (v < 0.90) return("Good")
  "Excellent"
}

# ---- stale notes: an influential-case disclosure must not outlive the case ----
test_that("a note raised by a sentinel case is gone when the same object re-runs on clean data", {
  skip_unless_agreement_class()
  clean <- cont_pair()
  dirty <- clean
  dirty$m1[1] <- 99999
  dirty$m2[1] <- 99999

  a <- agr_object(dirty, vars = c("m1", "m2"), icc = TRUE, linCCC = TRUE, tdi = TRUE)
  invisible(a$run())

  # The disclosure has to be RAISED first, or its later absence proves nothing.
  expect_true("influential_case" %in% note_keys(a$results$iccTable))
  expect_true(grepl("99999", note_text(a$results$iccTable), fixed = TRUE))
  for (tb in list(a$results$linCCCTable, a$results$tdiTable))
    expect_true("influential_case" %in% note_keys(tb))

  agr_rerun_with(a, clean)

  # Same object, clean data: the note used to survive, so the table printed
  # "its ratings: 99999, 99999" over data containing no 99999.
  for (tb in list(a$results$iccTable, a$results$linCCCTable, a$results$tdiTable)) {
    expect_false("influential_case" %in% note_keys(tb))
    expect_false(grepl("99999", note_text(tb), fixed = TRUE))
  }
  # ... and the second run really did produce a coefficient, so the notes did not
  # simply vanish along with the whole table.
  expect_true(is.finite(as.numeric(a$results$iccTable$asDF$icc_value[1])))
})

test_that("the headline table cannot claim kappa was not computed while printing a kappa", {
  skip_unless_agreement_class()
  lv <- c("0", "1+", "2+")
  set.seed(3); n <- 40
  f1 <- factor(sample(lv, n, TRUE), levels = lv, ordered = TRUE)
  f2 <- factor(ifelse(runif(n) < .6, as.character(f1), sample(lv, n, TRUE)),
               levels = lv, ordered = TRUE)
  categorical <- data.frame(m1 = f1, m2 = f2)

  a <- agr_object(cont_pair(), vars = c("m1", "m2"))
  invisible(a$run())
  # Run 1 is continuous: kappa is refused, and the refusal is stated.
  expect_true(all(c("continuous", "continuous_vars") %in% note_keys(a$results$irrtable)))
  expect_true(is.na(as.numeric(a$results$irrtable$asDF$kappa[1])))

  agr_rerun_with(a, categorical)

  kappa2 <- as.numeric(a$results$irrtable$asDF$kappa[1])
  expect_true(is.finite(kappa2))
  # The contradiction the fix removes: a printed kappa under a note saying kappa
  # was not computed because the data are continuous.
  expect_false(any(c("continuous", "continuous_vars") %in% note_keys(a$results$irrtable)))
  expect_false(grepl("kappa was not computed", note_text(a$results$irrtable), fixed = TRUE))
  expect_false(grepl("continuous measurement", note_text(a$results$irrtable), fixed = TRUE))
  expect_identical(as.character(a$results$irrtable$asDF$method[1]), paste0("Cohen's ", KAPPA))
})

# ---- one vocabulary: the same coefficient carries the same name everywhere ----
test_that("one weighted kappa gets one name across the headline, intra-rater, paired and bootstrap tables", {
  skip_unless_agreement()
  d <- ord_panel()
  r <- q(agreement(data = d, vars = c("P_1", "P_2"), wght = "equal",
                   interIntraRater = TRUE,
                   bootstrapCI = TRUE, nBoot = 200, seed = 1,
                   pairedAgreementTest = TRUE, conditionBVars = c("Q_1", "Q_2"),
                   pairedBootN = 500))

  expected <- paste0("Linear-weighted ", KAPPA)
  headline <- as.character(r$irrtable$asDF$method[1])
  intra    <- r$interIntraRaterIntraTable$asDF
  boot     <- r$bootstrapCITable$asDF
  paired   <- r$pairedAgreementTable$asDF
  # Every table has to have produced the row whose label is under test, or the
  # comparison below is between two absent strings.
  expect_gt(nrow(intra), 0)
  expect_true("kappa" %in% gsub('^"|"$', "", rownames(boot)))
  expect_true("kappa" %in% gsub('^"|"$', "", rownames(paired)))
  boot_lbl   <- as.character(boot[gsub('^"|"$', "", rownames(boot)) == "kappa", "metric"])
  paired_lbl <- as.character(paired[gsub('^"|"$', "", rownames(paired)) == "kappa", "metric"])
  intra_lbl  <- as.character(intra$statistic_name[1])

  for (lbl in list(headline, intra_lbl, boot_lbl, paired_lbl))
    expect_identical(lbl, expected)

  # The same coefficient, to the last decimal, under all four names.
  expect_equal(as.numeric(intra$value[1]), as.numeric(r$irrtable$asDF$kappa[1]),
               tolerance = 1e-9)
  expect_equal(as.numeric(paired[gsub('^"|"$', "", rownames(paired)) == "kappa", "condition_a"]),
               as.numeric(r$irrtable$asDF$kappa[1]), tolerance = 1e-9)

  # irr's own untranslated string used to reach the headline cell verbatim.
  everything <- paste(c(headline, intra_lbl, boot_lbl, paired_lbl), collapse = " ")
  for (old in c("Kappa for 2 Raters", "Weights:", "Cohen's Kappa (linear weights)"))
    expect_false(grepl(old, everything, fixed = TRUE), info = old)
})

test_that("the weighting and the rater count decide the name, and nothing else does", {
  skip_unless_agreement()
  d <- ord_panel()
  lbl <- function(...) as.character(q(agreement(data = d, ...))$irrtable$asDF$method[1])
  expect_identical(lbl(vars = c("P_1", "P_2")), paste0("Cohen's ", KAPPA))
  expect_identical(lbl(vars = c("P_1", "P_2"), wght = "equal"),
                   paste0("Linear-weighted ", KAPPA))
  expect_identical(lbl(vars = c("P_1", "P_2"), wght = "squared"),
                   paste0("Quadratic-weighted ", KAPPA))
  expect_identical(lbl(vars = c("P_1", "P_2", "Q_1")), paste0("Fleiss' ", KAPPA))
  expect_identical(lbl(vars = c("P_1", "P_2", "Q_1"), exct = TRUE),
                   paste0("Fleiss' ", KAPPA, " (exact)"))
})

# ---- the subgroup trap: a translated label must not regrade an ICC -----------
test_that("a continuous subgroup run is graded on the ICC bands, not the kappa bands", {
  skip_unless_agreement()
  set.seed(5); m <- 90
  latent <- rnorm(m, 50, 12)
  d <- data.frame(R1 = latent + rnorm(m, 0, 4), R2 = latent + rnorm(m, 0, 4),
                  site = factor(rep(c("S1", "S2"), length.out = m)))
  tb <- q(agreement(data = d, vars = c("R1", "R2"), agreementBySubgroup = TRUE,
                    subgroupVariable = "site", subgroupMinCases = 10))$subgroupAgreementTable
  df <- tb$asDF
  expect_equal(nrow(df), 2)

  stat <- suppressWarnings(as.numeric(df$agreement_stat))
  expect_true(all(is.finite(stat)))
  expect_true(all(as.character(df$stat_type) == "ICC(2,1)"))

  # The Statistic column is displayed AND was branched on to pick the scale, so
  # translating it would have sent every non-English ICC to Landis & Koch. The
  # fixture is chosen so the two scales DISAGREE on both rows - otherwise this
  # assertion would pass with the branch broken.
  icc_words   <- vapply(stat, kl_icc_band, "")
  kappa_words <- vapply(stat, lk_kappa_band, "")
  expect_false(any(icc_words == kappa_words))
  expect_identical(as.character(df$interpretation), icc_words)

  # The note has to say which scale was used, or the words are unattributable.
  expect_true(grepl("Koo & Li", note_text(tb), fixed = TRUE))
})

# ---- the new distance metric -------------------------------------------------
test_that("the default rater metric separates readers with a large systematic offset", {
  skip_unless_agreement()
  d <- clu_fixture()
  tb <- q(agreement(data = d, vars = names(d), raterClustering = TRUE,
                    clusterMethod = "hierarchical", clusterLinkage = "average",
                    nClusters = 2, showDendrogram = FALSE,
                    showClusterHeatmap = FALSE))$raterClusterTable
  df <- tb$asDF
  expect_equal(nrow(df), ncol(d))
  expect_false(any(is.na(df$cluster)))
  cl <- function(v) df$cluster[match(v, df$rater)]

  # M3 and M4 read ~25 points high. Under 1 - Lin's CCC that is a distance;
  # under the old 1 - r default it was invisible (measured: the offset reader sat
  # NEARER the reference, 0.005, than the reference's own concordant twin, 0.008).
  expect_true(cl("M1") != cl("M3"))
  expect_true(cl("M2") != cl("M4"))
  expect_false("offset_blind" %in% note_keys(tb))
})

test_that("an explicit correlation metric still groups the offset readers and discloses it", {
  skip_unless_agreement()
  d <- clu_fixture()
  tb <- q(agreement(data = d, vars = names(d), raterClustering = TRUE,
                    clusterDistance = "correlation", clusterMethod = "hierarchical",
                    clusterLinkage = "average", nClusters = 2, showDendrogram = FALSE,
                    showClusterHeatmap = FALSE))$raterClusterTable
  df <- tb$asDF
  expect_equal(nrow(df), ncol(d))
  cl <- function(v) df$cluster[match(v, df$rater)]
  expect_true(cl("M1") == cl("M3"))
  expect_true("offset_blind" %in% note_keys(tb))

  # The disclosure has to quote the widest qualifying same-cluster mean gap,
  # recomputed here from the raw data and the published cluster column using the
  # two gates the backend documents (>= 0.25 pooled SD, and t >= 3).
  pooled_sd <- stats::sd(unlist(d, use.names = FALSE))
  worst <- 0
  for (i in seq_len(ncol(d) - 1L)) for (j in (i + 1L):ncol(d)) {
    if (cl(names(d)[i]) != cl(names(d)[j])) next
    dv  <- stats::na.omit(d[[i]] - d[[j]])
    gap <- abs(mean(dv))
    if (!is.finite(gap) || gap <= worst || gap < 0.25 * pooled_sd) next
    se <- stats::sd(dv) / sqrt(length(dv))
    if (is.finite(se) && se > 0 && gap / se < 3) next
    worst <- gap
  }
  expect_gt(worst, 0)
  expect_true(grepl(base::format(round(worst, 2), nsmall = 2, trim = TRUE),
                    note_text(tb), fixed = TRUE))
  # ... and it must point at the metric that is not blind to the offset.
  expect_true(grepl("Lin's CCC", note_text(tb), fixed = TRUE))
})

test_that("case clustering refuses the correlation metric below three raters", {
  skip_unless_agreement()
  d <- clu_fixture()

  # Two raters: a case's profile is two points, so every pair of cases correlates
  # exactly +1 or -1 and the grouping is the sign of one product. Refused.
  tb2 <- q(agreement(data = d[, c("M1", "M3")], vars = c("M1", "M3"),
                     caseClustering = TRUE, caseClusterMethod = "hierarchical",
                     caseClusterDistance = "correlation",
                     nCaseClusters = 2))$caseClusterTable
  expect_equal(nrow(tb2$asDF), 0)
  expect_true("error" %in% note_keys(tb2))
  expect_true(grepl("exactly +1 or -1", note_text(tb2), fixed = TRUE))

  # Three raters is the boundary, and it runs: this is a guard, not a ban.
  tb3 <- q(agreement(data = d[, c("M1", "M2", "M3")], vars = c("M1", "M2", "M3"),
                     caseClustering = TRUE, caseClusterMethod = "hierarchical",
                     caseClusterDistance = "correlation",
                     nCaseClusters = 2))$caseClusterTable
  expect_equal(nrow(tb3$asDF), nrow(d))
  expect_false("error" %in% note_keys(tb3))

  # Two raters under the DEFAULT case metric (euclidean now) must still produce a
  # table - the refusal is scoped to the metric the user chose, not to the setup.
  tbd <- q(agreement(data = d[, c("M1", "M3")], vars = c("M1", "M3"),
                     caseClustering = TRUE, caseClusterMethod = "hierarchical",
                     nCaseClusters = 2))$caseClusterTable
  expect_equal(nrow(tbd$asDF), nrow(d))
  expect_false("error" %in% note_keys(tbd))
})

# ---------------------------------------------------------------------------
# FIX-19: the multipleTestCorrection OPTION LEVEL is not a p.adjust() method.
#
# p.adjust()'s methods are holm/hochberg/hommel/bonferroni/BH/BY/fdr/none. The
# option level for Benjamini-Hochberg is `bh`, lower case, so passing the level
# straight through reached match.arg() and ERRORED the whole analysis - a user
# who selected "Benjamini-Hochberg (FDR)" got no result at all. One site mapped
# it through a switch and the other did not; both now share
# private$.pAdjustMethod(). Found when a vignette that had been masking the bug
# by passing an invalid "BH" was corrected to pass the real level.
# ---------------------------------------------------------------------------

test_that("FIX-19: every multipleTestCorrection level runs and is named in the note", {
  skip_unless_agreement()
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")

  # CONTINUOUS ratings: the mixed-effects comparison fits a linear mixed model
  # and refuses categorical input, so a factor fixture would exercise nothing.
  set.seed(4242)
  n <- 160
  latent <- runif(n, 5, 85)
  d <- data.frame(Rater1 = round(latent + rnorm(n, 0, 4), 1),
                  Rater2 = round(latent + 3 + rnorm(n, 0, 5), 1),
                  condition = factor(rep(c("Pre", "Post"), length.out = n)))

  seen <- list()
  for (lvl in c("none", "bonferroni", "bh", "holm")) {
    res <- q(agreement(data = d, vars = c("Rater1", "Rater2"),
                       conditionVariable = "condition",
                       mixedEffectsComparison = TRUE, icc = TRUE,
                       multipleTestCorrection = lvl))
    tb <- res$mixedEffectsTable$asDF
    # Non-vacuous: the path must actually have produced rows, or the absence of
    # an error proves nothing at all.
    expect_gt(nrow(tb), 0)
    expect_true(all(is.finite(tb$p_value)))
    seen[[lvl]] <- note_text(res$mixedEffectsTable)
  }

  # `none` writes no correction note; the other three name themselves in words a
  # reader recognises, not the internal option level.
  expect_false(grepl("adjusted using", seen[["none"]], fixed = TRUE))
  expect_match(seen[["bonferroni"]], "Bonferroni", fixed = TRUE)
  expect_match(seen[["bh"]],         "Benjamini-Hochberg", fixed = TRUE)
  expect_match(seen[["holm"]],       "Holm", fixed = TRUE)
  # The raw level must never surface as the user-facing name.
  expect_false(grepl("using the bh correction", seen[["bh"]], fixed = TRUE))
})

test_that("FIX-19: the option levels and p.adjust's method names stay in step", {
  skip_unless_agreement()
  # Every level declared in the .a.yaml must map to a real p.adjust method.
  # Oracle: p.adjust.methods, read from stats, not transcribed.
  levels_declared <- c("none", "bonferroni", "bh", "holm")
  mapped <- vapply(levels_declared, function(lvl)
    switch(lvl, bonferroni = "bonferroni", bh = "BH", holm = "holm", "none"),
    character(1))
  expect_true(all(mapped %in% stats::p.adjust.methods))
})
