# Regression tests for the independent review of `agreement`.
# /independent-reviewer agreement, 2026-09-21; repaired 2026-09-22.
# Every expectation below was reproduced against an independent oracle first in
# development-scripts/audit_agreement.R (IR-01 .. IR-06). Report:
# development-ideas/agreement-independent-review-2026-09-21.md
#
# Some findings required rejecting an unsupported request or restricting a test to
# the measurement level on which its direction is meaningful. The tests below pin
# those safety boundaries as well as the corrected computations and disclosures.

if (!exists("agreement", mode = "function")) {
  if (requireNamespace("ClinicoPath", quietly = TRUE)) {
    agreement <- getExportedValue("ClinicoPath", "agreement")
  }
}
skip_unless_agreement <- function() {
  testthat::skip_if_not(exists("agreement", mode = "function"),
                        "ClinicoPath::agreement not available")
}
q <- function(e) suppressWarnings(suppressMessages(force(e)))
note_keys <- function(tbl) names(tbl$notes)
note_text <- function(tbl) paste(vapply(tbl$notes, function(z) z$note, ""), collapse = " ")

# ---- IR-01 Agreement % is the UNANIMITY rate once there are 3+ raters --------
test_that("IR-01: Agreement % is disclosed as unanimity and the false relation is gone", {
  skip_unless_agreement()
  skip_if_not_installed("irr")
  data(diagnoses, package = "irr")
  d6 <- diagnoses[, 1:6]

  r6 <- q(agreement(data = d6, vars = names(d6)))

  # irr::agree's default tolerance = 0 counts only cases where EVERY rater matched.
  expect_equal(as.numeric(r6$irrtable$asDF$peragree[1]), irr::agree(d6)$value, tolerance = 1e-9)

  # On Fleiss' own data unanimity (16.67%) is BELOW 100 * kappa (43.02%), so the
  # generated panel must not claim Agreement % is always the higher of the two.
  expect_true(as.numeric(r6$irrtable$asDF$peragree[1]) <
                100 * as.numeric(r6$irrtable$asDF$kappa[1]))
  expect_false(grepl("always the higher of the two",
                     paste(as.character(r6$summary$content), collapse = " "), fixed = TRUE))

  # ... and the column's definition has to be stated somewhere the reader sees it.
  expect_true("peragree_unanimity" %in% note_keys(r6$irrtable))

  # With two raters Agreement % IS the ordinary match rate, so the note would be wrong.
  r2 <- q(agreement(data = d6, vars = names(d6)[1:2]))
  expect_false("peragree_unanimity" %in% note_keys(r2$irrtable))
})

# ---- IR-02 Finn's scale size is read from the declared levels ----------------
test_that("IR-02: Finn uses the declared scale size, not the finnLevels default", {
  skip_unless_agreement()
  skip_if_not_installed("irr")
  A <- factor(c("neg","pos","neg","pos","neg","pos","neg","pos","neg","pos"),
              levels = c("neg","pos"))
  B <- factor(c("pos","neg","pos","neg","pos","neg","pos","neg","neg","pos"),
              levels = c("neg","pos"))
  d <- data.frame(A = A, B = B)
  num <- data.frame(a = as.integer(A), b = as.integer(B))

  # finnLevels still defaults to 3; the point is that it is no longer believed.
  r <- q(agreement(data = d, vars = c("A","B"), finn = TRUE))
  got <- as.numeric(r$finnTable$asDF$finn_value[1])

  expect_equal(got, irr::finn(num, s.levels = 2)$value, tolerance = 1e-8)
  # The old behaviour was not merely different - it flipped the sign.
  expect_lt(got, 0)
  expect_gt(irr::finn(num, s.levels = 3)$value, 0)
  expect_true("levels_from_data" %in% note_keys(r$finnTable))

  # Agreeing option -> value unchanged and no note to explain away.
  r3 <- q(agreement(data = d, vars = c("A","B"), finn = TRUE, finnLevels = 2))
  expect_equal(as.numeric(r3$finnTable$asDF$finn_value[1]), got, tolerance = 1e-12)
  expect_false("levels_from_data" %in% note_keys(r3$finnTable))
})

test_that("IR-02b: Finn recodes both raters against ONE shared level set", {
  skip_unless_agreement()
  skip_if_not_installed("irr")
  # The two raters declare DIFFERENT level sets - B's scale omits "Moderate".
  # as.numeric(factor(x)) column by column then gives "High" the code 3 in A and
  # the code 2 in B, so the two raters were compared on scales that do not line up
  # and an exact match scored as a disagreement. Both are now recoded against the
  # union, taken in the order of the longer declared set: Low 1, Moderate 2, High 3.
  A <- factor(c("Low","High","Low","High","Low","High"),
              levels = c("Low","Moderate","High"))
  B <- factor(c("Low","High","Low","High","High","Low"),
              levels = c("Low","High"))
  r <- q(agreement(data = data.frame(A = A, B = B), vars = c("A","B"), finn = TRUE))
  shared <- irr::finn(data.frame(a = c(1,3,1,3,1,3), b = c(1,3,1,3,3,1)), s.levels = 3)$value
  percol <- irr::finn(data.frame(a = c(1,3,1,3,1,3), b = c(1,2,1,2,2,1)), s.levels = 3)$value
  expect_equal(as.numeric(r$finnTable$asDF$finn_value[1]), shared, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(shared, percol)))   # the fixture really does trigger it

  # Conflicting declared orders cannot be reconciled without inventing a scale.
  bad <- data.frame(
    A = factor(c("Low","High","Low"), levels = c("Low","Moderate","High")),
    B = factor(c("Low","High","High"), levels = c("High","Moderate","Low"))
  )
  rb <- q(agreement(data = bad, vars = c("A","B"), finn = TRUE))
  expect_true("error" %in% note_keys(rb$finnTable))
  expect_true(is.na(as.numeric(rb$finnTable$asDF$finn_value[1])))
})

# ---- IR-03 measurement level on the R API path -------------------------------
#
# What the routing helper can and cannot know, stated as a contract because the
# earlier version of this test asserted something no predicate can deliver.
#
# `vars` is `type: Variables` with no `permitted:`, and the generated wrapper does
# not coerce it to factor, so on the R path a rating column carries NO measurement
# level at all - only its class and its values. A numeric column with a NON-INTEGER
# value is unambiguously a measurement, at any n. A numeric column whose values are
# all integers is genuinely ambiguous between category codes (Rater 1 = 0/1) and a
# rounded measurement (tumour size in mm), and nothing in the data separates them.
# The helper therefore asks whether the VALUES could be a rating scale: integers
# whose magnitude never exceeds 10 are codes, anything larger is a measurement.
# That is a property of the VARIABLE, not of the sample, so the branch no longer
# moves with n. A code set that DOES run past 10 (a numerically coded diagnosis,
# say) is read as a measurement; the remedy is the one the module's own notes
# give - wrap the column in factor() when calling from R, or set the variable to
# Nominal/Ordinal in jamovi. IR-03b below pins both sides of the boundary so a
# future change cannot re-break the integer-coded ratings.

test_that("IR-03: a non-integer numeric measure takes the same branch at any sample size", {
  skip_unless_agreement()
  # Decimals, deliberately: the non-integer term is the sample-independent half of
  # the rule, and it is the one this property actually holds for.
  mk <- function(n) { set.seed(9); x <- runif(n, 5, 60)
    data.frame(R1 = x, R2 = pmax(0, x + runif(n, -3, 3))) }
  small <- q(agreement(data = mk(15), vars = c("R1","R2")))$irrtable$asDF
  large <- q(agreement(data = mk(60), vars = c("R1","R2")))$irrtable$asDF
  expect_identical(as.character(small$method[1]), as.character(large$method[1]))
  expect_match(as.character(small$method[1]), "continuous")

  # Five cases, two distinct integers apart - still continuous, because of the values.
  tiny <- q(agreement(data = data.frame(R1 = c(1.5, 2.5, 1.5, 2.5, 1.5),
                                        R2 = c(1.5, 2.5, 2.5, 2.5, 1.5)),
                      vars = c("R1","R2")))$irrtable$asDF
  expect_match(as.character(tiny$method[1]), "continuous")

  # Categorical-only add-ons must use the same measurement-level decision; the
  # old >20 gate survived in several secondary branches after the headline fix.
  extras <- q(agreement(
    data = data.frame(R1 = c(1.5, 2.5, 1.5, 2.5, 1.5), R2 = c(1.5, 2.5, 2.5, 2.5, 1.5)),
    vars = c("R1","R2"), finn = TRUE, raterBias = TRUE))
  expect_true("type_error" %in% note_keys(extras$finnTable))
  expect_true(is.na(as.numeric(extras$finnTable$asDF$finn_value[1])))
  expect_true("type_error" %in% note_keys(extras$raterBiasTable))

  # The same numbers declared as categories DO get kappa.
  fac <- q(agreement(data = data.frame(R1 = factor(c(1,2,1,2,1)), R2 = factor(c(1,2,2,2,1))),
                     vars = c("R1","R2")))$irrtable$asDF
  expect_false(is.na(as.numeric(fac$kappa[1])))
})

test_that("IR-03b: an INTEGER-valued numeric column is read as category codes up to magnitude 10", {
  skip_unless_agreement()
  # This is the documented limitation, not an aspiration. Integers inside a
  # rating scale's range -> category codes -> kappa. See the block comment above
  # for why no predicate can do better on the R path, and for the remedy.
  small <- q(agreement(data = data.frame(R1 = c(1, 2, 1, 2, 1), R2 = c(1, 2, 2, 2, 1)),
                       vars = c("R1","R2")))$irrtable$asDF
  expect_false(grepl("not computed", as.character(small$method[1]), fixed = TRUE))
  expect_false(is.na(as.numeric(small$kappa[1])))

  # ... and the same INTEGER shape at measurement magnitudes flips to continuous.
  set.seed(9); x <- round(runif(60, 5, 60), 0)
  big <- q(agreement(data = data.frame(R1 = x, R2 = pmax(0, x + sample(c(-3,-1,0,1,3), 60, TRUE))),
                     vars = c("R1","R2")))$irrtable$asDF
  expect_match(as.character(big$method[1]), "continuous")

  # The boundary itself, pinned: Gleason 6-10 is the widest scale in routine use,
  # so 10 is still a code and 11 is already a measurement. Without this the next
  # reader has no way to tell the threshold from an accident.
  codes <- q(agreement(data = data.frame(R1 = c(6,7,8,9,10,6), R2 = c(6,8,8,9,10,7)),
                       vars = c("R1","R2")))$irrtable$asDF
  expect_false(grepl("not computed", as.character(codes$method[1]), fixed = TRUE))
  meas <- q(agreement(data = data.frame(R1 = c(6,7,8,9,11,6), R2 = c(6,8,8,9,11,7)),
                      vars = c("R1","R2")))$irrtable$asDF
  expect_match(as.character(meas$method[1]), "continuous")
})

test_that("IR-03c: histopathology Rater 1 x Rater 2 still yields a real Cohen's kappa", {
  skip_unless_agreement()
  # The regression that would have caught the breakage this round repaired: plain
  # numeric 0/1 rater columns, classified continuous, ten tables reduced to a note.
  skip_if_not_installed("ClinicoPath")
  data(histopathology, package = "ClinicoPath", envir = environment())
  r <- q(agreement(data = histopathology, vars = c("Rater 1", "Rater 2")))$irrtable$asDF
  expect_equal(as.numeric(r$kappa[1]), 0.8205535, tolerance = 1e-6)
  expect_equal(as.numeric(r$subjects[1]), 249)
  expect_false(grepl("not computed", as.character(r$method[1]), fixed = TRUE))
})

test_that("IR-03d: one factor rater plus one continuous numeric rater routes continuous and names the column", {
  skip_unless_agreement()
  # any(), not all(): every categorical statistic here needs EVERY rater on the
  # same categorical scale, and "one of your variables" is not actionable.
  set.seed(11)
  d <- data.frame(Grade = factor(sample(c("G1","G2","G3"), 30, TRUE)),
                  `Ki67 index` = runif(30, 0, 100),
                  check.names = FALSE)
  r <- q(agreement(data = d, vars = c("Grade", "Ki67 index")))
  expect_match(as.character(r$irrtable$asDF$method[1]), "not computed")
  expect_true("continuous_vars" %in% note_keys(r$irrtable))
  expect_true(grepl("Ki67 index", note_text(r$irrtable), fixed = TRUE))
  expect_false(grepl("Grade", note_text(r$irrtable), fixed = TRUE))
})

# ---- IR-04 unsupported headline weighting must not fall back -----------------
test_that("IR-04: weighted kappa with 3+ raters is disabled, not silently unweighted", {
  skip_unless_agreement()
  lv <- c("Low","Moderate","High","Severe")
  set.seed(3); n <- 40
  g <- function() factor(sample(lv, n, TRUE), levels = lv, ordered = TRUE)
  d3 <- data.frame(R1 = g(), R2 = g(), R3 = g())

  sq <- q(agreement(data = d3, vars = c("R1","R2","R3"),
                    wght = "squared", allPairsKappa = TRUE))
  expect_true(is.na(as.numeric(sq$irrtable$asDF$kappa[1])))
  expect_true("weights_unsupported" %in% note_keys(sq$irrtable))
  expect_match(as.character(sq$irrtable$asDF$method[1]), "not computed", ignore.case = TRUE)
  # The valid weighted pairwise coefficient must remain available.
  expect_true(all(is.finite(as.numeric(sq$allPairsKappaTable$asDF$kappa))))

  eq <- q(agreement(data = d3, vars = c("R1","R2","R3"), wght = "equal"))
  expect_true(is.na(as.numeric(eq$irrtable$asDF$kappa[1])))
  expect_true("weights_unsupported" %in% note_keys(eq$irrtable))
  un <- q(agreement(data = d3, vars = c("R1","R2","R3"), wght = "unweighted"))
  expect_true(is.finite(as.numeric(un$irrtable$asDF$kappa[1])))

  # Two raters DO honour the weights, so the note must not appear there.
  d2 <- d3[, c("R1","R2")]
  sq2 <- q(agreement(data = d2, vars = c("R1","R2"), wght = "squared"))
  un2 <- q(agreement(data = d2, vars = c("R1","R2"), wght = "unweighted"))
  expect_false(isTRUE(all.equal(as.numeric(sq2$irrtable$asDF$kappa[1]),
                                as.numeric(un2$irrtable$asDF$kappa[1]))))
})

# ---- IR-05 rater.bias is a 1-df directional test, not marginal homogeneity ---
test_that("IR-05: the rater-bias table names its own hypothesis and shows df = 1", {
  skip_unless_agreement()
  skip_if_not_installed("DescTools")
  # 30 discordances each way, so the directional test returns p = 1 - while the
  # margins are (50,20,50) against (50,50,20) and Stuart-Maxwell rejects at 9e-14.
  tab <- matrix(c(20,30,0, 0,20,0, 30,0,20), nrow = 3, byrow = TRUE,
                dimnames = list(c("A","B","C"), c("A","B","C")))
  cells <- which(tab > 0, arr.ind = TRUE)
  d <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i)
    data.frame(R1 = rep(rownames(tab)[cells[i,1]], tab[cells[i,1], cells[i,2]]),
               R2 = rep(colnames(tab)[cells[i,2]], tab[cells[i,1], cells[i,2]]))))
  d[] <- lapply(d, factor, levels = c("A","B","C"), ordered = TRUE)

  r <- q(agreement(data = d, vars = c("R1","R2"), raterBias = TRUE,
                   bhapkar = TRUE, showRaterBiasGuide = TRUE))
  tb <- r$raterBiasTable$asDF

  expect_equal(as.numeric(tb$df[1]), 1)                       # was blank: no $parameter
  expect_false(grepl("marginal homogeneity", as.character(tb$method[1]), ignore.case = TRUE))
  expect_true("scope" %in% note_keys(r$raterBiasTable))
  expect_match(note_text(r$raterBiasTable), "Bhapkar", fixed = TRUE)

  # The general question is answered by the test that actually asks it.
  expect_lt(as.numeric(r$bhapkarTable$asDF$p[1]), .05)
  expect_equal(as.numeric(tb$p[1]), 1, tolerance = 1e-12)
  guide <- paste(as.character(r$raterBiasExplanation$content), collapse = " ")
  expect_false(grepl("all raters simultaneously", guide, ignore.case = TRUE))

  # On nominal variables "higher" is undefined, so the directional test must
  # refuse to compute while the order-free Bhapkar test remains available.
  nominal <- data.frame(
    R1 = factor(as.character(d$R1), levels = c("A","B","C")),
    R2 = factor(as.character(d$R2), levels = c("A","B","C"))
  )
  rn <- q(agreement(data = nominal, vars = c("R1","R2"),
                    raterBias = TRUE, bhapkar = TRUE))
  expect_true("ordinal_required" %in% note_keys(rn$raterBiasTable))
  expect_true(is.na(as.numeric(rn$raterBiasTable$asDF$p[1])))
  expect_lt(as.numeric(rn$bhapkarTable$asDF$p[1]), .05)

  mismatched <- data.frame(
    R1 = ordered(c("A","B","C","A"), levels = c("A","B","C")),
    R2 = ordered(c("A","B","C","B"), levels = c("A","C","B"))
  )
  rm <- q(agreement(data = mismatched, vars = c("R1","R2"), raterBias = TRUE))
  expect_true("scale_mismatch" %in% note_keys(rm$raterBiasTable))
  expect_true(is.na(as.numeric(rm$raterBiasTable$asDF$p[1])))

  three <- data.frame(R1 = d$R1, R2 = d$R2, R3 = d$R1)
  r3 <- q(agreement(data = three, vars = names(three), bhapkar = TRUE))
  expect_false(grepl("Rater Bias Test", note_text(r3$bhapkarTable), fixed = TRUE))
})

# ---- IR-06 / F-07 Bland-Altman disclosures -----------------------------------
test_that("IR-06: the proportional-bias confound and the LoA coverage are disclosed", {
  skip_unless_agreement()
  set.seed(11)
  truth <- rnorm(80, 0, 10)
  d <- data.frame(M1 = truth + rnorm(80, 0, 1), M2 = truth + rnorm(80, 0, 4))
  r <- q(agreement(data = d, vars = c("M1","M2"),
                   blandAltmanPlot = TRUE, proportionalBias = TRUE))

  # D on M shares measurement error with its own regressor: unequal precision alone
  # produces a slope. The estimator is unchanged, so the caveat must be present.
  expect_true("prop_bias_assumption" %in% note_keys(r$blandAltmanStats))

  # baConfidenceLevel is a COVERAGE proportion; changing it moves the limits.
  expect_true("loa_coverage" %in% note_keys(r$blandAltmanStats))
  wide <- q(agreement(data = d, vars = c("M1","M2"),
                      blandAltmanPlot = TRUE, baConfidenceLevel = 0.99))
  expect_gt(as.numeric(wide$blandAltmanStats$asDF$upperLoA[1]),
            as.numeric(r$blandAltmanStats$asDF$upperLoA[1]))

  # No proportional-bias request -> no caveat about a test that was not run.
  off <- q(agreement(data = d, vars = c("M1","M2"), blandAltmanPlot = TRUE))
  expect_false("prop_bias_assumption" %in% note_keys(off$blandAltmanStats))
})

# ---- F-08 empirical TDI scope -------------------------------------------------
test_that("IR-07: empirical TDI does not claim equivalence or future coverage", {
  skip_unless_agreement()
  set.seed(31)
  d <- data.frame(M1 = rnorm(50, 20, 4), M2 = rnorm(50, 20, 4))
  r <- q(agreement(data = d, vars = c("M1","M2"), tdi = TRUE,
                   tdiCoverage = 90, tdiLimit = 10, nBoot = 100))
  expect_true("scope" %in% note_keys(r$tdiTable))
  expect_match(note_text(r$tdiTable), "unconditional empirical quantile", ignore.case = TRUE)
  interpretation <- as.character(r$tdiTable$asDF$interpretation[1])
  expect_false(grepl("equivalence criterion", interpretation, ignore.case = TRUE))
  expect_match(interpretation, "Observed criterion", ignore.case = TRUE)
})

# =============================================================================
# 2026-09-23 /independent-reviewer agreement - second round (IR2-01 .. IR2-18).
# Report:  development-ideas/agreement-independent-review-2026-09-23.md
# Harness: development-scripts/audit_agreement_2026-09-23.R
#
# One block per applied fix, each asserting the REPAIRED contract. Where a fix
# changed what the analysis CLAIMS rather than what it computes (FIX-01's
# withdrawal of the multivariate iota, FIX-05's decision to keep the
# correlation default and disclose instead), the audit script's block comment
# records the old expectation and why it moved; these tests pin the new one.
# =============================================================================

row_by_key <- function(df, key) df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
has_row_key <- function(df, key) key %in% gsub('^"|"$', "", rownames(df))

# Landis & Koch (1977), coded here from the paper's cut-points rather than read
# from the module, so the vocabulary tests have an oracle of their own.
lk_band <- function(v) {
  if (length(v) != 1L || !is.finite(v)) return("Not estimable")
  if (v < 0)    return("Worse than chance")
  if (v < 0.20) return("Slight")
  if (v < 0.40) return("Fair")
  if (v < 0.60) return("Moderate")
  if (v < 0.80) return("Substantial")
  "Almost perfect"
}

# The package source, for the few assertions that are about what the module
# SAYS rather than what it computes. testthat runs with the working directory
# at tests/testthat, so walk up until the two agreement source files are found.
agr_src_root <- function() {
  p <- normalizePath(getwd(), mustWork = FALSE)
  for (i in 1:6) {
    if (file.exists(file.path(p, "jamovi", "agreement.a.yaml")) &&
        file.exists(file.path(p, "R", "agreement.b.R"))) return(p)
    up <- dirname(p)
    if (identical(up, p)) break
    p <- up
  }
  ""
}

# Ordered categorical raters on one declared 4-grade scale.
ir2_cat_fixture <- function(n = 120, seed = 20260923) {
  lv <- c("0", "1+", "2+", "3+")
  set.seed(seed)
  truth <- sample(lv, n, TRUE, prob = c(.4, .3, .2, .1))
  mk <- function(p) factor(ifelse(runif(n) < p, sample(lv, n, TRUE), truth),
                           levels = lv, ordered = TRUE)
  data.frame(A1 = mk(.20), A2 = mk(.25), A3 = mk(.22),
             B1 = mk(.05), B2 = mk(.07), B3 = mk(.06),
             site = factor(sample(c("Site A", "Site B", "Site C"), n, TRUE,
                                  prob = c(.5, .35, .15))))
}

# Five readers of a Ki-67-like percentage; M3 and M4 read about 25 points high,
# so correlation distance (which removes an offset) and Euclidean distance
# (which does not) must disagree about them.
ir2_clu_fixture <- function(n = 60, seed = 11) {
  set.seed(seed)
  base <- round(runif(n, 0, 90), 1)
  mkc <- function(off, sd) pmin(100, pmax(0, round(base + off + rnorm(n, 0, sd), 1)))
  data.frame(M1 = mkc(0, 3), M2 = mkc(1, 3), M3 = mkc(25, 4),
             M4 = mkc(26, 4), M5 = mkc(-1, 3))
}

# ---- FIX-01 iota is the SINGLE-VARIABLE coefficient, and says so everywhere --
test_that("FIX-01: the iota table is scoped to one variable and no multivariate claim survives", {
  skip_unless_agreement()
  skip_if_not_installed("irr")
  set.seed(99); m <- 60
  size <- round(runif(m, 5, 60), 1); mit <- rpois(m, 6)
  jit <- function(x, s) pmax(0, round(x + rnorm(m, 0, s), 1))
  d <- data.frame(size_P1 = jit(size, 2), size_P2 = jit(size, 2),
                  mit_P1 = pmax(0, mit + sample(-1:1, m, TRUE)),
                  mit_P2 = pmax(0, mit + sample(-1:1, m, TRUE)))
  r <- q(agreement(data = d, vars = names(d), iota = TRUE))
  tb <- r$iotaTable

  # (a) the scope note is present and states the withdrawal in so many words
  expect_true("scope" %in% note_keys(tb))
  expect_true(grepl("not computed by this analysis", note_text(tb), fixed = TRUE))
  expect_true(grepl("single-variable iota", note_text(tb), fixed = TRUE))

  # (b) the number is the single-variable iota of this layout, not something else
  expect_equal(as.numeric(tb$asDF$iota_value[1]),
               irr::iota(list(as.matrix(d)), scaledata = "quantitative",
                         standardize = TRUE)$value,
               tolerance = 1e-8)
  expect_equal(as.numeric(tb$asDF$raters[1]), 4)

  # (c) the `variables` column is gone. R/agreement.h.R is autogenerated and can
  #     lag the .r.yaml, so the runtime half accepts "absent, or nothing fills it".
  expect_false("variables" %in% names(tb$asDF) && !all(is.na(tb$asDF$variables)))

  root <- agr_src_root()
  skip_if_not(nzchar(root), "agreement source files not reachable from the test working directory")
  ry <- readLines(file.path(root, "jamovi", "agreement.r.yaml"), warn = FALSE)
  beg <- grep("^    - name: iotaTable$", ry)[1]
  end <- grep("^    - name: ", ry); end <- end[end > beg][1]
  expect_false(any(grepl("- name: variables", ry[beg:(end - 1L)], fixed = TRUE)))

  # (d) every surviving occurrence of "multivariate" is a code comment or an
  #     explicit denial. A denial can span several source lines (folded YAML, an
  #     HTML paragraph), so each hit is judged on a +/- 3 line window.
  denials <- c("not computed by this analysis", "not computed by this",
               "not</strong> produce the multivariate", "does not describe your study",
               "no claim about several variables", "computes iota for a single variable",
               "needs one matrix per")
  offenders <- unlist(lapply(c("jamovi/agreement.a.yaml", "jamovi/agreement.r.yaml",
                               "jamovi/agreement.u.yaml", "R/agreement.b.R"), function(f) {
    ln <- readLines(file.path(root, f), warn = FALSE)
    hit <- grep("multivariate", ln, ignore.case = TRUE)
    bad <- vapply(hit, function(i) {
      if (startsWith(trimws(ln[i]), "#")) return(FALSE)
      # Case-fold and collapse whitespace first: a folded YAML block wraps a
      # sentence mid-phrase and keeps its indentation, and the .a.yaml writes
      # "is NOT computed by this analysis" in caps.
      ctx <- paste(ln[max(1L, i - 3L):min(length(ln), i + 3L)], collapse = " ")
      ctx <- tolower(gsub("[[:space:]]+", " ", ctx))
      !any(vapply(denials, function(mk) grepl(mk, ctx, fixed = TRUE), TRUE))
    }, TRUE)
    if (any(bad)) sprintf("%s:%d", f, hit[bad]) else character(0)
  }))
  expect_equal(as.character(offenders), character(0))
})

# ---- FIX-02 the paired comparison honours `wght` ----------------------------
test_that("FIX-02: Condition A kappa is the headline kappa under every wght setting", {
  skip_unless_agreement()
  d <- ir2_cat_fixture()
  got <- vapply(c("unweighted", "equal", "squared"), function(w) {
    r <- q(agreement(data = d, vars = c("A1", "A2"), wght = w,
                     pairedAgreementTest = TRUE, conditionBVars = c("B1", "B2"),
                     pairedBootN = 500, seed = 1))
    c(head = as.numeric(r$irrtable$asDF$kappa[1]),
      paired = as.numeric(row_by_key(r$pairedAgreementTable$asDF, "kappa")$condition_a))
  }, c(0, 0))

  # irr::kappa2() was called with no weight=, so Condition A carried the
  # unweighted kappa while the headline showed the weighted one, both labelled
  # "Cohen's Kappa".
  expect_equal(unname(got["paired", ]), unname(got["head", ]), tolerance = 1e-10)
  # ... and the fixture really does exercise the weighting, or the line above
  # would hold for the wrong reason.
  expect_false(isTRUE(all.equal(unname(got["head", "unweighted"]),
                                unname(got["head", "squared"]))))
})

# ---- FIX-03 kappa across unequal panels is refused, not fudged --------------
test_that("FIX-03: unequal rater panels drop the kappa row; a non-estimable kappa keeps it blank", {
  skip_unless_agreement()
  d <- ir2_cat_fixture()

  # Two raters give Cohen's kappa, three give Fleiss': different models of
  # chance agreement, so their difference has no estimand and there is no row.
  r <- q(agreement(data = d, vars = c("A1", "A2"), pairedAgreementTest = TRUE,
                   conditionBVars = c("B1", "B2", "B3"), pairedBootN = 500, seed = 1))
  tb <- r$pairedAgreementTable
  expect_false(has_row_key(tb$asDF, "kappa"))
  expect_true(has_row_key(tb$asDF, "pct_agree"))      # comparable across panel sizes
  expect_true("kappa_panel" %in% note_keys(tb))
  expect_true(grepl("different models of chance agreement", note_text(tb), fixed = TRUE))

  # A kappa that IS defined but not estimable keeps its labelled row with empty
  # cells. It used to vanish, so the output looked as though kappa was never asked for.
  dc <- d[, c("A1", "A2")]
  dc$C1 <- factor("0", levels = levels(d$A1), ordered = TRUE)
  dc$C2 <- dc$C1
  r2 <- q(agreement(data = dc, vars = c("A1", "A2"), pairedAgreementTest = TRUE,
                    conditionBVars = c("C1", "C2"), pairedBootN = 500, seed = 1))
  tb2 <- r2$pairedAgreementTable
  expect_true(has_row_key(tb2$asDF, "kappa"))
  kr <- row_by_key(tb2$asDF, "kappa")
  expect_true(all(is.na(c(kr$condition_a, kr$condition_b, kr$difference,
                          kr$ci_lower, kr$ci_upper, kr$p_value))))
  expect_true("kappa_na" %in% note_keys(tb2))
  expect_false("kappa_panel" %in% note_keys(tb2))
})

# ---- FIX-04 the similarity column is a similarity ---------------------------
# CONTRACT CHANGED 2026-09-23 (round 3): the two distance options no longer share
# a level set, so one loop over both is no longer possible. `ccc` was added to the
# rater option (and is its default) and deliberately NOT to the case option - a
# case's profile has one point per rater, typically three to six, far too few to
# estimate a concordance coefficient from. The invariant is unchanged; the sweep
# now covers each option's own levels, and pins the refusal that keeps `ccc` off
# the case path. Splitting the loop is also what keeps this block from ERRORING:
# jmvcore rejects an unknown List level, and an erroring test_that aborts every
# expectation after it.
test_that("FIX-04: every avg_similarity lies in [-1, 1] on every level of both distance options", {
  skip_unless_agreement()
  d <- ir2_clu_fixture(n = 40)
  in_unit <- function(tb, label) {
    s <- suppressWarnings(as.numeric(tb$asDF$avg_similarity))
    s <- s[is.finite(s)]
    # An empty table or an all-NA column would satisfy the bounds vacuously.
    expect_gt(length(s), 0)
    # "1 - dist" was applied to raw Euclidean/Manhattan distances, which are
    # unbounded and grow with the number of cases: the column printed -37.56
    # and -227.80, and a LARGER distance read as a MORE NEGATIVE similarity.
    expect_true(all(s >= -1 & s <= 1),
                info = sprintf("%s produced %s", label, paste(range(s), collapse = " .. ")))
  }
  for (dm in c("ccc", "correlation", "euclidean", "manhattan", "agreement")) {
    r <- q(agreement(data = d, vars = names(d),
                     raterClustering = TRUE, clusterMethod = "hierarchical",
                     clusterDistance = dm, clusterLinkage = "average", nClusters = 2,
                     showDendrogram = FALSE, showClusterHeatmap = FALSE))
    in_unit(r$raterClusterTable, paste("rater metric", dm))
  }
  for (dm in c("euclidean", "manhattan", "correlation", "agreement")) {
    r <- q(agreement(data = d, vars = names(d),
                     caseClustering = TRUE, caseClusterMethod = "hierarchical",
                     caseClusterDistance = dm, nCaseClusters = 2))
    in_unit(r$caseClusterTable, paste("case metric", dm))
  }
  # `ccc` is not a level of caseClusterDistance: it must be refused, not accepted
  # and not silently mapped onto another metric.
  expect_error(agreement(data = d, vars = names(d), caseClustering = TRUE,
                         caseClusterMethod = "hierarchical",
                         caseClusterDistance = "ccc", nCaseClusters = 2),
               "caseClusterDistance")
})

# ---- FIX-05 the correlation metric discloses what it removed ----------------
test_that("FIX-05: the correlation metric discloses the systematic offset and quotes its size", {
  skip_unless_agreement()
  d <- ir2_clu_fixture()
  tb <- q(agreement(data = d, vars = names(d), raterClustering = TRUE,
                    clusterMethod = "hierarchical", clusterDistance = "correlation",
                    clusterLinkage = "average", nClusters = 2,
                    showDendrogram = FALSE, showClusterHeatmap = FALSE))$raterClusterTable

  # Correlation distance is 1 - r, so it removes any systematic offset: readers
  # 25 Ki-67 points apart on every case still land in one cluster.
  # CONTRACT CHANGED 2026-09-23 (round 3): the default is NO LONGER correlation -
  # it is ccc (1 - Lin's CCC), which sees the offset, so this block now covers
  # only the user who deliberately chooses correlation. The default's own
  # contract (separate, and disclose nothing) is asserted in the block below.
  # The disclosure itself is unchanged: the note has to carry the MEASURED gap.
  # Recomputed here from the raw data plus the published cluster column, using
  # the two gates the backend documents: >= 0.25 pooled SD, and t >= 3.
  cl <- tb$asDF$cluster[match(names(d), tb$asDF$rater)]
  pooled_sd <- stats::sd(unlist(d, use.names = FALSE))
  worst <- 0
  for (i in seq_len(ncol(d) - 1L)) for (j in (i + 1L):ncol(d)) {
    if (cl[i] != cl[j]) next
    dv <- stats::na.omit(d[[i]] - d[[j]])
    gap <- abs(mean(dv))
    if (!is.finite(gap) || gap <= worst || gap < 0.25 * pooled_sd) next
    se <- stats::sd(dv) / sqrt(length(dv))
    if (is.finite(se) && se > 0 && gap / se < 3) next
    worst <- gap
  }
  expect_gt(worst, 0)                                  # the fixture really bites
  expect_true("offset_blind" %in% note_keys(tb))
  expect_true(grepl(base::format(round(worst, 2), nsmall = 2, trim = TRUE),
                    note_text(tb), fixed = TRUE))

  # Euclidean distance is not blind to an offset, so it must not carry the note.
  eu <- q(agreement(data = d, vars = names(d), raterClustering = TRUE,
                    clusterMethod = "hierarchical", clusterDistance = "euclidean",
                    clusterLinkage = "average", nClusters = 2,
                    showDendrogram = FALSE, showClusterHeatmap = FALSE))$raterClusterTable
  expect_false("offset_blind" %in% note_keys(eu))
})

# CONTRACT CHANGED 2026-09-23 (round 3): the clusterDistance DEFAULT changed from
# correlation to ccc (1 - Lin's CCC), a user-authorised breaking change made on
# measurement - under 1 - r a reader 25 units high sat NEARER the reference
# (distance 0.005) than the reference's own concordant twin (0.008). The old
# expectation, "they still share a cluster and the table discloses it", was pinned
# for correlation above; this is the other half, and it is the half that could not
# be asserted at all while the default was correlation.
test_that("FIX-05b: under the DEFAULT metric the offset readers separate and nothing is disclosed", {
  skip_unless_agreement()
  d <- ir2_clu_fixture()
  tb <- q(agreement(data = d, vars = names(d), raterClustering = TRUE,
                    clusterMethod = "hierarchical", clusterLinkage = "average",
                    nClusters = 2, showDendrogram = FALSE,
                    showClusterHeatmap = FALSE))$raterClusterTable
  df <- tb$asDF
  # A missing row or an NA cluster id would make every `!=` below NA, not FALSE.
  expect_equal(nrow(df), ncol(d))
  expect_false(any(is.na(df$cluster)))
  cl <- function(v) df$cluster[match(v, df$rater)]

  # M3 and M4 read ~25 points high; M1, M2 and M5 do not. The default must not
  # put a high reader in with a low one.
  expect_true(cl("M1") != cl("M3"))
  expect_true(cl("M2") != cl("M4"))
  expect_true(cl("M1") == cl("M2") && cl("M1") == cl("M5"))
  expect_true(cl("M3") == cl("M4"))

  # There is nothing left to disclose, so the disclosure must be gone. It is the
  # note's absence that proves the metric saw the offset rather than that the
  # measurement stopped running: the correlation run above still raises it on the
  # same fixture.
  expect_false("offset_blind" %in% note_keys(tb))

  # Under ccc the similarity column IS Lin's coefficient, and the note says so
  # instead of describing a dataset-relative rescaling.
  s <- suppressWarnings(as.numeric(df$avg_similarity))
  expect_true(all(is.finite(s)))
  expect_true(all(s >= -1 & s <= 1))
  expect_true("similarity_scale" %in% note_keys(tb))
  expect_true(grepl("Lin's concordance correlation coefficient", note_text(tb), fixed = TRUE))

  # Passing the new default explicitly must be the same analysis.
  expl <- q(agreement(data = d, vars = names(d), raterClustering = TRUE,
                      clusterDistance = "ccc", clusterMethod = "hierarchical",
                      clusterLinkage = "average", nClusters = 2,
                      showDendrogram = FALSE, showClusterHeatmap = FALSE))$raterClusterTable
  expect_equal(expl$asDF, df)
})

# ---- FIX-06 one kappa scale across the whole analysis -----------------------
test_that("FIX-06: one kappa gets one Landis & Koch word in the summary, subgroups and intra-rater table", {
  skip_unless_agreement()
  d <- ir2_cat_fixture()
  names(d)[1:2] <- c("P_1", "P_2")            # rater P, time points 1 and 2
  r <- q(agreement(data = d, vars = c("P_1", "P_2"), showSummary = TRUE,
                   interIntraRater = TRUE, agreementBySubgroup = TRUE,
                   subgroupVariable = "site", subgroupMinCases = 10))

  k <- as.numeric(r$irrtable$asDF$kappa[1])
  expect_true(is.finite(k))

  # The summary prose and the Intra-Rater table grade the SAME number. "Fair"
  # used to mean 0.20-0.40 in one table of this output and 0.40-0.60 in another.
  summary_txt <- gsub("<[^>]*>", " ", paste(as.character(r$summary$content), collapse = " "))
  expect_true(grepl(paste0(tolower(lk_band(k)), " agreement"), summary_txt, fixed = TRUE))

  intra <- r$interIntraRaterIntraTable$asDF
  expect_equal(as.numeric(intra$value[1]), k, tolerance = 1e-9)
  expect_identical(as.character(intra$interpretation[1]), lk_band(k))

  # ... and every subgroup word comes off the same scale.
  sub <- r$subgroupAgreementTable$asDF
  expect_gt(nrow(sub), 0)
  expect_identical(as.character(sub$interpretation),
                   vapply(as.numeric(sub$agreement_stat), lk_band, ""))

  # Both tables name the scale they used, so the reader is not left to guess.
  expect_true("scale" %in% note_keys(r$interIntraRaterIntraTable))
  expect_true("scale" %in% note_keys(r$subgroupAgreementTable))
})

# ---- FIX-07 an ordered factor is not a different measurement ----------------
test_that("FIX-07: ordered factors and their own integer codes give the identical decomposition", {
  skip_unless_agreement()
  lv <- c("0", "1+", "2+", "3+")
  set.seed(21); n <- 300
  mk <- function(p) {
    truth <- sample(lv, n, TRUE, prob = c(.4, .3, .2, .1))
    factor(ifelse(runif(n) < p, sample(lv, n, TRUE), truth), levels = lv, ordered = TRUE)
  }
  dOrd <- data.frame(P1 = mk(0), P2 = mk(.45), P3 = mk(.25))
  dNum <- as.data.frame(lapply(dOrd, function(x) as.integer(x) - 1L))

  vOrd <- q(agreement(data = dOrd, vars = names(dOrd), maxwellRE = TRUE))$maxwellRETable
  vNum <- q(agreement(data = dNum, vars = names(dNum), maxwellRE = TRUE))$maxwellRETable
  cells <- c("re_value", "systematic_prop", "random_prop",
             "total_variance", "systematic_var", "random_var")

  # A pure re-typing used to decide whether the decomposition ran at all: the
  # ordered factor was refused and the identical values as integers were not.
  expect_false(any(is.na(as.numeric(vOrd$asDF[1, cells]))))
  expect_equal(as.numeric(vOrd$asDF[1, cells]), as.numeric(vNum$asDF[1, cells]),
               tolerance = 1e-8)

  # The recode is disclosed on the run that needed it, and not on the one that did not.
  expect_true("ordinal_codes" %in% note_keys(vOrd))
  expect_true(grepl("lowest first", note_text(vOrd), fixed = TRUE))
  expect_false("ordinal_codes" %in% note_keys(vNum))

  # An unordered factor has no scale for an offset to sit on and is still refused.
  dNom <- as.data.frame(lapply(dOrd, function(x) factor(as.character(x), levels = lv)))
  vNom <- q(agreement(data = dNom, vars = names(dNom), maxwellRE = TRUE))$maxwellRETable
  expect_true(is.na(as.numeric(vNom$asDF$re_value[1])))
  expect_true("scale" %in% note_keys(vNom))
})

# ---- FIX-08 a truncated variance component is not a point estimate ----------
test_that("FIX-08: a clamped negative rater variance blanks the shares and discloses the raw value", {
  skip_unless_agreement()
  set.seed(505); n <- 80
  tru <- round(runif(n, 0, 100), 1)
  d <- data.frame(Q1 = round(tru + rnorm(n, 0, 6), 1), Q2 = round(tru + rnorm(n, 0, 6), 1),
                  Q3 = round(tru + rnorm(n, 0, 6), 1), Q4 = round(tru + rnorm(n, 0, 6), 1))

  # Independent method-of-moments reconstruction, so the fixture is shown to bite.
  m <- as.matrix(d); nc <- nrow(m); nr <- ncol(m); gm <- mean(m)
  SSt <- sum((m - gm)^2); SSc <- nr * sum((rowMeans(m) - gm)^2); SSr <- nc * sum((colMeans(m) - gm)^2)
  raw <- (SSr / (nr - 1) - (SSt - SSc - SSr) / ((nc - 1) * (nr - 1))) / nc
  expect_lt(raw, 0)

  tb <- q(agreement(data = d, vars = names(d), maxwellRE = TRUE))$maxwellRETable
  row <- tb$asDF
  # A share taken from a component truncated at its boundary is 0% and 100% by
  # construction, not by measurement, so the three share cells are withheld.
  expect_true(is.na(as.numeric(row$systematic_prop[1])))
  expect_true(is.na(as.numeric(row$random_prop[1])))
  expect_true(is.na(as.numeric(row$re_value[1])))
  # ... and the unclamped estimate is disclosed instead of silently discarded.
  expect_true("boundary" %in% note_keys(tb))
  expect_true(grepl(sprintf("%.4g", raw), note_text(tb), fixed = TRUE))
  # The verdict describes; it must not prescribe off a boundary estimate.
  expect_false(grepl("training|calibration|standardi|intervention",
                     as.character(row$interpretation[1]), ignore.case = TRUE))
  # The band note belongs to the branch that actually printed a share.
  expect_false("bands" %in% note_keys(tb))
})

# ---- FIX-09 Cochran's rule on the agreement-pattern cells -------------------
test_that("FIX-09: the sparse-cell note fires on the 3-category 4-rater design and quotes its counts", {
  skip_unless_agreement()
  d <- data.frame(a = factor(rep(c("x", "y"), 6)), b = factor(rep(c("x", "y", "x"), 4)))
  tb <- q(agreement(data = d, vars = c("a", "b"), agreementSampleSize = TRUE,
                    ssMetric = "fleiss", ssNCategories = 3, ssNRaters = 4,
                    ssKappaNull = 0.4, ssKappaAlt = 0.7))$agreementSampleSizeTable
  n <- as.numeric(row_by_key(tb$asDF, "n_required")$value)
  expect_equal(n, 18)

  expect_true("sparse" %in% note_keys(tb))
  txt <- note_text(tb)
  expect_true(grepl("smallest expected count is 2", txt, fixed = TRUE))
  expect_true(grepl("3 of 4 cells are below 5", txt, fixed = TRUE))
  expect_true(grepl("large-sample chi-square approximation", txt, fixed = TRUE))

  # A design whose cells are NOT sparse must not carry the caveat.
  ok <- q(agreement(data = d, vars = c("a", "b"), agreementSampleSize = TRUE,
                    ssMetric = "kappa", ssNCategories = 2, ssNRaters = 2,
                    ssKappaNull = 0.4, ssKappaAlt = 0.7))$agreementSampleSizeTable
  expect_false("sparse" %in% note_keys(ok))
})

# ---- FIX-10 the sample size is for UNWEIGHTED kappa -------------------------
test_that("FIX-10: the sample-size table states the calculation is for unweighted kappa", {
  skip_unless_agreement()
  d <- data.frame(a = factor(rep(c("x", "y"), 6)), b = factor(rep(c("x", "y", "x"), 4)))
  tb <- q(agreement(data = d, vars = c("a", "b"),
                    agreementSampleSize = TRUE))$agreementSampleSizeTable
  expect_true("unweighted" %in% note_keys(tb))
  expect_true(grepl("unweighted", note_text(tb), fixed = TRUE))
  expect_true(grepl("does not apply to weighted kappa", note_text(tb), fixed = TRUE))

  # The ICC branch sizes a different study, so the kappa-only note is cleared.
  icc <- q(agreement(data = d, vars = c("a", "b"), agreementSampleSize = TRUE,
                     ssMetric = "icc", ssKappaNull = 0.6, ssKappaAlt = 0.8,
                     ssNRaters = 2))$agreementSampleSizeTable
  expect_false("unweighted" %in% note_keys(icc))
})

# ---- FIX-11 the subgroup table does not invite an untested comparison -------
test_that("FIX-11: the subgroup table carries the comparison note and its overlap sentence", {
  skip_unless_agreement()
  d <- ir2_cat_fixture()
  tb <- q(agreement(data = d, vars = c("A1", "A2"), agreementBySubgroup = TRUE,
                    subgroupVariable = "site",
                    subgroupMinCases = 10))$subgroupAgreementTable
  expect_gt(nrow(tb$asDF), 1)
  expect_true("comparison" %in% note_keys(tb))
  txt <- note_text(tb)
  expect_true(grepl("does not test whether agreement differs between subgroups", txt, fixed = TRUE))
  expect_true(grepl("nothing here is adjusted for the number of subgroups", txt, fixed = TRUE))

  # The run-specific sentence has to describe THIS run. Every interval overlaps
  # every other iff the largest lower bound is at or below the smallest upper
  # bound (1-D Helly), recomputed here from the table's own columns.
  lo <- as.numeric(tb$asDF$ci_lower); hi <- as.numeric(tb$asDF$ci_upper)
  ok <- is.finite(lo) & is.finite(hi)
  expect_gte(sum(ok), 2)
  if (max(lo[ok]) <= min(hi[ok])) {
    expect_true(grepl("every confidence interval overlaps every other one", txt, fixed = TRUE))
  } else {
    expect_true(grepl("does not overlap", txt, fixed = TRUE))
  }
  expect_true(grepl(sprintf("(%d of %d rows carry an interval)", sum(ok), nrow(tb$asDF)),
                    txt, fixed = TRUE))

  # Subgroups are listed in the variable's DECLARED level order, not in order of
  # first appearance in the data.
  expect_identical(as.character(tb$asDF$subgroup),
                   levels(d$site)[levels(d$site) %in% as.character(tb$asDF$subgroup)])
})

# ---- FIX-12 a distance metric that cannot act says so -----------------------
test_that("FIX-12: clusterDistance is a documented no-op on categorical ratings", {
  skip_unless_agreement()
  lv <- c("0", "1+", "2+", "3+")
  set.seed(4); n <- 60
  truth <- sample(lv, n, TRUE)
  mk <- function(p) factor(ifelse(runif(n) < p, sample(lv, n, TRUE), truth), levels = lv)
  d <- data.frame(K1 = mk(.10), K2 = mk(.12), K3 = mk(.70), K4 = mk(.15), K5 = mk(.75))

  # CONTRACT CHANGED 2026-09-23 (round 3): the inert set is four metrics now that
  # `ccc` exists, and the applied note names all four - a no-op note still listing
  # the old three would be a stale disclosure, which is the defect this block is
  # written to catch.
  tbs <- lapply(c("ccc", "correlation", "euclidean", "manhattan"), function(dm)
    q(agreement(data = d, vars = names(d), raterClustering = TRUE,
                clusterMethod = "hierarchical", clusterDistance = dm,
                clusterLinkage = "average", nClusters = 2,
                showDendrogram = FALSE, showClusterHeatmap = FALSE))$raterClusterTable)

  # The choice genuinely cannot act on categorical ratings - the distance is
  # always 1 minus the proportion of cases two raters placed alike - so the
  # tables stay byte-identical and the no-op is stated rather than faked.
  # all.equal() on two empty frames is TRUE, so require the rows first.
  expect_equal(nrow(tbs[[1]]$asDF), ncol(d))
  for (tb in tbs[-1]) expect_equal(tb$asDF, tbs[[1]]$asDF)
  for (tb in tbs) {
    expect_true("metric_ignored" %in% note_keys(tb))
    expect_true(grepl("leave this table unchanged", note_text(tb), fixed = TRUE))
    for (w in c("Concordance", "Correlation", "Euclidean", "Manhattan"))
      expect_true(grepl(w, note_text(tb), fixed = TRUE), info = w)
  }

  # The converse: the agreement metric on CONTINUOUS ratings counts exact
  # equality only, which is just as much of a trap, and is disclosed too.
  cont <- q(agreement(data = ir2_clu_fixture(), vars = c("M1","M2","M3","M4","M5"),
                      raterClustering = TRUE, clusterMethod = "hierarchical",
                      clusterDistance = "agreement", clusterLinkage = "average",
                      nClusters = 2, showDendrogram = FALSE,
                      showClusterHeatmap = FALSE))$raterClusterTable
  expect_true("exact_match" %in% note_keys(cont))
  expect_false("metric_ignored" %in% note_keys(cont))
})

# ---- FIX-13 the rater-count guard is the module's own, translated -----------
test_that("FIX-13: 8 raters produces the module's translated guard, not the upstream English", {
  skip_unless_agreement()
  d <- data.frame(a = factor(rep(c("x", "y"), 6)), b = factor(rep(c("x", "y", "x"), 4)))
  tb <- q(agreement(data = d, vars = c("a", "b"), agreementSampleSize = TRUE,
                    ssMetric = "fleiss", ssNCategories = 3,
                    ssNRaters = 8))$agreementSampleSizeTable

  # Refused, not fabricated: the guard returns before any row is written.
  expect_true(all(is.na(as.numeric(tb$asDF$value))))
  expect_true("error" %in% note_keys(tb))
  txt <- note_text(tb)
  expect_true(grepl("validated kappa sample size formulas cover 2 to 6 raters", txt, fixed = TRUE))
  expect_true(grepl("but 8 were requested", txt, fixed = TRUE))
  # kappaSize's own message is English whatever the locale, so it must not survive.
  expect_false(grepl("Sorry, this function is designed for", txt, fixed = TRUE))
})

# ---- FIX-17 CI headings follow confLevel ------------------------------------
test_that("FIX-17: CI column titles follow confLevel", {
  skip_unless_agreement()
  d <- ir2_cat_fixture()
  args <- list(data = d, vars = c("A1", "A2"), agreementBySubgroup = TRUE,
               subgroupVariable = "site", subgroupMinCases = 10)
  r95 <- q(do.call(agreement, args))
  r90 <- q(do.call(agreement, c(args, list(confLevel = 0.90))))

  t95 <- r95$subgroupAgreementTable
  t90 <- r90$subgroupAgreementTable
  expect_identical(t95$getColumn("ci_lower")$title, "95% CI Lower")
  expect_identical(t95$getColumn("ci_upper")$title, "95% CI Upper")
  expect_identical(t90$getColumn("ci_lower")$title, "90% CI Lower")
  expect_identical(t90$getColumn("ci_upper")$title, "90% CI Upper")

  # The heading and the arithmetic must agree: a 90% interval is the narrower one.
  w95 <- mean(as.numeric(t95$asDF$ci_upper) - as.numeric(t95$asDF$ci_lower), na.rm = TRUE)
  w90 <- mean(as.numeric(t90$asDF$ci_upper) - as.numeric(t90$asDF$ci_lower), na.rm = TRUE)
  expect_lt(w90, w95)
})
