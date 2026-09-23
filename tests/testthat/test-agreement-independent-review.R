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
# The helper therefore falls back to a distinct-value count (> 20 distinct), which
# is a property of the SAMPLE and so is sample-dependent by construction.
# Remedies for the ambiguous case, both stated in the module's own notes: wrap the
# column in factor() when calling from R, or set the variable to Nominal/Ordinal in
# jamovi. IR-03b below pins the limitation so a future change cannot re-break the
# integer-coded ratings while claiming to have "fixed" the sample dependence.

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

test_that("IR-03b: an INTEGER-valued numeric column is read as category codes below the distinct-count threshold", {
  skip_unless_agreement()
  # This is the documented limitation, not an aspiration. Integers, few distinct
  # values -> category codes -> kappa. See the block comment above for why no
  # predicate can do better on the R path, and for the two remedies.
  small <- q(agreement(data = data.frame(R1 = c(1, 2, 1, 2, 1), R2 = c(1, 2, 2, 2, 1)),
                       vars = c("R1","R2")))$irrtable$asDF
  expect_false(grepl("not computed", as.character(small$method[1]), fixed = TRUE))
  expect_false(is.na(as.numeric(small$kappa[1])))

  # ... and the same INTEGER shape with many distinct values flips to continuous.
  # The branch really is sample-dependent here; that is the cost of the fallback.
  set.seed(9); x <- round(runif(60, 5, 60), 0)
  big <- q(agreement(data = data.frame(R1 = x, R2 = pmax(0, x + sample(c(-3,-1,0,1,3), 60, TRUE))),
                     vars = c("R1","R2")))$irrtable$asDF
  expect_match(as.character(big$method[1]), "continuous")
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
