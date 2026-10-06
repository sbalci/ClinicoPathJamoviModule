# Regression tests from /validate-function cotest depth=quick (2026-09-20).
# Oracle for every live test below is HAND CALCULATION in exact rational arithmetic, stated in the
# comment above each block. cotest is a closed-form calculator: no third-party statistical package
# computes this, so battery B (reference parity) has no independent implementation and the hand
# calculation is the oracle (EXEC-IND).
#
# Confirmed open defects are written with the CORRECT expectation and skip-guarded, so the fix
# lands on a test that already exists. Remove the skip() when the defect is fixed.

library(ClinicoPath)

q <- function(expr) suppressWarnings(suppressMessages(force(expr)))
# jmvcore's asDF returns row names WITH literal quote characters ("\"both_pos\""), so a bare
# rownames match silently yields a 0-row frame and $col on it is numeric(0).
row_of <- function(df, key) {
    r <- df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
    if (nrow(r) != 1L) stop(sprintf("row key %s matched %d rows", key, nrow(r)))
    r
}
cot <- function(...) q(cotest(..., showGuidance = FALSE, preset = "custom"))

test_that("C01/C02 single-test likelihood ratios are sens/(1-spec) and (1-sens)/spec", {
    # PLR1 = .80/.10 = 8 ; NLR1 = .20/.90 = 2/9 ; PLR2 = .75/.05 = 15 ; NLR2 = .25/.95 = 5/19
    tp <- cot(test1_sens = 0.80, test1_spec = 0.90,
              test2_sens = 0.75, test2_spec = 0.95,
              prevalence = 0.10, indep = TRUE)$testParamsTable$asDF
    expect_equal(row_of(tp, "test1")$plr, 8,     tolerance = 1e-12)
    expect_equal(row_of(tp, "test1")$nlr, 2 / 9, tolerance = 1e-12)
    expect_equal(row_of(tp, "test2")$plr, 15,    tolerance = 1e-12)
    expect_equal(row_of(tp, "test2")$nlr, 5 / 19, tolerance = 1e-12)
})

test_that("C05/C06 independent-model posteriors equal the exact-fraction hand calculation", {
    # pre-test odds = 1/9. both+ LR = 8*15 = 120 -> P = 120/129. t1 only LR = 8*(5/19) = 40/19
    # -> P = 40/211. t2 only LR = (2/9)*15 = 10/3 -> P = 10/37. both- LR = 10/171 -> P = 10/1549.
    # either+ : P(e|D) = 1-.2*.25 = .95, P(e|nD) = 1-.9*.95 = .145, LR = 190/29 -> P = 190/451.
    cr <- cot(test1_sens = 0.80, test1_spec = 0.90,
              test2_sens = 0.75, test2_spec = 0.95,
              prevalence = 0.10, indep = TRUE)$cotestResultsTable$asDF
    expect_equal(row_of(cr, "both_pos")$postProb,   120 / 129,  tolerance = 1e-12)
    expect_equal(row_of(cr, "test1_pos")$postProb,  40 / 211,   tolerance = 1e-12)
    expect_equal(row_of(cr, "test2_pos")$postProb,  10 / 37,    tolerance = 1e-12)
    expect_equal(row_of(cr, "both_neg")$postProb,   10 / 1549,  tolerance = 1e-12)
    expect_equal(row_of(cr, "either_pos")$postProb, 190 / 451,  tolerance = 1e-12)
    expect_equal(row_of(cr, "both_pos")$orValue,    120 / 9,    tolerance = 1e-12)
    expect_equal(row_of(cr, "either_pos")$orValue,  190 / 261,  tolerance = 1e-12)
})

test_that("G04 the four exclusive cells return the prevalence by total probability", {
    # sum_cell P(D|cell) * P(cell) must equal the prevalence exactly, using only displayed
    # posteriors and the entered marginals.
    prev <- 0.10
    cr <- cot(test1_sens = 0.80, test1_spec = 0.90,
              test2_sens = 0.75, test2_spec = 0.95,
              prevalence = prev, indep = TRUE)$cotestResultsTable$asDF
    pc <- function(pD, pnD) pD * prev + pnD * (1 - prev)
    tot <- row_of(cr, "both_pos")$postProb  * pc(0.80 * 0.75, 0.10 * 0.05) +
           row_of(cr, "test1_pos")$postProb * pc(0.80 * 0.25, 0.10 * 0.95) +
           row_of(cr, "test2_pos")$postProb * pc(0.20 * 0.75, 0.90 * 0.05) +
           row_of(cr, "both_neg")$postProb  * pc(0.20 * 0.25, 0.90 * 0.95)
    expect_equal(tot, prev, tolerance = 1e-12)
})

test_that("M03 rho = 0 under the dependent model reproduces conditional independence", {
    a <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
             prevalence = .10, indep = TRUE)$cotestResultsTable$asDF
    b <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
             prevalence = .10, indep = FALSE,
             cond_dep_pos = 0, cond_dep_neg = 0)$cotestResultsTable$asDF
    expect_equal(as.numeric(b$postProb), as.numeric(a$postProb), tolerance = 1e-12)
})

test_that("SET D an arm that is impossible under both hypotheses is left blank, not numbered", {
    # rho = 0.90 exceeds the Frechet bound in both groups: P(T1-,T2+) clamps to exactly 0 under
    # D and under nD, so that likelihood ratio is 0/0 and no posterior exists.
    cr <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
              prevalence = .10, indep = FALSE,
              cond_dep_pos = 0.90, cond_dep_neg = 0.90)$cotestResultsTable$asDF
    expect_true(is.na(row_of(cr, "test2_pos")$postProb))
    # the surviving arms remain exact: LR(both+) = .75/.05 = 15 -> odds 15/9 -> P = 0.625
    expect_equal(row_of(cr, "both_pos")$postProb,   0.625,      tolerance = 1e-12)
    expect_equal(row_of(cr, "either_pos")$postProb, 8 / 17,     tolerance = 1e-12)
    expect_equal(row_of(cr, "test1_pos")$postProb,  0.10,       tolerance = 1e-12)
})

test_that("VAL-cotest-01 the dependence essay no longer states a universal direction (negative rho)", {
    # Fixed 2026-10-05. The essay said independence makes P(D|both+) too high and P(D|both-) too
    # low "in 100%" of sets; both reverse for negative rho. The arithmetic is right; the claim went.
    ind <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
               prevalence = .10, indep = TRUE)$cotestResultsTable$asDF
    neg <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
               prevalence = .10, indep = FALSE,
               cond_dep_pos = -0.05, cond_dep_neg = -0.05)$cotestResultsTable$asDF
    expect_lt(row_of(ind, "both_pos")$postProb, row_of(neg, "both_pos")$postProb)
    expect_gt(row_of(ind, "both_neg")$postProb, row_of(neg, "both_neg")$postProb)
    essay <- gsub("<[^>]+>", " ", q(cotest(showGuidance = TRUE))$dependenceExplanation$content)
    expect_false(grepl("100%", essay, fixed = TRUE))
    expect_false(grepl("85%", essay, fixed = TRUE))
    expect_match(essay, "no single direction holds for every row", fixed = TRUE)
})

test_that("VAL-cotest-02 a combined LR below 1 is not called 'no diagnostic value'", {
    # Fixed by 2026-10-05 (re-validated depth=exhaustive): .interpretPLR now has an LR < 1 branch.
    r <- cot(test1_sens = 0.30, test1_spec = 0.50,
             test2_sens = 0.30, test2_spec = 0.50, prevalence = 0.10, indep = TRUE)
    expect_false(grepl("no diagnostic value", r$explanation$content, fixed = TRUE))
})

test_that("VAL-cotest-03 no worked example is called 'the strongest dependence'", {
    # Fixed 2026-10-05 by rewording: mammogram_ultrasound (rho+ .25) is not the strongest -
    # covid_antigen_pcr carries .30 - but it is the one dependent in both groups.
    mam <- q(cotest(preset = "mammogram_ultrasound", showGuidance = TRUE))
    expect_false(grepl("strongest dependence", mam$notices$content, fixed = TRUE))
    expect_false(grepl("strongest dependence", mam$instructions$content, fixed = TRUE))
})

test_that("VAL-cotest-04 the low-prevalence guard is reachable", {
    # Fixed by 2026-10-05 (re-validated depth=exhaustive): the guard is now prevalence < 0.05.
    r <- cot(prevalence = 0.001)
    expect_match(r$notices$content, "prevalence", ignore.case = TRUE)
})

# =============================================================================================
# Added by /validate-function cotest depth=exhaustive (2026-10-05).
# Live tests: oracle values come from a BLINDED Python implementation (decimal, 50 digits) built
# from the mathematical specification only; it never saw R/cotest.b.R. Script and full matrix:
# development-scripts/validate_cotest.R, development-ideas/cotest-validation-2026-10-05.md.
# =============================================================================================
txt_of <- function(h) trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", h)))
posts_of <- function(r) {
    df <- r$cotestResultsTable$asDF
    vapply(c("either_pos", "test1_pos", "test2_pos", "both_pos", "both_neg"),
           function(k) row_of(df, k)$postProb, 0)
}

test_that("C05 negative rho+ truncated at the Frechet FLOOR matches the blinded oracle (O5)", {
    # s .80/.70, c .90/.95, prev .20, rho+ -.50 (floor .5 -> p00|D = 0), rho- .10
    r <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .70, test2_spec = .95,
             prevalence = .20, indep = FALSE, cond_dep_pos = -.50, cond_dep_neg = .10)
    expect_equal(unname(posts_of(r)),
                 c(0.64356416902453806, 0.45882321188430380, 0.56521666851463169,
                   0.91549371623996344, 0), tolerance = 1e-10)
    expect_match(txt_of(r$dependenceInfo$content), "Realized phi (disease): -0.33", fixed = TRUE)
})

test_that("C05/D02 structural probability 1 has Inf odds and a 'Not an estimate' footnote (O7)", {
    # spec .99/.90 with rho- .40 truncates P(T1+,T2-|nD) to 0: test-1-only LR is infinite
    r <- cot(test1_sens = .80, test1_spec = .99, test2_sens = .75, test2_spec = .90,
             prevalence = .10, indep = FALSE, cond_dep_pos = .05, cond_dep_neg = .40)
    expect_equal(unname(posts_of(r)),
                 c(0.51122545311173804, 1, 0.14856915898032723, 0.87118202376641142,
                   0.0071899370989020580), tolerance = 1e-10)
    expect_true(is.infinite(row_of(r$cotestResultsTable$asDF, "test1_pos")$orValue))
    fns <- r$cotestResultsTable$getCell(rowKey = "test1_pos", col = "postProb")$footnotes
    expect_true(any(grepl("Not an estimate", fns, fixed = TRUE)))
})

test_that("C05 positive-dependence sets O3 and O4 match the blinded oracle", {
    o3 <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
              prevalence = .10, indep = FALSE, cond_dep_pos = .30, cond_dep_neg = .005)
    o4 <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
              prevalence = .10, indep = FALSE, cond_dep_pos = .005, cond_dep_neg = .30)
    expect_equal(unname(posts_of(o3)),
                 c(0.40818145498509689, 0.14802423303132151, 0.19603922615473655,
                   0.93150164624423714, 0.013072151540542687), tolerance = 1e-10)
    expect_equal(unname(posts_of(o4)),
                 c(0.45684185072954382, 0.22690781639499999, 0.39495376168898448,
                   0.73062375814644352, 0.0064205311781305106), tolerance = 1e-10)
})

test_that("C05 worked example hpv_pap reproduces the blinded oracle (O8)", {
    r <- q(cotest(preset = "hpv_pap", showGuidance = FALSE))
    expect_equal(unname(posts_of(r)),
                 c(0.21653861754322547, 0.095424551260014853, 0.029454206960780547,
                   0.70076388298789970, 0.0019317381397656998), tolerance = 1e-10)
})

test_that("M04 relabelling disease/positive as no-disease/negative maps both+ to 1 - both-", {
    # (s, c, prev, rho+, rho-) -> (c, s, 1 - prev, rho-, rho+): P'(D'|both+) = 1 - P(D|both-),
    # P'(D'|t1 only) = 1 - P(D|t2 only). A property of Bayes' rule, no oracle needed.
    a <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
             prevalence = .10, indep = FALSE, cond_dep_pos = .20, cond_dep_neg = .10)
    b <- cot(test1_sens = .90, test1_spec = .80, test2_sens = .95, test2_spec = .75,
             prevalence = .90, indep = FALSE, cond_dep_pos = .10, cond_dep_neg = .20)
    pa <- posts_of(a); pb <- posts_of(b)
    expect_equal(pb[["both_pos"]],  1 - pa[["both_neg"]],  tolerance = 1e-10)
    expect_equal(pb[["both_neg"]],  1 - pa[["both_pos"]],  tolerance = 1e-10)
    expect_equal(pb[["test1_pos"]], 1 - pa[["test2_pos"]], tolerance = 1e-10)
})

test_that("M05 combined likelihood ratios do not depend on the prevalence", {
    lr <- function(p) {
        df <- cot(prevalence = p, indep = FALSE, cond_dep_pos = .20, cond_dep_neg = .10)$cotestResultsTable$asDF
        as.numeric(df$orValue) / (p / (1 - p))
    }
    expect_equal(lr(.30), lr(.10), tolerance = 1e-10)
    expect_equal(lr(.70), lr(.10), tolerance = 1e-10)
})

test_that("M06 raising rho+ raises P(D|both+) and P(D|both-) and lowers P(D|either+)", {
    g <- seq(-0.20, 0.85, by = 0.05)
    P <- sapply(g, function(x) posts_of(cot(indep = FALSE, cond_dep_pos = x, cond_dep_neg = .10)))
    expect_true(all(diff(P["both_pos", ])   >= -1e-12))
    expect_true(all(diff(P["both_neg", ])   >= -1e-12))
    expect_true(all(diff(P["either_pos", ]) <=  1e-12))
})

test_that("N10 under independence, either+ and both- equal sequentialtests' parallel PPV and 1 - NPV", {
    skip_if_not(exists("sequentialtests"))
    # independent implementation of the same rule in this module (instructions-panel claim)
    sq <- q(sequentialtests(preset = "custom", test1_name = "A", test1_sens = .80, test1_spec = .90,
                            test1_cost = 0, test2_name = "B", test2_sens = .75, test2_spec = .95,
                            test2_cost = 0, strategy = "parallel", prevalence = .10,
                            population_size = 1000, show_explanation = FALSE, show_formulas = FALSE,
                            show_cost_analysis = FALSE, show_plots = FALSE))$summary_table$asDF
    p <- posts_of(cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
                      prevalence = .10, indep = TRUE))
    expect_equal(p[["either_pos"]], sq$combined_ppv[1], tolerance = 1e-6)
    expect_equal(p[["both_neg"]],   1 - sq$combined_npv[1], tolerance = 1e-6)
})

test_that("I01 the Fagan nomogram plots the parallel-rule LRs shown in the table", {
    r <- cot(fagan = TRUE)
    df <- r$cotestResultsTable$asDF; po <- .10 / .90
    expect_equal(r$plot1$state$Plr_PositiveRule, row_of(df, "either_pos")$orValue / po, tolerance = 1e-12)
    expect_equal(r$plot1$state$Nlr_NegativeRule, row_of(df, "both_neg")$orValue / po,   tolerance = 1e-12)
})

test_that("V01-V04 guard thresholds fire on the stated side of the boundary", {
    has <- function(r, s) grepl(s, txt_of(r$notices$content), fixed = TRUE)
    yd <- "Test 1 has low discriminatory power"
    expect_true(has(cot(test1_sens = .50, test1_spec = .59), yd))
    expect_false(has(cot(test1_sens = .50, test1_spec = .61), yd))
    expect_true(has(cot(prevalence = .049), "Low prevalence"))
    expect_false(has(cot(prevalence = .051), "Low prevalence"))
    expect_false(has(cot(prevalence = .50), "High prevalence (above 50%)"))
    expect_true(has(cot(prevalence = .501), "High prevalence (above 50%)"))
    expect_true(has(cot(prevalence = .951), "Very high prevalence"))
})

test_that("D05 the Frechet truncation warning fires just above the attainable rho+, not below", {
    cap <- (min(.80, .75) - .80 * .75) / sqrt(.80 * .20 * .75 * .25)   # 0.8660254
    has <- function(r) grepl("for the diseased group", txt_of(r$notices$content), fixed = TRUE)
    expect_false(has(cot(indep = FALSE, cond_dep_pos = cap - 1e-4)))
    expect_true(has(cot(indep = FALSE, cond_dep_pos = cap + 1e-4)))
})

# ---- confirmed open defects: CORRECT expectation, skip-guarded -------------------------------

test_that("VAL-cotest-01 (extended) the essay's direction claims fail for POSITIVE dependence too", {
    # Fixed 2026-10-05 (/fix-function cotest).
    ind <- posts_of(cot(indep = TRUE))
    o3  <- posts_of(cot(indep = FALSE, cond_dep_pos = .30, cond_dep_neg = .005))
    o4  <- posts_of(cot(indep = FALSE, cond_dep_pos = .005, cond_dep_neg = .30))
    # the correct statement: the direction depends on rho+ relative to rho-
    expect_lt(ind[["both_pos"]], o3[["both_pos"]])
    expect_gt(ind[["both_neg"]], o4[["both_neg"]])
    essay <- txt_of(q(cotest(showGuidance = TRUE))$dependenceExplanation$content)
    expect_false(grepl("this held in 100% of them", essay, fixed = TRUE))
    expect_match(essay, "depends on where the dependence lies", fixed = TRUE)
})

test_that("VAL-cotest-07 a rule-in from 92% to 99.9% is not called 'minimal change'", {
    # Fixed 2026-10-05 (/fix-function cotest).
    r <- cot(test1_sens = .90, test1_spec = .99, test2_sens = .50, test2_spec = .65,
             prevalence = .92, indep = TRUE)
    expect_false(grepl("(minimal change)", txt_of(r$explanation$content), fixed = TRUE))
    # read on the combined-LR scale: LR 69.2 -> large shift, whatever the prevalence
    expect_match(txt_of(r$explanation$content),
                 "Test 1 positive only: 99.87% - combined likelihood ratio 69.2; by the usual rule of thumb, a large shift towards disease",
                 fixed = TRUE)
})

test_that("VAL-cotest-08 a NEGATIVE rho truncated at the floor is not called 'the largest value'", {
    # Fixed 2026-10-05 (/fix-function cotest).
    r <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .70, test2_spec = .95,
             prevalence = .20, indep = FALSE, cond_dep_pos = -.50, cond_dep_neg = .10)
    expect_false(grepl("truncated to the largest value", txt_of(r$explanation$content), fixed = TRUE))
    expect_false(grepl("Lower the conditional dependence", txt_of(r$notices$content), fixed = TRUE))
    expect_match(txt_of(r$explanation$content), "truncated to the nearest value", fixed = TRUE)
    expect_match(txt_of(r$explanation$content), "is not an estimate", fixed = TRUE)
})

test_that("VAL-cotest-09 a non-zero post-test probability is never printed as 0.0% or 100.0%", {
    # Fixed 2026-10-05 (/fix-function cotest).
    lo <- cot(prevalence = .001, indep = TRUE)
    hi <- cot(test1_sens = .99, test1_spec = .99, test2_sens = .99, test2_spec = .99,
              prevalence = .999, indep = TRUE)
    expect_false(grepl("disease probability is 0.0%", txt_of(lo$explanation$content), fixed = TRUE))
    expect_false(grepl("disease probability is 100.0%", txt_of(hi$explanation$content), fixed = TRUE))
    # P(D|both-) = 5.85e-5 and P(D|both+) = 0.9999999 keep significant digits
    expect_match(txt_of(lo$explanation$content), "0.0059% when both were negative", fixed = TRUE)
    expect_match(txt_of(hi$explanation$content), ">99.999% when both tests were positive", fixed = TRUE)
})

test_that("VAL-cotest-10 an unchanged probability is not described as an increase or a reduction", {
    # Fixed 2026-10-05 (/fix-function cotest).
    r <- cot(test1_sens = .5, test1_spec = .5, test2_sens = .5, test2_spec = .5,
             prevalence = .3, indep = TRUE)
    e <- txt_of(r$explanation$content)
    expect_false(grepl("1.0x increase", e, fixed = TRUE))
    expect_false(grepl("reduced to 1.00x", e, fixed = TRUE))
    expect_match(e, "30.0% when both were negative (about the same as the prevalence)", fixed = TRUE)
})

test_that("VAL-cotest-11 the copy-ready summary reports the parallel rule's positive arm", {
    # Fixed 2026-10-05 (/fix-function cotest).
    r <- cot(indep = TRUE)
    e <- sub(".*Copy-ready summary:", "", txt_of(r$explanation$content))
    expect_match(e, sprintf("%.1f%%", 100 * posts_of(r)[["either_pos"]]), fixed = TRUE)
})

# =============================================================================================
# Added after /independent-reviewer cotest (2026-10-05) and the fix pass that followed. IR-NN are
# the review's finding numbers (development-ideas/cotest-independent-review-2026-10-05.md).
# =============================================================================================
notes_of <- function(r) txt_of(r$notices$content)

test_that("IR-02 the dependence boxes name their scale and explain covariance conversion", {
    # the option title reaches the user in the worked-example override notice
    n <- notes_of(q(cotest(preset = "hpv_pap", cond_dep_pos = .30, showGuidance = FALSE)))
    expect_match(n, "Correlation (phi) among subjects with disease (0.3 given, 0.15 used)", fixed = TRUE)
    essay <- txt_of(q(cotest(showGuidance = TRUE))$dependenceExplanation$content)
    expect_match(essay, "not a covariance or a kappa", fixed = TRUE)
})

test_that("IR-03 extreme-prevalence notices ask for a check instead of claiming prevalence drives the result", {
    # prevalence .96 with two .99-sensitive tests: both- = 0.3%, so "will stay high" was false
    hi <- notes_of(cot(test1_sens = .99, test1_spec = .90, test2_sens = .99, test2_spec = .90,
                       prevalence = .96, indep = TRUE))
    lo <- notes_of(cot(test1_sens = .95, test1_spec = .99, test2_sens = .90, test2_spec = .99,
                       prevalence = .04, indep = TRUE))
    expect_false(grepl("will stay high", hi, fixed = TRUE))
    expect_false(grepl("driven mainly by the prevalence", lo, fixed = TRUE))
    expect_match(hi, "Very high prevalence (above 95%)", fixed = TRUE)
    expect_match(lo, "Low prevalence (below 5%)", fixed = TRUE)
})

test_that("IR-04 / IR-13 no unconditional 'optimistic' or 'moves the answer most' claims; no phantom 'below'", {
    r <- q(cotest(indep = TRUE, showGuidance = TRUE))
    expect_false(grepl("moves the answer more than any other", txt_of(r$instructions$content), fixed = TRUE))
    di <- txt_of(r$dependenceInfo$content)
    expect_false(grepl("Independence is the optimistic assumption", di, fixed = TRUE))
    expect_false(grepl("joint probabilities below", di, fixed = TRUE))
})

test_that("IR-05 the copy-ready sentence states a calculation and keeps the entered precision", {
    e <- txt_of(cot(test1_sens = .925, test1_spec = .875, test2_sens = .985, test2_spec = .845)$explanation$content)
    expect_false(grepl("were applied", e, fixed = TRUE))
    expect_match(e, "The calculation assumes both tests are performed in parallel", fixed = TRUE)
    expect_match(e, "(sensitivity 92.5%, specificity 87.5%)", fixed = TRUE)
})

test_that("IR-06 a test below the chance diagonal is named as possibly reversed", {
    n <- notes_of(cot(test1_sens = .20, test1_spec = .30, indep = TRUE))
    expect_match(n, "Test 1 is below the chance diagonal", fixed = TRUE)
    expect_false(grepl("Test 1 has low discriminatory power", n, fixed = TRUE))
    # just above the diagonal keeps the weak-test note
    expect_match(notes_of(cot(test1_sens = .50, test1_spec = .55, indep = TRUE)), "Test 1 has low discriminatory power", fixed = TRUE)
})

test_that("IR-07 the instructions warn about where sensitivity and specificity come from", {
    ins <- txt_of(q(cotest(showGuidance = TRUE))$instructions$content)
    expect_match(ins, "spectrum and verification bias", fixed = TRUE)
})

test_that("IR-08 untouched default dependence is marked as a default in the copy-ready sentence", {
    expect_match(txt_of(cot()$explanation$content), "analysis defaults, not estimates from data", fixed = TRUE)
    expect_false(grepl("analysis defaults", txt_of(cot(cond_dep_pos = .2)$explanation$content), fixed = TRUE))
    expect_false(grepl("analysis defaults", txt_of(cot(indep = TRUE)$explanation$content), fixed = TRUE))
})

test_that("IR-09 a tag-like '<' in a test name is broken in table cells; '<' before a digit is kept", {
    r <- cot(test1_name = "HPV <LOD", test2_name = "Ki-67 <10%")
    expect_identical(row_of(r$testParamsTable$asDF, "test1")$test, "HPV < LOD")
    expect_identical(row_of(r$testParamsTable$asDF, "test2")$test, "Ki-67 <10%")
})

test_that("IR-10 essay directions are scoped to tests better than chance", {
    essay <- txt_of(q(cotest(showGuidance = TRUE))$dependenceExplanation$content)
    expect_match(essay, "For tests that do better than chance", fixed = TRUE)
})

test_that("IR-12 a Fagan refusal caused by a truncated dependence names the dependence", {
    n <- notes_of(cot(test1_sens = .90, test1_spec = .90, test2_sens = .90, test2_spec = .95,
                      prevalence = .10, indep = FALSE, cond_dep_pos = -.5, fagan = TRUE))
    expect_match(n, "the dependence is at, or was truncated to, the limit", fixed = TRUE)
})

test_that("IR-14 no branch-dependent 'Very high ... ratio' note", {
    vh <- function(indep) grepl("Very high", notes_of(cot(test1_sens = .99, test1_spec = .99, test2_sens = .99,
                                                           test2_spec = .99, indep = indep,
                                                           cond_dep_pos = 0, cond_dep_neg = 0)), fixed = TRUE)
    expect_identical(vh(TRUE), vh(FALSE))
})

test_that("IR-15 / IR-17 the table notes use the panel's LR bands and the exact inversion criterion", {
    tp <- cot()$testParamsTable
    note <- function(k) get("note", envir = tp$.__enclos_env__$private$.notes[[k]])
    expect_match(note("lr_defs"), "0.5 to 2 as little change", fixed = TRUE)
    expect_match(note("typical_ranges"), "sensitivity plus specificity is below 1", fixed = TRUE)
})

test_that("IR-16 a typed name on a worked example is flagged", {
    expect_match(notes_of(q(cotest(preset = "hpv_pap", test1_name = "Ki67", showGuidance = FALSE))),
                 "You named the tests Ki67", fixed = TRUE)
})

test_that("IR-16 the example's own names, as events.js writes them into the GUI boxes, are not flagged", {
    # Every worked example in the GUI used to warn "the numbers do not describe those tests".
    expect_no_match(notes_of(q(cotest(preset = "hpv_pap", test1_name = "HPV",
                                      test2_name = "pap cytology", showGuidance = FALSE))),
                    "You named the tests", fixed = TRUE)
    n <- notes_of(q(cotest(preset = "hpv_pap", test1_name = "HPV", test2_name = "Ki67",
                           showGuidance = FALSE)))
    expect_match(n, "You named the tests Ki67, but", fixed = TRUE)
})

test_that("the low-power notice names the test as the tables do and points at the table holding the LRs", {
    n <- notes_of(cot(test1_name = "Ki67", test1_sens = .50, test1_spec = .55, indep = TRUE))
    expect_match(n, "Ki67 has low discriminatory power", fixed = TRUE)
    expect_match(n, "likelihood ratios in the Test Parameters table", fixed = TRUE)
})
