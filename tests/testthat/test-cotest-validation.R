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

test_that("VAL-cotest-01 the dependence essay's direction claim holds for NEGATIVE rho too", {
    skip("VAL-cotest-01 open defect: the 'Impact of Ignoring Dependence' panel states that assuming independence makes P(D|both+) too high and P(D|both-) too low 'in 100%' of sets. Both reverse for negative rho, which the analysis permits (min -1.00) and the same panel calls legitimate. Remove this skip when fixed.")
    ind <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
               prevalence = .10, indep = TRUE)$cotestResultsTable$asDF
    neg <- cot(test1_sens = .80, test1_spec = .90, test2_sens = .75, test2_spec = .95,
               prevalence = .10, indep = FALSE,
               cond_dep_pos = -0.05, cond_dep_neg = -0.05)$cotestResultsTable$asDF
    # the panel asserts this unconditionally; it is true only for positive dependence
    expect_gt(row_of(ind, "both_pos")$postProb, row_of(neg, "both_pos")$postProb)
    expect_lt(row_of(ind, "both_neg")$postProb, row_of(neg, "both_neg")$postProb)
})

test_that("VAL-cotest-02 a combined LR below 1 is not called 'no diagnostic value'", {
    skip("VAL-cotest-02 open defect: .interpretPLR returns 'no diagnostic value' for every LR <= 1. At LR = 0.36 the same sentence simultaneously reports a 2.6-fold DECREASE, which is informative evidence against disease. Only LR = 1 has no diagnostic value. Remove this skip when fixed.")
    r <- cot(test1_sens = 0.30, test1_spec = 0.50,
             test2_sens = 0.30, test2_spec = 0.50, prevalence = 0.10, indep = TRUE)
    expect_false(grepl("no diagnostic value", r$explanation$content, fixed = TRUE))
})

test_that("VAL-cotest-03 the preset called 'strongest dependence' really is the strongest", {
    skip("VAL-cotest-03 open defect: R/cotest.b.R:58 and :1165 call mammogram_ultrasound the strongest dependence among the worked examples, but it carries cond_dep_pos = 0.25 while covid_antigen_pcr carries 0.30. Neither clamps. Remove this skip when fixed.")
    mam <- q(cotest(preset = "mammogram_ultrasound", showGuidance = FALSE))
    cov <- q(cotest(preset = "covid_antigen_pcr",    showGuidance = FALSE))
    phi <- function(r) as.numeric(sub(".*Realized phi \\(disease\\): ([0-9.-]+).*", "\\1",
                                      gsub("\n", " ", r$dependenceInfo$content)))
    expect_gt(phi(mam), phi(cov))
})

test_that("VAL-cotest-04 the low-prevalence guard is reachable", {
    skip("VAL-cotest-04 open defect: .validateInputParameters warns when prevalence < 0.001, but the option minimum IS 0.001 and jmvcore rejects anything below it, so the branch is dead code. At the reachable minimum (0.001, the most unstable point) no warning fires. Remove this skip when fixed.")
    r <- cot(prevalence = 0.001)
    expect_match(r$notices$content, "prevalence", ignore.case = TRUE)
})
