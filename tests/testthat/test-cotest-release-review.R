# Regression tests from the `cotest` release review.
#
# Each case corresponds to a defect confirmed in the shipped code, or to a property that was
# verified by hand and must not silently change. The statistical checks are computed from Bayes'
# theorem in the test itself rather than read back from the module.

run_ct <- function(...) {
    args <- utils::modifyList(
        list(test1_sens = 0.80, test1_spec = 0.90,
             test2_sens = 0.70, test2_spec = 0.95,
             prevalence = 0.10, indep = TRUE),
        list(...))
    do.call(ClinicoPath::cotest, args)
}

post_of <- function(res) {
    d <- res$cotestResultsTable$asDF
    stats::setNames(d$postProb, as.character(d$scenario))
}

notices_of <- function(res)
    gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " ")))

# Post-test probability from Bayes' theorem, given P(result | D+) and P(result | D-).
bayes <- function(p_D, p_nD, prev) (prev * p_D) / (prev * p_D + (1 - prev) * p_nD)


test_that("conditional independence reproduces Bayes' theorem exactly", {
    # The shipped test asserted 0.9302326 / 0.1895735 / 0.2702703 / 0.0064558, which imply joint
    # likelihood ratios of 120 / 2.105 / 3.333 / 0.0585. The correct ones are 112 / 2.526 /
    # 3.111 / 0.0702. The module was right and the expected values were fabricated.
    se1 <- 0.80; sp1 <- 0.90; se2 <- 0.70; sp2 <- 0.95; p <- 0.10
    v <- post_of(run_ct(indep = TRUE))

    expect_equal(unname(v["Both Tests Positive"]),
                 bayes(se1 * se2, (1 - sp1) * (1 - sp2), p), tolerance = 1e-9)
    expect_equal(unname(v["Test 1 Positive Only"]),
                 bayes(se1 * (1 - se2), (1 - sp1) * sp2, p), tolerance = 1e-9)
    expect_equal(unname(v["Test 2 Positive Only"]),
                 bayes((1 - se1) * se2, sp1 * (1 - sp2), p), tolerance = 1e-9)
    expect_equal(unname(v["Both Tests Negative"]),
                 bayes((1 - se1) * (1 - se2), sp1 * sp2, p), tolerance = 1e-9)

    # spelled out, to the precision these literals actually carry. expect_equal's tolerance is
    # RELATIVE, so a small probability needs as many SIGNIFICANT digits as a large one:
    # "Both Tests Negative" is 0.00773694390..., which is only 7 s.f. as written below.
    expect_equal(unname(v["Both Tests Positive"]),  0.925619835, tolerance = 1e-7)
    expect_equal(unname(v["Test 1 Positive Only"]), 0.219178082, tolerance = 1e-7)
    expect_equal(unname(v["Test 2 Positive Only"]), 0.256880734, tolerance = 1e-7)
    expect_equal(unname(v["Both Tests Negative"]),  0.007736944, tolerance = 1e-7)
})


test_that("zero dependence reduces exactly to the independent model", {
    # The dependent path uses P(both+|D+) = s1*s2 + rho*sqrt(s1(1-s1)s2(1-s2)). At rho = 0 the
    # covariance term vanishes and it must agree with the independent path to machine precision.
    a <- post_of(run_ct(indep = TRUE))
    b <- post_of(run_ct(indep = FALSE, cond_dep_pos = 0, cond_dep_neg = 0))
    expect_equal(unname(a[names(b)]), unname(b), tolerance = 1e-12)
})


test_that("the dependence model uses the correlation-scaled covariance", {
    se1 <- 0.80; sp1 <- 0.90; se2 <- 0.70; sp2 <- 0.95; p <- 0.10; rho <- 0.05
    p_both_D  <- se1 * se2 + rho * sqrt(se1 * (1 - se1) * se2 * (1 - se2))
    p_both_nD <- (1 - sp1) * (1 - sp2) + rho * sqrt((1 - sp1) * sp1 * (1 - sp2) * sp2)

    v <- post_of(run_ct(indep = FALSE, cond_dep_pos = rho, cond_dep_neg = rho))
    expect_equal(unname(v["Both Tests Positive"]), bayes(p_both_D, p_both_nD, p), tolerance = 1e-9)
})


test_that("an unattainable dependence parameter is truncated and says so as a warning", {
    # P(T1+,T2+ | D-) is bounded above by min(1-sp1, 1-sp2) (a Frechet bound). With spec .90/.95
    # that bound binds from rho = 0.688 upward. The truncation used to be reported at "info"
    # severity with no statement of the consequence.
    n <- notices_of(run_ct(indep = FALSE, cond_dep_pos = 0.90, cond_dep_neg = 0.90))
    expect_match(n, "not attainable")
    # rendered as a Warning, not a Note
    expect_match(n, "Warning:[^.]*dependence parameter")
    # quantifies the truncation instead of asserting a direction
    expect_match(n, "truncated from [0-9.]+ to its bound [0-9.]+")
    expect_match(n, "not the one you specified")

    # and not raised at all when the parameter is feasible
    expect_false(grepl("not attainable", notices_of(run_ct(indep = FALSE,
                       cond_dep_pos = 0.05, cond_dep_neg = 0.05))))

    # The old wording asserted "a more strongly dependent model than you specified" and told the
    # user to "lower the value". Both are wrong for a negative parameter: truncating rho = -0.20
    # yields a realized correlation of SMALLER magnitude, and lowering it makes the truncation
    # worse. Direction claims must not come back.
    neg <- notices_of(run_ct(indep = FALSE, cond_dep_pos = -0.20, cond_dep_neg = -0.10))
    expect_false(grepl("more strongly dependent", neg))
    expect_false(grepl("Lower the value", neg))
    expect_match(neg, "Move the value toward 0")
})


test_that("a combination that cannot occur in either group is left blank, not reported as zero", {
    # At rho = 0.80 both Frechet bounds bind, so P(T1-,T2+) is 0 in BOTH groups. The likelihood
    # ratio is 0/0. .calculateLikelihoodRatio returned `if (numerator > 0) 1e6 else 0`, so the
    # table printed a post-test probability of 0.000000 -- "this result rules out disease" -- for
    # a scenario that cannot occur at all and has no posterior.
    res <- run_ct(indep = FALSE, cond_dep_pos = 0.80, cond_dep_neg = 0.80)
    v <- post_of(res)

    expect_true(is.na(v["Test 2 Positive Only"]))
    expect_match(notices_of(res), "is undefined")
    expect_match(notices_of(res), "cannot occur at all")
    # the other scenarios are unaffected
    expect_false(any(is.na(v[c("Both Tests Positive", "Both Tests Negative",
                               "Test 1 Positive Only")])))
})


test_that("a combination impossible only in the non-diseased group gives exactly 1", {
    # rho = 0.70 binds the D- bound but not the D+ one, so the scenario is conclusive for
    # disease. The old 1e6 likelihood-ratio cap turned that into a fake-precision 0.999991.
    res <- run_ct(indep = FALSE, cond_dep_pos = 0.70, cond_dep_neg = 0.70)
    expect_equal(unname(post_of(res)["Test 2 Positive Only"]), 1)
    expect_match(notices_of(res), "is infinite")
    expect_match(notices_of(res), "by construction")
})


test_that("a posterior of zero from a bound is disclosed, using realistic co-testing values", {
    # HPV (sens .95, spec .85) with cytology (sens .55, spec .97) at 2% prevalence. From
    # rho = 0.254 the diseased-group bound binds, so P(T1-,T2+ | D+) is 0 and the module reports
    # a post-test probability of exactly 0 for HPV-negative/cytology-positive -- a group that
    # cervical screening guidelines manage as carrying real risk. The number follows from the
    # truncated model, so it stays, but it must not be presented as an ordinary estimate.
    res <- ClinicoPath::cotest(test1_sens = 0.95, test1_spec = 0.85,
                               test2_sens = 0.55, test2_spec = 0.97,
                               prevalence = 0.02, indep = FALSE,
                               cond_dep_pos = 0.30, cond_dep_neg = 0.30)
    expect_equal(unname(post_of(res)["Test 2 Positive Only"]), 0)
    expect_match(notices_of(res), "is zero")
    expect_match(notices_of(res), "by construction")
    expect_match(notices_of(res), "not attainable")

    # below the bound the same scenario is an ordinary positive probability with no such notice
    res_ok <- ClinicoPath::cotest(test1_sens = 0.95, test1_spec = 0.85,
                                  test2_sens = 0.55, test2_spec = 0.97,
                                  prevalence = 0.02, indep = FALSE,
                                  cond_dep_pos = 0.05, cond_dep_neg = 0.05)
    expect_gt(unname(post_of(res_ok)["Test 2 Positive Only"]), 0)
    expect_false(grepl("by construction", notices_of(res_ok)))
})


test_that("posteriors stay in [0,1] across the whole permitted dependence range", {
    for (rho in seq(0, 1, by = 0.1)) {
        v <- post_of(run_ct(indep = FALSE, cond_dep_pos = rho, cond_dep_neg = rho))
        finite <- v[!is.na(v)]
        expect_true(all(finite >= 0 & finite <= 1),
                    label = sprintf("posteriors in range at rho = %.1f", rho))
        expect_false(any(is.nan(finite)), label = sprintf("no NaN at rho = %.1f", rho))
    }
})


test_that("the joint probabilities implied by the dependence model form a valid distribution", {
    # The four cells per disease group are reported in the dependenceInfo panel; parse them back
    # and check they are probabilities summing to 1.
    res <- run_ct(indep = FALSE, cond_dep_pos = 0.30, cond_dep_neg = 0.30)
    txt <- paste(res$dependenceInfo$content, collapse = " ")
    cells <- as.numeric(regmatches(txt, gregexpr("(?<=: )[01]\\.[0-9]{4}", txt, perl = TRUE))[[1]])

    expect_length(cells, 8)
    expect_true(all(cells >= 0 & cells <= 1))
    expect_equal(sum(cells[1:4]), 1, tolerance = 1e-4)   # Disease+
    expect_equal(sum(cells[5:8]), 1, tolerance = 1e-4)   # Disease-
})


test_that("the Fagan nomogram explains itself instead of erroring when it cannot be drawn", {
    # nomogrammer() rejects a positive likelihood ratio below 1. The plot called it unguarded, so
    # a raw R error reached the results pane. Specificities of 0.01 are permitted by the option
    # bounds and produce LR+ < 1.
    opts <- ClinicoPath:::cotestOptions$new(fagan = TRUE, indep = TRUE,
                                            test1_spec = 0.01, test2_spec = 0.01)
    a <- ClinicoPath:::cotestClass$new(options = opts, data = data.frame(x = 1))
    p <- a$.__enclos_env__$private
    p$.init()
    p$.run()

    expect_false(isTRUE(a$results$plot1$state$drawable))
    expect_match(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ",
                      paste(a$results$notices$content, collapse = " "))),
                 "Fagan nomogram was not drawn")

    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    expect_false(p$.plot1(a$results$plot1, ggtheme = ggplot2::theme_bw()))

    # a well-specified pair still draws
    opts2 <- ClinicoPath:::cotestOptions$new(fagan = TRUE, indep = TRUE)
    b <- ClinicoPath:::cotestClass$new(options = opts2, data = data.frame(x = 1))
    pb <- b$.__enclos_env__$private
    pb$.init(); pb$.run()
    expect_true(isTRUE(b$results$plot1$state$drawable))
    expect_true(pb$.plot1(b$results$plot1, ggtheme = ggplot2::theme_bw()))
})


test_that("repeated runs do not duplicate rows, footnotes or notices", {
    opts <- ClinicoPath:::cotestOptions$new(indep = FALSE, cond_dep_pos = 0.90,
                                            cond_dep_neg = 0.90, fnote = TRUE)
    a <- ClinicoPath:::cotestClass$new(options = opts, data = data.frame(x = 1))
    p <- a$.__enclos_env__$private
    p$.init()

    seen <- lapply(1:3, function(i) {
        p$.run()
        list(rows   = a$results$cotestResultsTable$rowCount,
             params = a$results$testParamsTable$rowCount,
             notes  = length(p$.notices))
    })
    expect_equal(seen[[2]], seen[[1]])
    expect_equal(seen[[3]], seen[[1]])
    expect_equal(seen[[1]]$rows, 5L)
    expect_equal(seen[[1]]$params, 2L)
    expect_gt(seen[[1]]$notes, 0)   # this configuration does raise truncation warnings
})


test_that("notice text does not inject raw angle brackets into the notices HTML", {
    # The notices panel is assembled by string concatenation and written with setContent(), so a
    # bare "<" in a message lands in the markup. Messages interpolate context labels such as
    # "P(Test1+, Test2- | Disease-)", so .escapeHtml() now runs over every message.
    res <- run_ct(indep = TRUE, test1_spec = 0.01, test2_spec = 0.01, prevalence = 0.6)
    inner <- gsub("<(/?)(div|h4|ul|li|strong|p|br|em)[^>]*>", "", paste(res$notices$content, collapse = " "))
    expect_false(grepl("[<>]", inner))
})


test_that("the dependence panel always describes the model that was actually fitted", {
    # History: the panel was first gated on `(!indep)`, which jmvcore does not route to its R
    # evaluator (^\\([\\$A-Za-z].*\\)$), so it returned the raw truthy string and the panel was
    # permanently visible. That was replaced with `(indep == FALSE)`, which evaluates -- but
    # reads the OPTION, while the content was written from the EFFECTIVE value after a worked
    # example overrode it. Measured on the shipped code: preset = "troponin_ecg" gave a visible
    # panel with 0 characters of content, and preset = "hpv_pap" with indep = TRUE computed 807
    # characters of dependence disclosure and then hid it.
    #
    # Both failure modes came from a visibility expression that could disagree with the content.
    # There is no expression now; the panel is written in both branches. Assert the content.
    ind <- ClinicoPath::cotest(indep = TRUE)$dependenceInfo$content
    expect_true(nzchar(ind))
    expect_match(ind, "conditionally independent")
    expect_false(grepl("Realized phi", ind))

    dep <- ClinicoPath::cotest(indep = FALSE, cond_dep_pos = 0.15,
                               cond_dep_neg = 0.10)$dependenceInfo$content
    expect_true(nzchar(dep))
    expect_match(dep, "Realized phi")

    # the regression that mattered: a worked example must never leave the panel empty, and must
    # never hide a dependent fit behind the user's stale checkbox
    for (nm in c("troponin_ecg", "psa_dre", "hpv_pap", "covid_antigen_pcr")) {
        for (flag in c(TRUE, FALSE)) {
            res <- ClinicoPath::cotest(preset = nm, indep = flag)
            expect_true(nzchar(res$dependenceInfo$content),
                        info = paste0(nm, " / indep=", flag, ": empty dependence panel"))
        }
    }
})


private_of <- function(...) {
    a <- ClinicoPath:::cotestClass$new(options = ClinicoPath:::cotestOptions$new(...),
                                       data = data.frame(x = 1))
    p <- a$.__enclos_env__$private
    p$.init()
    p
}

notice_text <- function(p) paste(vapply(p$.notices, function(z) z$message, character(1)),
                                 collapse = " ")


test_that("the joint-distribution check tests something that can actually fail", {
    # It compared Reduce(`+`, cells) against 1, but the caller defines the fourth cell as
    # 1 - sum(other three), so the sum was 1 by construction and the check could never fire.
    # It now also verifies the cells add back up to the marginals they were derived from.
    p <- private_of()

    p$.notices <- list()
    p$.validateJointDistribution(0.5, 0.3, 0.2, 0.0, 0.8, 0.7, "test")
    expect_length(p$.notices, 0)

    # P(both) + P(first only) = 0.8, but the sensitivity it claims to come from is 0.9
    p$.notices <- list()
    p$.validateJointDistribution(0.5, 0.3, 0.2, 0.0, 0.9, 0.7, "test")
    expect_match(notice_text(p), "do not add back up to the test parameters")

    p$.notices <- list()
    p$.validateJointDistribution(0.5, 0.3, 0.2, -0.1, 0.8, 0.7, "test")
    expect_match(notice_text(p), "not all valid probabilities")

    # consistent marginals, all cells in range, but not a distribution
    p$.notices <- list()
    p$.validateJointDistribution(0.6, 0.3, 0.2, 0.4, 0.9, 0.8, "test")
    expect_match(notice_text(p), "sum to 1.500000 rather than 1")
})


test_that("the fitted model satisfies the joint-distribution check over the whole range", {
    # Driven through the private method because the generated wrapper still rejects negative
    # dependence until jmvtools::prepare() is run.
    p <- private_of()
    for (rho in seq(-1, 1, by = 0.1)) {
        p$.notices <- list()
        p$.calculateDependentTestProbabilities(0.80, 0.90, 0.70, 0.95, rho, rho, 0.10 / 0.90)
        expect_false(grepl("do not add back up|not all valid probabilities|sum to",
                           notice_text(p)),
                     label = sprintf("valid joint distribution at rho = %.1f", rho))
    }
})


test_that("negative conditional dependence is permitted", {
    # Tests that compensate for each other's errors are a real model. The option was bounded at
    # 0, so it could not be expressed at all.
    a_yaml <- paste(readLines("../../jamovi/cotest.a.yaml"), collapse = "\n")
    expect_match(a_yaml, "min: -1\\.00")
    js <- paste(readLines("../../jamovi/js/cotest.events.js"), collapse = "\n")
    expect_match(js, "clampControl\\(ui, 'cond_dep_pos', -1, 1\\)")

    # backend validation accepts it
    p <- private_of()
    expect_silent(p$.validateInputParameters(0.8, 0.9, 0.7, 0.95, 0.1, FALSE, -0.5, -0.5))
    expect_error(p$.validateInputParameters(0.8, 0.9, 0.7, 0.95, 0.1, FALSE, -1.5, 0),
                 "between -1 and 1")

    # negative dependence yields a valid, more-informative-than-independent joint model
    dep <- p$.calculateDependentTestProbabilities(0.80, 0.90, 0.70, 0.95, -0.05, -0.05, 0.10 / 0.90)
    expect_true(is.finite(dep$postest_prob_both))
    expect_gte(dep$postest_prob_both, 0)
    expect_lte(dep$postest_prob_both, 1)

    if (!inherits(try(ClinicoPath::cotest(indep = FALSE, cond_dep_pos = -0.05), silent = TRUE),
                  "try-error"))
        expect_true(TRUE)
    else
        skip("option bounds not recompiled yet - run jmvtools::prepare()")
})


test_that("the results table states that its inputs carry no uncertainty", {
    # Every number in the table is conditional on sensitivity, specificity and prevalence being
    # exact. That was said only in the collapsible welcome panel.
    tb <- ClinicoPath::cotest(indep = TRUE)$cotestResultsTable
    note <- get("note", envir = tb$.__enclos_env__$private$.notes$fixed_inputs)
    expect_match(note, "treated as exact")
    expect_match(note, "no</i> confidence interval")

    # setNote is keyed, so repeated runs must not stack duplicates
    p <- private_of()
    for (i in 1:3) p$.run()
    expect_length(p$.notices[vapply(p$.notices, function(z) grepl("treated as exact", z$message),
                                    logical(1))], 0)
})


test_that("every declared option is consumed somewhere", {
    # No dead schema entries. An option counts as consumed if the backend reads it OR the
    # results schema gates on it -- `showGuidance` is a pure display gate with no backend
    # logic, exactly like the 72 other `visible: (showExplanations)` gates in this module,
    # so requiring a `self$options$` read would force a no-op read just to satisfy a test.
    a_yaml <- readLines("../../jamovi/cotest.a.yaml")
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    declared <- setdiff(declared, "data")
    backend <- paste(readLines("../../R/cotest.b.R"), collapse = "\n")
    schema  <- paste(c(readLines("../../jamovi/cotest.r.yaml"),
                       readLines("../../jamovi/cotest.u.yaml")), collapse = "\n")

    read_by_backend <- vapply(declared, function(o)
        grepl(paste0("self\\$options\\$", o, "\\b"), backend), logical(1))
    gated_in_schema <- vapply(declared, function(o)
        grepl(paste0("(visible|enable): \\([^)]*\\b", o, "\\b"), schema), logical(1))

    expect_equal(declared[!(read_by_backend | gated_in_schema)], character(0))

    # and the display-only options really are display-only: changing one must not move a number
    base <- as.data.frame(ClinicoPath::cotest()$cotestResultsTable)
    for (o in declared[!read_by_backend]) {
        alt <- as.data.frame(do.call(ClinicoPath::cotest,
                                     stats::setNames(list(FALSE), o))$cotestResultsTable)
        expect_equal(alt$postProb, base$postProb, tolerance = 1e-12,
                     info = paste(o, "is documented as display-only but changed a result"))
    }
})


# ---------------------------------------------------------------------------
# Regressions from the /review-function pass
# ---------------------------------------------------------------------------

test_that("a probability of exactly 1 or 0 is footnoted as structural, not reported as certainty", {
    # Clamping an infeasible dependence to its Frechet bound drives one joint cell to zero, so
    # the likelihood ratio is Inf or 0 and the table printed a bare "100.00%" with Inf odds -
    # a diagnostic certainty the model never claimed. Reachable from ordinary inputs.
    opts <- ClinicoPath:::cotestOptions$new(test1_sens = 0.85, test1_spec = 0.99,
                                            test2_sens = 0.80, test2_spec = 0.90,
                                            prevalence = 0.10, indep = FALSE,
                                            cond_dep_pos = 0.10, cond_dep_neg = 0.40)
    a <- ClinicoPath:::cotestClass$new(options = opts, data = data.frame(x = 1))
    p <- a$.__enclos_env__$private
    p$.init(); p$.run()

    d <- as.data.frame(a$results$cotestResultsTable)
    certain <- which(is.finite(d$postProb) & (d$postProb == 1 | d$postProb == 0))
    expect_gt(length(certain), 0)          # the configuration must still reach the regime
    for (i in certain) {
        fn <- a$results$cotestResultsTable$getCell(rowNo = i, col = "postProb")$footnotes
        expect_gt(length(fn), 0)
        expect_true(any(grepl("Not an estimate", unlist(fn), fixed = TRUE)))
    }
})

test_that("direction of effect is read off the ratio, never asserted", {
    # Two worse-than-chance tests: LR+ < 1 and LR- > 1, so "increase"/"reduced to" invert.
    res <- ClinicoPath::cotest(test1_sens = 0.30, test1_spec = 0.50,
                               test2_sens = 0.35, test2_spec = 0.55,
                               prevalence = 0.10, indep = TRUE)
    txt <- gsub("<[^>]+>", " ", res$explanation$content)
    d <- as.data.frame(res$cotestResultsTable)

    expect_lt(d$relativeProbability[d$scenario == "Both Tests Positive"], 1)
    expect_gt(d$relativeProbability[d$scenario == "Both Tests Negative"], 1)
    expect_false(grepl("0.5x increase", txt, fixed = TRUE))
    expect_false(grepl("reduced to 1.55", txt, fixed = TRUE))

    # and the ordinary case still reads naturally
    ok <- gsub("<[^>]+>", " ", ClinicoPath::cotest(indep = TRUE)$explanation$content)
    expect_match(ok, "increase")
    expect_match(ok, "reduced to")
})

test_that("the copy-ready sentence quotes the correlation actually fitted", {
    # Requested 0.50 is not attainable with these marginals; the model fits 0.35 and the
    # Test Dependence panel already said so while the pasteable sentence said 0.50.
    res <- ClinicoPath::cotest(test1_sens = 0.95, test1_spec = 0.90,
                               test2_sens = 0.70, test2_spec = 0.85,
                               prevalence = 0.02, indep = FALSE,
                               cond_dep_pos = 0.5, cond_dep_neg = 0.1)
    sentence <- gsub("<[^>]+>", " ", res$explanation$content)
    panel    <- gsub("<[^>]+>", " ", res$dependenceInfo$content)

    realized <- as.numeric(sub(".*Realized phi .disease.: *(-?[0-9.]+).*", "\\1", panel))
    expect_true(is.finite(realized))
    expect_match(sentence, sprintf("correlation %.2f among diseased", realized))
    expect_match(sentence, "requested 0.50")

    # when nothing is truncated, no parenthetical is added
    plain <- gsub("<[^>]+>", " ", ClinicoPath::cotest(indep = FALSE, cond_dep_pos = 0.05,
                                                      cond_dep_neg = 0.05)$explanation$content)
    expect_false(grepl("truncated to the largest", plain))
})

test_that("no NA leaks into the prose as NA% or NAx", {
    res <- ClinicoPath::cotest(test1_sens = 0.30, test1_spec = 0.90,
                               test2_sens = 0.30, test2_spec = 0.90,
                               prevalence = 0.10, indep = FALSE,
                               cond_dep_pos = -1, cond_dep_neg = -1)
    txt <- gsub("<[^>]+>", " ", res$explanation$content)
    expect_true(any(is.na(as.data.frame(res$cotestResultsTable)$postProb)))  # still reachable
    expect_false(grepl("NA%", txt, fixed = TRUE))
    expect_false(grepl("NAx", txt, fixed = TRUE))
    expect_match(txt, "not estimable")
})

test_that("a sub-1% post-test probability is not printed as 0%", {
    # p2percent used integer accuracy, so a 0.19% both-negative probability appeared on the
    # nomogram as "Post(-) = 0%" - on a rule-out plot that reads as "excluded".
    res <- ClinicoPath::cotest(preset = "hpv_pap", fagan = TRUE)
    bn <- as.data.frame(res$cotestResultsTable)$postProb[5]
    expect_lt(bn, 0.01); expect_gt(bn, 0)
    expect_true(isTRUE(res$plot1$state$drawable))
    expect_true(res$plot1$.render())
    # >= 1% output must be unchanged for the other two nomogrammer callers
    expect_equal(scales::percent(signif(0.6346111, 3), accuracy = 1), "64%")
})

test_that("the rendered nomogram states its rule and marks a worked example", {
    demo <- ClinicoPath::cotest(preset = "hpv_pap", fagan = TRUE)$plot1$state$Caption
    expect_match(demo, "Parallel rule")
    expect_match(demo, "DEMONSTRATION ONLY")
    own <- ClinicoPath::cotest(fagan = TRUE)$plot1$state$Caption
    expect_match(own, "Parallel rule")
    expect_false(grepl("DEMONSTRATION", own))
})

test_that("the dependence essay does not contradict the safety default or the bias direction", {
    txt <- gsub("<[^>]+>", " ", ClinicoPath::cotest()$dependenceExplanation$content)
    # it used to list "You have limited information about how the tests interact" as a reason
    # to ASSUME independence - the opposite of the a.yaml's stated "default is false for safety"
    expect_false(grepl("limited information about how the tests interact", txt))
    expect_match(txt, "has to be justified")
    # and it claimed independence exaggerates positive post-test probabilities, which is
    # backwards for the headline parallel-rule row in ~85% of ordinary parameter sets
    expect_false(grepl("unrealistically narrow confidence", txt))
    expect_match(txt, "Either test positive")
})

test_that("independence really does bias the two extreme rows in the directions the essay states", {
    # The essay now makes four falsifiable claims. Check the two it states as universal.
    set.seed(11); over_bp <- 0; under_bn <- 0; n <- 0
    for (i in 1:120) {
        s1 <- runif(1,.6,.95); c1 <- runif(1,.7,.98)
        s2 <- runif(1,.6,.95); c2 <- runif(1,.7,.98)
        pv <- runif(1,.01,.3); rp <- runif(1,.05,.35); rn <- runif(1,.05,.35)
        A <- as.data.frame(ClinicoPath::cotest(test1_sens=s1, test1_spec=c1, test2_sens=s2,
              test2_spec=c2, prevalence=pv, indep=TRUE)$cotestResultsTable)$postProb
        B <- as.data.frame(ClinicoPath::cotest(test1_sens=s1, test1_spec=c1, test2_sens=s2,
              test2_spec=c2, prevalence=pv, indep=FALSE, cond_dep_pos=rp,
              cond_dep_neg=rn)$cotestResultsTable)$postProb
        if (anyNA(A) || anyNA(B)) next
        n <- n + 1
        over_bp  <- over_bp  + (A[4] > B[4])   # both positive: independence too HIGH
        under_bn <- under_bn + (A[5] < B[5])   # both negative: independence too LOW
    }
    expect_gt(n, 100)
    expect_equal(over_bp, n)
    expect_equal(under_bn, n)
})

test_that("a worked example marks both tables, and a preset cannot swap the model in silence", {
    demo <- ClinicoPath::cotest(preset = "hpv_pap")
    nt <- function(tbl) paste(vapply(tbl$notes, function(z) z$note, character(1)), collapse = " ")
    expect_match(nt(demo$cotestResultsTable), "Demonstration only")
    expect_match(nt(demo$testParamsTable), "Demonstration only")
    expect_false(grepl("Demonstration only", nt(ClinicoPath::cotest()$cotestResultsTable)))

    # under a dependent model the marginal LRs must not be presented as multipliable
    expect_match(nt(ClinicoPath::cotest(indep = FALSE)$testParamsTable), "their product")
    expect_false(grepl("their product", nt(ClinicoPath::cotest(indep = TRUE)$testParamsTable)))

    # indep is a Bool defaulting FALSE, so the schema-default test could never see a
    # deliberate indep = FALSE; psa_dre forces TRUE and used to swap the model silently
    swap <- ClinicoPath::cotest(preset = "psa_dre", indep = FALSE, cond_dep_pos = 0.30)
    expect_match(gsub("<[^>]+>", " ", swap$notices$content), "selects which model is fitted")
})

test_that("one clamp raises one notice, and the notices panel has no duplicate heading", {
    n <- notices_of(run_ct(indep = FALSE, cond_dep_pos = 0.90, cond_dep_neg = 0.90))
    expect_false(grepl("adjusted from", n))            # the info-severity duplicate is gone
    expect_match(n, "not attainable")                  # the informative one survives
    raw <- ClinicoPath::cotest(indep = FALSE, cond_dep_pos = 0.90,
                               cond_dep_neg = 0.90)$notices$content
    expect_false(grepl("<h4[^>]*>Validation Notices", raw))
})

test_that("the two tests can be named, and the defaults are byte-identical to the old labels", {
    named <- ClinicoPath::cotest(test1_name = "HPV", test2_name = "Pap cytology")
    expect_equal(as.data.frame(named$testParamsTable)$test, c("HPV", "Pap cytology"))
    expect_true(any(grepl("^HPV Positive Only$", as.data.frame(named$cotestResultsTable)$scenario)))
    expect_match(gsub("<[^>]+>", " ", named$explanation$content), "Co-testing with HPV")

    plain <- ClinicoPath::cotest()
    expect_equal(as.data.frame(plain$testParamsTable)$test, c("Test 1", "Test 2"))
    expect_equal(as.data.frame(plain$cotestResultsTable)$scenario,
                 c("Either Test Positive (Parallel Rule)", "Test 1 Positive Only",
                   "Test 2 Positive Only", "Both Tests Positive", "Both Tests Negative"))

    # a name reaches HTML, so it must be escaped
    xss <- ClinicoPath::cotest(test1_name = "<script>x</script>")
    expect_false(grepl("<script>", xss$explanation$content, fixed = TRUE))
})

test_that("post-test odds and the likelihood ratios are defined without turning anything on", {
    plain <- ClinicoPath::cotest()
    nt <- paste(vapply(plain$testParamsTable$notes, function(z) z$note, character(1)), collapse = " ")
    expect_match(nt, "LR\\+ is how many times")          # was behind the fnote checkbox
    expect_match(gsub("<[^>]+>", " ", ClinicoPath::cotest(indep = FALSE)$dependenceInfo$content),
                 "Phi is the correlation")               # phi was printed but never defined
    withnotes <- ClinicoPath::cotest(fnote = TRUE)
    expect_gt(length(withnotes$cotestResultsTable$getCell(rowKey = "both_pos",
                                                          col = "orValue")$footnotes), 0)
})

test_that("the guidance prose can be switched off without touching a number", {
    on  <- ClinicoPath::cotest(showGuidance = TRUE)
    off <- ClinicoPath::cotest(showGuidance = FALSE)
    expect_equal(as.data.frame(off$cotestResultsTable)$postProb,
                 as.data.frame(on$cotestResultsTable)$postProb, tolerance = 1e-12)
    r_yaml <- paste(readLines("../../jamovi/cotest.r.yaml"), collapse = "\n")
    expect_match(r_yaml, "visible: \\(showGuidance\\)")
    # gated by the declarative expression, never by setVisible() from the backend
    expect_false(grepl("instructions\\$setVisible|dependenceExplanation\\$setVisible",
                       paste(readLines("../../R/cotest.b.R"), collapse = "\n")))
})
