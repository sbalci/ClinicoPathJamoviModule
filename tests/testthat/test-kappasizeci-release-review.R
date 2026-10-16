# Regression tests from the `kappaSizeCI` release review.
#
# Since the validation fix of 2026-10-15 (development-ideas/kappaSizeCI-validation-2026-10-15.md) the
# analysis REPORTS the closed-form minimum n = ceiling(crit / min slope). kappaSize's own search can
# return 1-2 more (it stops at X^2 >= crit + 0.001) and never fewer than 11, so the sample sizes below
# that differ from kappaSize were re-derived with the independent oracle of
# test-kappaSizeCI-validation.R, and the engine comparisons assert the documented relationship.
# Since the independent review of 2026-10-15 (IR-kappaSizeCI-01), for every design except a binary
# outcome with two raters, the reported n is the larger of that fixed-proportion minimum and the
# restricted-ML one (proportions re-estimated at each kappa). Values below that moved were re-derived with the restricted-ML oracle
# of development-scripts/audit_kappaSizeCI.R, not read back from the module.

ci_run <- function(...) {
    args <- utils::modifyList(
        list(outcome = "2", citype = "two_sided", kappa0 = 0.60, kappaL = 0.40,
             kappaU = 0.80, props = "0.20, 0.80", raters = "2", alpha = 0.05,
             showExplanations = TRUE, showSummary = TRUE),
        list(...))
    do.call(ClinicoPath::kappaSizeCI, args)
}
ci_n <- function(res) {
    l <- strsplit(res$text1$content, "\n")[[1]][1]
    # .fmtN() prints a thousands separator, so "[0-9]+" alone stops at the comma and
    # turns "Required sample size: 1,625" into 1. Match the separators, then strip them.
    as.integer(gsub(",", "", regmatches(l, regexpr("[0-9][0-9,]*", l)), fixed = TRUE))
}


test_that("the sample size is kappaSize's, less at most its stopping margin, two-sided and one-sided", {
    # kappaSize stops at X^2 >= crit + 0.001 and starts at 11; the reported minimum is never above
    # it and at most two below. These three designs land where the two agree exactly.
    skip_if_not_installed("kappaSize")
    expect_equal(ci_n(ci_run()),
                 as.integer(ceiling(kappaSize::CIBinary(kappa0 = 0.60, kappaL = 0.40,
                     kappaU = 0.80, props = c(0.20, 0.80), raters = 2, alpha = 0.05)$n)))
    expect_equal(ci_n(ci_run(outcome = "3", props = "0.20, 0.30, 0.50")),
                 as.integer(ceiling(kappaSize::CI3Cats(kappa0 = 0.60, kappaL = 0.40,
                     kappaU = 0.80, props = c(0.20, 0.30, 0.50), raters = 2, alpha = 0.05)$n)))
    # one-sided uses kappaU = NA as kappaSize's sentinel
    expect_equal(ci_n(ci_run(citype = "one_sided")),
                 as.integer(ceiling(kappaSize::CIBinary(kappa0 = 0.60, kappaL = 0.40,
                     kappaU = NA, props = c(0.20, 0.80), raters = 2, alpha = 0.05)$n)))
    # where they differ, by the margin only: [0.55, 0.65] is 1624 (N* = 1623.82), kappaSize 1625
    eng <- kappaSize::CIBinary(kappa0 = 0.60, kappaL = 0.55, kappaU = 0.65, props = c(0.20, 0.80),
                               raters = 2, alpha = 0.05)$n
    expect_equal(c(ci_n(ci_run(kappaL = 0.55, kappaU = 0.65)), eng), c(1624, 1625))
    # and one-sided genuinely differs from two-sided
    expect_lt(ci_n(ci_run(citype = "one_sided")), ci_n(ci_run()))
})


test_that("kappaU is ignored in one-sided mode", {
    n <- ci_n(ci_run(citype = "one_sided"))
    for (ku in c(0.65, 0.80, 0.99))
        expect_equal(ci_n(ci_run(citype = "one_sided", kappaU = ku)), n, label = paste("kappaU", ku))
})


test_that("an unsizeably narrow interval is refused instead of freezing", {
    # kappaSize searches by brute force -- n <- 10; while (...) n <- n + 1 -- in interpreted R
    # with no cap, and the required n grows as about 1/(distance to the nearer limit)^2. Measured
    # on the binary engine: distances 0.20/0.05/0.01/0.005 give n = 118/1,625/38,203/151,533 in
    # 0.00-1.38 s, while 0.0005 had not finished after 8 s. jamovi cannot abort a running
    # analysis, so the user was simply stuck. setTimeLimit does interrupt the loop.
    t0 <- Sys.time()
    expect_error(ci_run(kappaL = 0.5995, kappaU = 0.6005), "too narrow to size")
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    # .predictedN() answers in closed form, so the refusal no longer waits out a wall-clock
    # budget. This was 20 s (and skip_on_cran) before that guard existed.
    expect_lt(elapsed, 5)

    # the message must tell the user what to change AND what the design would cost
    msg <- tryCatch(ci_run(kappaL = 0.5995, kappaU = 0.6005), error = conditionMessage)
    expect_match(msg, "Widen the interval")
    # the closed-form minimum (kappaSize's own rule would give 15,037,011)
    expect_match(msg, "15,033,097 subjects", fixed = TRUE)
})


# --- /release-review-function pass (2026-08-24) ---------------------------------------------

test_that("the explanation names the limit that GOVERNS n, not the geometrically nearer one", {
    # kappaSize stops when the chi-square clears the critical value at BOTH limits, so the
    # binding limit is the one with the smaller slope -- which disagrees with "closest to
    # kappa0" in about 18% of two-sided designs. Here kappaU is nearer (0.10 vs 0.15) and has
    # literally zero influence: moving it 0.85 -> 0.95 leaves n at 167, while moving kappaL
    # 0.60 -> 0.70 takes n to 1212.
    skip_if_not_installed("kappaSize")
    txt <- ci_run(kappa0 = 0.75, kappaL = 0.60, kappaU = 0.85)$text2$content
    expect_match(txt, "The limit that drives the sample size is 0.6", fixed = TRUE)
    expect_false(grepl("nearer limit", txt, fixed = TRUE))

    expect_equal(ci_n(ci_run(kappa0 = 0.75, kappaL = 0.60, kappaU = 0.85)),
                 ci_n(ci_run(kappa0 = 0.75, kappaL = 0.60, kappaU = 0.95)))   # kappaU inert
    expect_gt(ci_n(ci_run(kappa0 = 0.75, kappaL = 0.70, kappaU = 0.85)),
              ci_n(ci_run(kappa0 = 0.75, kappaL = 0.60, kappaU = 0.85)))      # kappaL binds
})


test_that("proportions are renormalised, so a sum inside the tolerance cannot corrupt n", {
    # kappaSize accepts abs(sum - 1) <= 0.001 and then uses the values verbatim, which drove the
    # lumped goodness-of-fit cell negative and returned 215,974 where the valid design needs
    # 93,636 -- a 2.3-fold error with no warning.
    skip_if_not_installed("kappaSize")
    p <- c(0.9997, 0.0002, 0.0002)
    # For the renormalised vector the fixed-proportion minimum is 93,611 (kappaSize's own search,
    # with its 0.001 stopping margin of ceiling(0.001 x 93611 / 3.841459) = 25 subjects: 93,636).
    # With 3 categories the proportions are also re-estimated (IR-kappaSizeCI-01), which needs
    # 100,389 (restricted-ML oracle) -- past the 100,000 ceiling, so the design is refused. The
    # refusal quotes the VALID figure: the unnormalised vector's corrupted 215,974 never appears.
    msg <- tryCatch(ci_run(outcome = "3", kappa0 = 0.90, kappaL = 0.50, kappaU = 0.95,
                           props = "0.9997, 0.0002, 0.0002"), error = conditionMessage)
    expect_match(msg, "about 100,389 subjects", fixed = TRUE)
    expect_false(grepl("215,974", msg, fixed = TRUE))
    eng <- as.integer(ceiling(kappaSize::CI3Cats(kappa0 = 0.90, kappaL = 0.50,
        kappaU = 0.95, props = p / sum(p), raters = 2, alpha = 0.05)$n))
    expect_equal(eng, 93636L)

    # and renormalising is bit-identical for anything that already sums to one
    expect_equal(ci_n(ci_run()), 118L)
})


test_that("the proportion ORDER cannot decide whether the analysis converges", {
    # kappaSize's binary engine keeps props[1] and evaluates polynomials in it; those cancel
    # differently for p and 1 - p, so 0.9999/0.0001 with 5 raters ran 18 s and was then refused
    # while 0.0001/0.9999 -- the same study -- returned in 1.9 s. Agreement is symmetric under
    # relabelling, so the module hands the engine the smaller proportion first.
    skip_if_not_installed("kappaSize")
    # The original design ([0.40, 0.80]) now needs about 119,848 subjects once the proportions are
    # re-estimated (IR-kappaSizeCI-01) and is refused before the engine runs, which would no longer
    # exercise the engine's ordering. [0.25, 0.90] keeps the same extreme prevalence and 5 raters
    # inside the ceiling: fixed-proportion minimum 29,399 (what kappaSize searches to), reported
    # 30,574 = ceiling(3.841459 / 1.256463e-4), the restricted-ML slope at kappaL agreeing to 1e-6
    # across golden-section, logit-scale golden-section and score-root fits. (An oracle evaluated with
    # 0.9999 first gave 30,578: George-Bowman's alternating sum loses precision near p = 1 and returned
    # a negative cell; the model is symmetric, so the oracle is evaluated at the smaller proportion.)
    t0 <- Sys.time()
    big   <- ci_n(ci_run(props = "0.9999, 0.0001", raters = "5", kappaL = 0.25, kappaU = 0.90))
    small <- ci_n(ci_run(props = "0.0001, 0.9999", raters = "5", kappaL = 0.25, kappaU = 0.90))
    expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 15)
    expect_equal(big, small)
    expect_equal(big, 30574L)
    # and the original design is refused identically in both orders, quoting the same number
    m1 <- tryCatch(ci_run(props = "0.9999, 0.0001", raters = "5"), error = conditionMessage)
    m2 <- tryCatch(ci_run(props = "0.0001, 0.9999", raters = "5"), error = conditionMessage)
    expect_identical(m1, m2)
    expect_match(m1, "about 119,8", fixed = TRUE)
})


test_that("the notices panel never gives opposite rater advice in one run", {
    # "use fewer raters" (sparse) and "increase the number of raters" (large n) used to render
    # in adjacent boxes with no trade-off statement.
    skip_if_not_installed("kappaSize")
    # Needs a design that is sparse on the POOLED cells and above 1,000. No binary design is (its
    # pooled cells hold about rho * p * n subjects), so a rare category of three: n = 1494.
    txt <- ci_run(outcome = "3", props = "0.01, 0.01, 0.98", raters = "4",
                  kappa0 = 0.60, kappaL = 0.50, kappaU = 0.70)$notices$content
    expect_match(txt, "Sparse categories")
    expect_match(txt, "very large and may be")
    expect_match(txt, "using fewer raters", fixed = TRUE)
    expect_false(grepl("increasing the number of raters", txt, fixed = TRUE))
    expect_match(txt, "the two cannot both be improved by the rater count", fixed = TRUE)
})


test_that("a binary outcome is not told to collapse categories it does not have", {
    skip_if_not_installed("kappaSize")
    bin <- ci_run(outcome = "2", props = "0.05, 0.95", raters = "6")$notices$content
    cat5 <- ci_run(outcome = "5", props = "0.02,0.02,0.02,0.02,0.92",
                   raters = "6")$notices$content
    expect_match(bin, "Sparse categories")
    expect_false(grepl("collapsing rare", bin, fixed = TRUE))
    expect_match(cat5, "collapsing rare", fixed = TRUE)
})


test_that("the design that used to hang now sizes, and agrees with the closed form", {
    # This design (6 raters, props 0.999/0.001, kappa0 0.70 on [0.60, 0.80]) previously made
    # kappaSize loop forever: its binary engine keeps props[1] and evaluates polynomials in it,
    # which cancel to exactly zero at extreme p. Handing it the smaller proportion first -- the
    # same study, since agreement is symmetric under relabelling -- converges in under a second,
    # and lands exactly on what .predictedN said all along.
    skip_if_not_installed("kappaSize")
    t0 <- Sys.time()
    n <- ci_n(ci_run(outcome = "2", raters = "6", props = "0.999, 0.001",
                     kappa0 = 0.70, kappaL = 0.60, kappaU = 0.80))
    expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 10)
    # fixed-proportion minimum 23,067; with six raters the reported n also covers re-estimating the
    # proportions, 38,375 (restricted-ML oracle, IR-kappaSizeCI-01)
    expect_equal(n, 38375L)

    # .predictedN is kappaSize's own stopping rule (time budget): 23073, what the engine returns
    an   <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new())
    pred <- an$.__enclos_env__$private$.predictedN
    expect_equal(pred(list(outcome = 2, raters = 6, props = c(0.001, 0.999), kappa0 = 0.70,
                           kappaL = 0.60, kappaU = 0.80, alpha = 0.05)), 23073)

    # five raters sized this all along; both orders must now agree with each other too
    expect_equal(ci_n(ci_run(outcome = "2", raters = "5", props = "0.999, 0.001",
                             kappa0 = 0.70, kappaL = 0.60, kappaU = 0.80)),
                 ci_n(ci_run(outcome = "2", raters = "5", props = "0.001, 0.999",
                             kappa0 = 0.70, kappaL = 0.60, kappaU = 0.80)))
})


test_that("the non-convergence branch is a backstop, and its advice is the opposite of narrow", {
    # After the proportion-order fix no binary design under engine_n_limit could be found that
    # still fails to converge (swept raters 5-6 x p up to 0.99999 x five kappa layouts). The
    # branch is kept because unreachability cannot be proven across all four engines -- so test
    # the MESSAGE directly rather than pretending to trigger it. If it ever fires it must send
    # the user to FEWER raters, never the "more raters" advice that a narrow interval wants.
    an  <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new())
    src <- readLines("../../R/kappaSizeCI.b.R", warn = FALSE)
    # Reconstruct the sentence the user would see. Taking it by regex over the whole file does
    # not work (R's sub() has no lazy quantifier, so ".*?" spans everything). The message used
    # to be split across paste0() literals, so this read a (start - 2):(start + 8) block; it is
    # now a single .() msgid, and that window ran on into the NEXT reject() -- the too-narrow-
    # interval one, whose "use more raters" advice is correct there -- and failed on it. Window
    # the msgid line alone, strip the R string syntax and collapse whitespace.
    start <- grep("extreme for the kappaSize engine", src, fixed = TRUE)
    expect_length(start, 1L)
    prose <- gsub("\\s+", " ", gsub('"', "", src[start]))
    expect_match(prose, "Use fewer raters", fixed = TRUE)
    expect_false(grepl("use more raters", prose, fixed = TRUE))
    expect_true(is.function(an$.__enclos_env__$private$.predictedN))
})


test_that("the engine budget is calibrated on the SLOWEST engine, not the fastest", {
    # Cost per iteration spans 9.7 us (binary, 2 raters) to 99 us (5 categories, 6 raters).
    # A budget calibrated on the binary engine would cut short a legitimate five-category run.
    # This asserts the invariant the two constants have to satisfy, so neither can drift alone:
    # at the ceiling, the budget must cover the worst engine's real cost at least twice.
    src <- paste(readLines("../../R/kappaSizeCI.b.R", warn = FALSE), collapse = "\n")
    ceiling_n <- as.numeric(sub(".*engine_n_limit <- ([0-9]+).*", "\\1", src))
    mult      <- as.numeric(sub(".*predicted \\* ([0-9.e-]+) \\* 2.*", "\\1", src))
    cap       <- as.numeric(sub(".*min\\(([0-9]+), max\\(5, ceiling\\(predicted.*", "\\1", src))
    expect_equal(mult, 1e-4)                       # seconds per iteration, slowest engine
    worst_case_secs <- ceiling_n * mult            # real work at the ceiling
    budget_secs     <- min(cap, ceiling_n * mult * 2)
    expect_gte(budget_secs, worst_case_secs * 2)
})


test_that("a refusal above the integer ceiling still prints a number", {
    # .fmtN used as.integer(), which is capped at 2,147,483,647. The engine could never produce
    # a value that large (the wall-clock guard stopped it first), but .predictedN reports what
    # the design would actually cost, so the message read "about NA subjects" with a coercion
    # warning. Found by fuzzing 600 random option combinations.
    msg <- tryCatch(ci_run(kappa0 = 0.60, kappaL = 0.5999999, kappaU = 0.80),
                    error = conditionMessage)
    expect_match(msg, "too narrow to size")
    expect_false(grepl("about NA subjects", msg, fixed = TRUE))
    expect_match(msg, "[0-9],[0-9]{3}")           # a real, separated figure
    expect_silent(ClinicoPath:::kappaSizeCIClass$private_methods$.fmtN(3.8e13))
})


test_that(".predictedN reproduces the engine exactly, so triage cannot refuse a sizeable design", {
    # kappaSize's chi-square is exactly linear in n -- every term is
    #   (n P_j(k0) - n P_j(rho))^2 / (n P_j(rho)) = n (P_j(k0) - P_j(rho))^2 / P_j(rho)
    # -- so its brute-force search is solving a division. If this ever drifts from the engine,
    # the triage threshold starts refusing designs kappaSize would have sized (or vice versa).
    skip_if_not_installed("kappaSize")
    # .predictedN calls private$.limitSlopes, so it must be bound off an instance rather than
    # lifted out of $private_methods the way the self-contained helpers can be.
    an   <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new())
    pred <- an$.__enclos_env__$private$.predictedN
    engines <- list(`2` = kappaSize::CIBinary, `3` = kappaSize::CI3Cats,
                    `4` = kappaSize::CI4Cats, `5` = kappaSize::CI5Cats)
    props <- list(`2` = c(0.20, 0.80), `3` = c(0.20, 0.60, 0.20),
                  `4` = c(0.20, 0.40, 0.20, 0.20), `5` = c(0.10, 0.30, 0.20, 0.20, 0.20))

    for (oc in 2:5) for (rt in 2:6) for (al in c(0.01, 0.05, 0.10)) for (one in c(FALSE, TRUE)) {
        p  <- props[[as.character(oc)]]
        kU <- if (one) NA else 0.80
        got <- pred(list(outcome = oc, raters = rt, props = p,
                         kappa0 = 0.60, kappaL = 0.40, kappaU = kU, alpha = al))
        want <- engines[[as.character(oc)]](kappa0 = 0.60, kappaL = 0.40, kappaU = kU,
                                            props = p, raters = rt, alpha = al)$n
        expect_equal(got, want,
                     label = sprintf("outcome %d, raters %d, alpha %.2f, one-sided %s",
                                     oc, rt, al, one))
    }
})


test_that("a degenerate slope falls through to the engine rather than returning the n = 11 floor", {
    # A zero expected cell makes the engine's own chi-square infinite, which it maps to 0 and
    # then loops forever. .predictedN() must return NA there (so the setTimeLimit backstop still
    # runs) instead of a slope of Inf, which would silently collapse to the engine's n = 11 floor.
    # .predictedN calls private$.limitSlopes, so it must be bound off an instance rather than
    # lifted out of $private_methods the way the self-contained helpers can be.
    an   <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new())
    pred <- an$.__enclos_env__$private$.predictedN
    expect_true(is.na(pred(list(outcome = 2, raters = 2, props = c(0, 1),
                                kappa0 = 0.60, kappaL = 0.40, kappaU = 0.80, alpha = 0.05))))
    # and kappa0 == kappaL gives a zero slope: no n ever separates them, so this must report
    # Inf (refused instantly) rather than NA (which would send an unanswerable design to the
    # engine to hang for the full 20-second backstop)
    expect_true(is.infinite(pred(list(outcome = 2, raters = 2, props = c(0.2, 0.8),
                                      kappa0 = 0.60, kappaL = 0.60, kappaU = NA, alpha = 0.05))))
})


test_that("the guard does not block intervals that are merely demanding", {
    # Over-blocking would be worse than the freeze: these are legitimate designs.
    # closed-form minima (kappaSize's own search: 1625 / 9707 / 38203)
    expect_equal(ci_n(ci_run(kappaL = 0.55, kappaU = 0.65)), 1624L)
    expect_equal(ci_n(ci_run(kappaL = 0.58, kappaU = 0.62)), 9705L)
    expect_equal(ci_n(ci_run(kappaL = 0.59, kappaU = 0.61)), 38193L)
})


test_that("proportions parse on every separator, and a decimal comma says so", {
    n <- ci_n(ci_run(props = "0.20, 0.80"))
    for (ps in c("0.20 0.80", "0.20;0.80", "0.20|0.80", "0.20  ,  0.80"))
        expect_equal(ci_n(ci_run(props = ps)), n, label = ps)

    # the old class "[,;|\\t]+" was the SET {, ; | \ t}: it matched a literal backslash and the
    # letter t but NOT a tab, and not a space, so a mixed comma+space list was rejected
    expect_equal(ci_n(ci_run(outcome = "3", props = "0.2, 0.3 0.5")),
                 ci_n(ci_run(outcome = "3", props = "0.2, 0.3; 0.5")))
    expect_equal(ci_n(ci_run(props = "0.20\t0.80")), n)

    # "0,20 0,80" split into 0, 20, 0, 80 and reported a range error
    expect_error(ci_run(props = "0,20 0,80"), "decimal point, not a decimal comma")
})


test_that("kappaSize's sparse-cell caveat reaches the notices panel", {
    # It was emitted only inside the Summary pane, where a reader looking for caveats would not
    # find it; the engine is relying on a large-sample approximation in exactly that case.
    res <- ci_run(outcome = "5", props = "0.01, 0.04, 0.15, 0.30, 0.50")
    expect_equal(ci_n(res), 53L)
    expect_match(res$text_summary$content, "less than five")
    expect_match(res$notices$content, "Sparse categories")
    # wording tightened when the rule moved from kappaSize's marginal check to Cochran's rule
    # on the agreement-pattern cells: the approximation being leaned on is the chi-square one
    expect_match(res$notices$content, "large-sample chi-square approximation")

    # and it is not raised when every category is well populated
    expect_false(grepl("Sparse categories", ci_run()$notices$content))
})


test_that("the explanation does not claim the interval width drives the sample size", {
    # kappaSize sizes on the limit that BINDS: with kappaL = 0.55 the answer is 1624 for every kappaU
    # from 0.65 to 0.99, so "Precision width" was the wrong quantity to show.
    skip_if_not_installed("kappaSize")
    for (ku in c(0.65, 0.80, 0.99))
        expect_equal(ci_n(ci_run(kappaL = 0.55, kappaU = ku)), 1624L, label = paste("kappaU", ku))

    txt <- ci_run()$text2$content
    # The explanation now names the limit that GOVERNS n rather than the geometrically nearer
    # one; at the defaults ([0.40, 0.80] around 0.60) they coincide at kappaL = 0.4.
    expect_match(txt, "The limit that drives the sample size is 0.4", fixed = TRUE)
    expect_match(txt, "not the full interval width", fixed = TRUE)
    expect_false(grepl("Precision width", txt, fixed = TRUE))
    expect_false(grepl("nearer limit", txt, fixed = TRUE))
})


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/kappaSizeCI.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    backend <- paste(readLines("../../R/kappaSizeCI.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})


# --- /check-function pass (2026-08-23) ------------------------------------------------------

test_that("sparseness is judged on the test's goodness-of-fit cells, not the outcome marginals", {
    # kappaSize's own check is `props[i] * n < 5`, and for a binary outcome the engine has
    # already reduced props to props[1] -- so it never looks at (1 - p) at all. The chi-square
    # the engine actually runs is over agreement patterns, whose expected counts go sparse far
    # earlier. Measured over 10 realistic designs, the marginal rule missed 7 of them.
    skip_if_not_installed("kappaSize")

    # six raters on the DEFAULT prevalence: the pooled cells of the published 1-df test (all
    # negative / any disagreement / all positive; ADK 2001 p.586) have 1 of 3 below 5, and
    # kappaSize prints nothing. (Before VAL-kappaSizeCI-02 the notice judged the 7 unpooled cells.)
    res <- ci_run(raters = "6")
    expect_false(grepl("less than five", res$text_summary$content))   # engine stays silent
    expect_match(res$notices$content, "Sparse categories")
    expect_match(res$notices$content, "goodness-of-fit cells")
    expect_match(res$notices$content, "the number of cells below 5 is 1 of 3", fixed = TRUE)

    for (design in list(list(props = "0.10, 0.90", raters = "6"),
                        list(outcome = "3", props = "0.20, 0.60, 0.20", raters = "3")))
        expect_match(do.call(ci_run, design)$notices$content, "Sparse categories",
                     info = paste(unlist(design), collapse = "/"))
    # p = .05 with 4 raters was sparse at the fixed-proportion n (211). At the reported n, which
    # now also covers re-estimating the proportions (251, IR-kappaSizeCI-01), its pooled cells hold
    # 218 / 28 / 5.0 and 232 / 9.3 / 10.0 at the two limits: Cochran's rule is met, so it is quiet.
    expect_false(grepl("Sparse categories",
                       ci_run(props = "0.05, 0.95", raters = "4")$notices$content, fixed = TRUE))

    # well-populated designs stay quiet
    expect_false(grepl("Sparse categories", ci_run()$notices$content))
    expect_false(grepl("Sparse categories",
                       ci_run(outcome = "4", props = "0.25, 0.25, 0.25, 0.25")$notices$content))
})


test_that("the large-n warning no longer deletes the sparse-cell warning", {
    # `.buildNotices` built the large-n block with paste0(...) instead of paste0(warn, ...),
    # so whenever both conditions held only the large-n block survived.
    an <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new())
    build <- an$.__enclos_env__$private$.buildNotices
    both <- build(5000, TRUE, sparse_min = 0.013, sparse_below5 = 6L, sparse_total = 7L)
    expect_match(both, "Sparse categories")
    expect_match(both, "very large and may be")
    expect_equal(lengths(regmatches(both, gregexpr("<div", both))), 3L)  # sparse + large-n + info

    only_sparse <- build(500, TRUE, sparse_min = 0.013, sparse_below5 = 6L, sparse_total = 7L)
    expect_match(only_sparse, "Sparse categories")
    expect_false(grepl("very large and may be", only_sparse))

    only_big <- build(5000, FALSE)
    expect_false(grepl("Sparse categories", only_big))
    expect_match(only_big, "very large and may be")
})


test_that("the menu description does not call this a power analysis", {
    # It read "Power Analysis for Interobserver Agreement Analysis.", contradicting the
    # backend's own explanation ("It answers a different question from the power approach").
    a <- paste(readLines("../../jamovi/kappaSizeCI.a.yaml", warn = FALSE), collapse = "\n")
    expect_false(grepl("Power Analysis for Interobserver Agreement Analysis", a, fixed = TRUE))
    expect_match(a, "NOT a power", fixed = TRUE)
    # and the usage block it documents is a call that actually runs
    expect_match(a, "kappaSizeCI(", fixed = TRUE)
    expect_equal(ci_n(ci_run()), 118L)
})


test_that("the sparse notice quotes one coherent confidence limit, not a mix of both", {
    # The first version of this check took the element-wise minimum of the expected counts at
    # kappaL and kappaU. P_j(rho) moves in opposite directions across cells, so that vector was
    # assembled from BOTH limits and "k of m cells are below 5" became a union count that no
    # single chi-square ever has.
    skip_if_not_installed("kappaSize")
    # .gofCells was de-duplicated out of the three kappaSize backends into the top-level
    # kappaSizeGofCells() in R/utils-kappasize.R; the private method no longer exists, so the
    # old $private_methods lookup yielded NULL and calling it died with "non-function".
    gof <- ClinicoPath:::kappaSizeGofCells   # (outcome, raters, props, rho)

    res <- ci_run(kappa0 = 0.60, kappaL = 0.30, kappaU = 0.80, props = "0.05, 0.95", raters = "2")
    n <- ci_n(res)
    expect_equal(n, 181L)

    per <- lapply(c(0.30, 0.80), function(rho) {
        e <- gof(2, 2, c(0.05, 0.95), rho) * n
        list(min = min(e), below5 = sum(e < 5), total = length(e))
    })
    # each limit alone has exactly one cell below 5 -- so must the notice
    expect_equal(vapply(per, function(x) x$below5, integer(1)), c(1L, 1L))
    expect_match(res$notices$content, "the number of cells below 5 is 1 of 3", fixed = TRUE)
    expect_false(grepl("the number of cells below 5 is 2 of 3", res$notices$content, fixed = TRUE))

    # and the count quoted is the smallest across the limits, from that same limit
    expect_match(res$notices$content,
                 paste0("smallest expected count is ",
                        format(signif(min(vapply(per, function(x) x$min, numeric(1))), 2),
                               scientific = FALSE, trim = TRUE)),
                 fixed = TRUE)
})


test_that("a confidence limit with a negative expected cell is skipped, never printed", {
    # props inside kappaSize's own 0.001 sum tolerance can exceed 1 (0.99945 + 0.0003 + 0.0003
    # = 1.00005; both the module validator and CI3Cats accept it), which drives the lumped
    # P0 = 1 - sum(agree) negative. The notice used to read "the smallest expected count is -1".
    skip_if_not_installed("kappaSize")
    res <- ci_run(outcome = "3", kappa0 = 0.60, kappaL = 0.40, kappaU = 0.99,
                  props = "0.99945, 0.0003, 0.0003", raters = "2")
    expect_false(grepl("count is -", res$notices$content, fixed = TRUE))
    expect_false(grepl("-", sub(".*smallest expected count is ([^ ]+).*", "\\1",
                                res$notices$content), fixed = TRUE))
    # the rest of the notice panel still renders
    expect_match(res$notices$content, "very large and may be")
    expect_match(res$notices$content, "Methodology")
})


test_that("expected counts never reach the reader in scientific notation", {
    # signif() pasted into prose rendered the small tail as "8.9e-06" in a sentence aimed at
    # pathologists. The floor was 0.01 and is now 0.0001, matching the two siblings: nothing
    # here rounds to fixed decimals, so the old "0.000 would be worse" justification never
    # applied, and 0.01 also swallowed the default design's own 0.013 neighbourhood. The
    # same three assertions live in test-kappasizepower- and test-kappasizefixedn-.
    skip_if_not_installed("kappaSize")
    # .fmtCount calls .() for its two literal returns, and jmvcore's .() looks up `self` in the
    # calling frame -- lifted out of $private_methods it is unbound and throws "object 'self'
    # not found". Bind it off an instance instead.
    an  <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new())
    fmt <- an$.__enclos_env__$private$.fmtCount
    expect_equal(fmt(8.9e-06), "below 0.0001")
    expect_equal(fmt(0.0013),  "0.0013")
    expect_equal(fmt(0.013),   "0.013")
    expect_equal(fmt(4.9),     "4.9")
    expect_equal(fmt(NA_real_), "unavailable")

    # Rare categories of three: since the binary 3+-rater cells are pooled, binary designs rarely
    # reach the sparse notice at all.
    fired <- 0L; sci <- 0L
    for (pr in c("0.02, 0.98", "0.01, 0.01, 0.98", "0.002, 0.008, 0.99"))
        for (ra in c("2", "4", "6")) {
            r <- ci_run(outcome = if (lengths(strsplit(pr, ",")) == 3) "3" else "2",
                        props = pr, raters = ra, kappaL = 0.45)
            if (grepl("Sparse categories", r$notices$content)) {
                fired <- fired + 1L
                if (grepl("count is [0-9.]+e[-+]", r$notices$content)) sci <- sci + 1L
            }
        }
    expect_gt(fired, 0L)
    expect_equal(sci, 0L)
})


test_that("no literal % survives into the option descriptions or the rendered Rd", {
    # A literal % in an .a.yaml description becomes \% in the .h.R roxygen and \\% in the .Rd,
    # where the parser reads the backslash as a literal and the % as a comment start -- silently
    # eating the rest of the line. "0.05 gives a 95% interval (capped at 0.20). Two-sided
    # intervals use qchisq(1 - alpha, 1);" rendered as "0.05 gives a 95\ 1);", losing 70
    # characters including the cap. tools::checkRd() does not flag it and neither does R CMD check.
    a <- yaml::yaml.load_file("../../jamovi/kappaSizeCI.a.yaml")
    for (o in a$options)
        for (side in c("ui", "R")) {
            txt <- o$description[[side]]
            if (!is.null(txt))
                expect_false(grepl("%", txt, fixed = TRUE),
                             info = paste0(o$name, " description$", side))
        }

    rd_path <- "../../man/kappaSizeCI.Rd"
    skip_if_not(file.exists(rd_path))
    rendered <- paste(utils::capture.output(
        tools::Rd2txt(tools::parse_Rd(rd_path), out = stdout())), collapse = " ")
    # the whole alpha sentence must survive, not just its first half
    expect_match(rendered, "capped at 0.20")
    expect_match(rendered, "one-sided lower bound uses qchisq")
    # a bare backslash is not a valid regex (trailing backslash), so match it literally
    expect_false(grepl("95\\", rendered, fixed = TRUE))
})


test_that("the CI-approach paper this analysis implements is in its refs", {
    # .buildNotices credits "(Rotondi & Donner)" in prose; Rotondi & Donner 2012 is the
    # confidence-interval sample-size paper kappaSize's CI* engines implement.
    r <- yaml::yaml.load_file("../../jamovi/kappaSizeCI.r.yaml")
    expect_true(all(c("rotondiDonnerKappaCI", "donnerEliasziwKappaGOF") %in% r$refs))
    refs <- yaml::yaml.load_file("../../jamovi/00refs.yaml")$refs
    for (k in r$refs) {
        expect_true(!is.null(refs[[k]]), info = k)
        for (fld in c("title", "author", "url"))
            expect_true(nzchar(as.character(refs[[k]][[fld]])), info = paste(k, fld))
    }
})


test_that("the Notes state the answer before the Methodology (check-function-full 2026-10-16)", {
    # `notices` is the first result item, so the n sat below ~410 words of method text.
    skip_if_not_installed("kappaSize")
    res <- ci_run(kappaL = 0.10, kappaU = 0.95)   # n = 19: sparse and small-sample panels fire
    nt <- res$notices$content
    expect_match(nt, "Required sample size: <b>19</b> subjects, each rated by both raters.", fixed = TRUE)
    pos <- vapply(c("Sparse categories.", "Required sample size: <b>", "Methodology."),
                  function(s) regexpr(s, nt, fixed = TRUE)[[1]], numeric(1))
    expect_true(all(pos > 0))
    expect_true(pos[1] < pos[2] && pos[2] < pos[3])
    expect_match(ci_run()$notices$content, "Required sample size: <b>118</b> subjects", fixed = TRUE)
})


test_that("an extreme prevalence gets its own warning (check-function-full 2026-10-16)", {
    # p = 0.02, two raters: n = 855 with healthy cells, and no panel said anything about kappa's
    # dependence on prevalence. 855 x 0.02 = 17.1 subjects per rater in the rare category.
    skip_if_not_installed("kappaSize")
    res <- ci_run(props = "0.02")
    expect_equal(ci_n(res), 855L)
    expect_match(res$notices$content, "Rare category.", fixed = TRUE)
    expect_match(res$notices$content, "expected in 2% of ratings", fixed = TRUE)
    expect_match(res$notices$content, "about 17 of the 855 subjects", fixed = TRUE)
    # the same design written from the common side, and a rare category among five
    expect_match(ci_run(props = "0.98")$notices$content, "Rare category.", fixed = TRUE)
    expect_match(ci_run(outcome = "5", props = "0.01, 0.29, 0.30, 0.20, 0.20",
                        kappaL = 0.5, kappaU = 0.7)$notices$content, "Rare category.", fixed = TRUE)
    # at and above the 5% cut-off it stays quiet
    expect_false(grepl("Rare category.", ci_run()$notices$content, fixed = TRUE))
    expect_false(grepl("Rare category.", ci_run(props = "0.05")$notices$content, fixed = TRUE))
    expect_true("Byrt1993" %in% yaml::yaml.load_file("../../jamovi/kappaSizeCI.r.yaml")$refs)
})


test_that("the Methodology says which interval the n is planned for (check-function-full 2026-10-16)", {
    skip_if_not_installed("kappaSize")
    expect_match(ci_run()$notices$content,
                 "does not establish the requested precision for the Wald, Fisher-z or bootstrap intervals",
                 fixed = TRUE)
})


test_that("percentages and a leading separator are read correctly (check-function-full 2026-10-16)", {
    skip_if_not_installed("kappaSize")
    expect_error(ci_run(props = "20, 80"), "not percentages", fixed = TRUE)
    expect_error(ci_run(props = "20%, 80%"), "not percentages", fixed = TRUE)
    expect_error(ci_run(outcome = "3", props = "20, 60, 20"), "not percentages", fixed = TRUE)
    # a genuinely out-of-range vector keeps the range message
    expect_error(ci_run(props = "1.2, -0.2"), "between 0 and 1", fixed = TRUE)
    # "; 0.2 0.8" left an empty first token that was reported as "must be valid numbers"
    expect_equal(ci_n(ci_run(props = "; 0.20 0.80")), 118L)
    expect_equal(ci_n(ci_run(props = "0.20, 0.80,")), 118L)
})


test_that("no number reaches the prose in scientific notation (check-function-full 2026-10-16)", {
    skip_if_not_installed("kappaSize")
    # the refusal read "The limit that governs the sample size is 1e-04 away from kappa0"
    msg <- tryCatch(ci_run(kappaL = 0.5999), error = conditionMessage)
    expect_match(msg, "is 0.0001 away from kappa0", fixed = TRUE)
    expect_false(grepl("[0-9]e-[0-9]", msg))
    # n above about 38,000 makes the per-subject slope < 1e-04, which the Study Explanation printed
    # as "3.84146 / 8.1...e-05"
    res <- ci_run(kappaL = 0.591)
    expect_gt(ci_n(res), 38415L)
    expect_false(grepl("[0-9]e-[0-9]", res$text2$content))
    expect_match(res$text2$content, "Required n = ⌈", fixed = TRUE)
})


test_that("every kappaSizeCI msgid has a Turkish translation with the same placeholders (2026-10-16)", {
    # The analysis shipped with ~60 msgids that had never been translated, and every rewording
    # since added more: Turkish users read the whole analysis in English.
    po <- testthat::test_path("..", "..", "jamovi", "i18n", "tr.po")
    skip_if_not(file.exists(po))
    lines <- readLines(po, encoding = "UTF-8", warn = FALSE)
    unq <- function(s) gsub('\\\\"', '"', sub('^"', "", sub('"$', "", s)))
    entries <- list(); cur <- NULL; field <- NULL; refs <- character(0)
    flush <- function() if (!is.null(cur)) entries[[length(entries) + 1L]] <<- cur
    for (ln in lines) {
        if (startsWith(ln, "#:")) refs <- c(refs, trimws(sub("^#:", "", ln)))
        else if (startsWith(ln, "msgid ")) {
            flush(); cur <- list(msgid = unq(sub("^msgid ", "", ln)), msgstr = "", refs = refs)
            field <- "msgid"; refs <- character(0)
        } else if (startsWith(ln, "msgstr ")) { cur$msgstr <- unq(sub("^msgstr ", "", ln)); field <- "msgstr" }
        else if (startsWith(ln, "\"") && !is.null(field)) cur[[field]] <- paste0(cur[[field]], unq(ln))
        else if (!nzchar(ln)) field <- NULL
    }
    flush()
    mine <- Filter(function(e) "R/kappaSizeCI.b.R" %in% e$refs && nzchar(e$msgid), entries)
    expect_gt(length(mine), 70L)
    ph <- function(s) sort(regmatches(s, gregexpr("\\{[^{}]*\\}", s))[[1]])
    for (e in mine) {
        expect_true(nzchar(e$msgstr), info = e$msgid)
        expect_identical(ph(e$msgstr), ph(e$msgid), info = e$msgid)
    }
})


test_that("a smallest expected count just under 5 is not printed as 5 (review-function 2026-10-16)", {
    # signif(x, 2) turned 4.95-4.99 into "5", so ten binary designs read "the smallest expected count is 5,
    # and the number of cells below 5 is 1 of 3". Same guard as R/kappaSizePower.b.R .fmtCount (VAL-11).
    res <- ci_run(raters = "3", props = "0.24, 0.76", kappa0 = 0.50, kappaL = 0.30, kappaU = 0.70)
    txt <- res$notices$content
    expect_match(txt, "number of cells below 5 is 1 of 3", fixed = TRUE)
    m <- regmatches(txt, regexpr("smallest expected count is [0-9.]+", txt))
    expect_length(m, 1L)
    shown <- as.numeric(sub("smallest expected count is ", "", m))
    expect_lt(shown, 5)
    expect_gt(shown, 4.9)
})


test_that(".fmtCount never rounds a count below 5 up to 5 (review-function 2026-10-16)", {
    an  <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new())
    fmt <- an$.__enclos_env__$private$.fmtCount
    expect_identical(fmt(4.94), "4.9")
    expect_identical(fmt(4.95), "4.95")
    expect_identical(fmt(4.99), "4.99")
    expect_identical(fmt(4.9996), "4.99")
    expect_identical(fmt(5), "5")
    expect_identical(fmt(0.013), "0.013")
})


test_that("method detail and the kappaSize summary are off by default; the caveats are not (review-function 2026-10-16)", {
    # The Notes ran to ~480 words on every run. The answer, the warnings and the caveats that qualify
    # the number stay; the methodology, Study Explanation and kappaSize's printout wait for a checkbox.
    base <- list(outcome = "2", citype = "two_sided", kappa0 = 0.60, kappaL = 0.40, kappaU = 0.80,
                 props = "0.20, 0.80", raters = "4", alpha = 0.05)
    run <- function(...) do.call(ClinicoPath::kappaSizeCI, utils::modifyList(base, list(...)))
    off <- run()
    expect_false(off$text2$visible)
    expect_false(off$text_summary$visible)
    filled <- function(x) !is.null(x) && nzchar(x)
    expect_false(filled(off$text2$content))
    expect_false(filled(off$text_summary$content))
    notes <- off$notices$content
    expect_match(notes, "Required sample size: <b>62</b>", fixed = TRUE)
    expect_match(notes, "This is a planning expectation, not a guarantee.", fixed = TRUE)
    expect_match(notes, "model-based goodness-of-fit interval", fixed = TRUE)
    expect_match(notes, "Cohen's", fixed = TRUE)
    expect_false(grepl("Methodology.", notes, fixed = TRUE))
    expect_false(grepl("reproduces the kappaSize package", notes, fixed = TRUE))
    words <- length(strsplit(trimws(gsub("<[^>]+>", " ", notes)), "\\s+")[[1]])
    expect_lt(words, 200L)

    det <- run(showExplanations = TRUE)
    expect_true(det$text2$visible)
    expect_match(det$text2$content, "Required n = ", fixed = TRUE)
    expect_match(det$notices$content, "Methodology.", fixed = TRUE)
    expect_false(det$text_summary$visible)
    expect_false(grepl("reproduces the kappaSize package", det$notices$content, fixed = TRUE))

    sm <- run(showSummary = TRUE)
    expect_true(sm$text_summary$visible)
    expect_match(sm$text_summary$content, "kappaSize's own search, above, reports 56", fixed = TRUE)
    expect_match(sm$notices$content, "reproduces the kappaSize package", fixed = TRUE)
    expect_false(grepl("Methodology.", sm$notices$content, fixed = TRUE))

    # the checkboxes change what is shown, never the number
    both <- run(showExplanations = TRUE, showSummary = TRUE)
    for (r in list(off, det, sm, both))
        expect_identical(strsplit(r$text1$content, "\n")[[1]][1], "Required sample size: 62")
})


test_that("the Methodology does not claim re-estimation when the restricted fit failed (review-function 2026-10-16)", {
    an    <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new())
    build <- an$.__enclos_env__$private$.buildNotices
    failed <- build(190, alpha = 0.05, outcome = 3, raters = 3, rml_checked = FALSE,
                    props = c(0.11, 0.70, 0.19), show_details = TRUE)
    expect_match(failed, "could not be re-estimated", fixed = TRUE)
    expect_false(grepl("or re-estimates them by maximum likelihood", failed, fixed = TRUE))
    ok <- build(190, alpha = 0.05, outcome = 3, raters = 3, rml_checked = TRUE,
                props = c(0.11, 0.70, 0.19), show_details = TRUE)
    expect_match(ok, "or re-estimates them by maximum likelihood", fixed = TRUE)
    expect_false(grepl("could not be re-estimated", ok, fixed = TRUE))
})


test_that("every option named in a validation message is named on screen; kappaL help is not 'nearer limit' (review-function 2026-10-16)", {
    ui <- testthat::test_path("..", "..", "jamovi", "kappaSizeCI.u.yaml")
    ay <- testthat::test_path("..", "..", "jamovi", "kappaSizeCI.a.yaml")
    skip_if_not(file.exists(ui) && file.exists(ay))
    labels <- unlist(lapply(yaml::read_yaml(ui)$children, function(x) {
        walk <- function(n) c(n$label, unlist(lapply(n$children, walk)))
        walk(x)
    }))
    for (id in c("kappa0", "kappaL", "kappaU"))
        expect_true(any(grepl(paste0("(", id, ")"), labels, fixed = TRUE)), info = id)
    # kappa0 .75 on [.60, .85] needs 167 whatever the nearer kappaU is: the help may not say otherwise
    opt <- Filter(function(o) identical(o$name, "kappaL"), yaml::read_yaml(ay)$options)[[1]]
    for (d in c(opt$description$ui, opt$description$R)) expect_match(d, "not always", fixed = TRUE)
    expect_identical(ci_n(ci_run(kappa0 = 0.75, kappaL = 0.60, kappaU = 0.85)),
                     ci_n(ci_run(kappa0 = 0.75, kappaL = 0.60, kappaU = 0.95)))
})


test_that("code strings are ASCII (R CMD check portability); the rendered caveats are unchanged (release review 2026-10-16)", {
    # 77e3905 typed kappa, curly quotes and an em dash straight into three .() msgids. R CMD check
    # WARNs on non-ASCII outside comments; \uXXXX gives the identical string, so the catalog still
    # matches. Parse tokens, not lines: comments may carry UTF-8.
    path <- testthat::test_path("..", "..", "R", "kappaSizeCI.b.R")
    skip_if_not(file.exists(path), "needs the source tree")
    pd   <- utils::getParseData(parse(path, keep.source = TRUE, encoding = "UTF-8"))
    toks <- pd$text[pd$terminal & pd$token != "COMMENT"]
    expect_identical(toks[vapply(toks, function(s) any(utf8ToInt(s) > 127L), logical(1))],
                     character(0))
    notes <- ci_run(showSummary = TRUE)$notices$content
    for (s in c("Cohen's or Fleiss' κ.", "this κ equals Cohen's κ",
                "will “ensure” the limits — read it"))
        expect_match(notes, s, fixed = TRUE)
})


test_that("the syntax pane escapes props: a crafted value cannot inject an argument (security audit 2026-10-16)", {
    # jmvcore quotes a String without escaping it: this value exported a call with an extra
    # `alpha = {...}` argument that ran its code when pasted into R. Parse only, never evaluate.
    src_of <- function(props) ClinicoPath:::kappaSizeCIClass$new(
        options = ClinicoPath:::kappaSizeCIOptions$new(props = props))$asSource()
    for (bad in c('0.2", alpha = {stop("INJECTED"); 0.05}, citype = "two_sided',
                  '0.2\\", 0.8\\', '0.2"\nstop("x")\n"')) {
        ex <- parse(text = src_of(bad))[[1]]
        expect_identical(ex$props, bad)
        expect_identical(names(ex)[-1], "props")
    }
    expect_match(src_of("0.30, 0.70"), 'props = "0.30, 0.70"', fixed = TRUE)
    expect_false(grepl("props", src_of("0.20, 0.80"), fixed = TRUE))   # default still omitted
})


test_that("decimal-comma advice only when the comma reading parses in full (security audit 2026-10-16)", {
    # "1.2," became NA, was dropped, and 0.8 alone made the typo look like a decimal comma.
    for (p in c("1.2, 0.8", "2, 0.8", "0x1A, 0.8"))
        expect_error(ci_run(props = p), "between 0 and 1", fixed = TRUE, info = p)
    for (p in c("0,20 0,80", "0,2, 0,8", "; 0,20 0,80"))
        expect_error(ci_run(props = p), "decimal point, not a decimal comma", fixed = TRUE, info = p)
})


test_that("two raters read 'both raters'; three or more keep 'all N raters' (2026-10-16)", {
    expect_match(ci_run()$notices$content, "each rated by both raters.", fixed = TRUE)
    expect_match(ci_run(raters = "3")$notices$content, "each rated by all 3 raters.", fixed = TRUE)
})


test_that("with the Summary pane off the kappaSize engine does not run, and text1 is unchanged (2026-10-16)", {
    # The reported n is closed-form (.minimumN), yet kappaSize's brute-force search ran on every run and was
    # discarded unless showSummary was on: 10 s for n = 90,407 (5 categories, 6 raters, [0.5975, 0.6025]).
    # An engine that fails proves it is not called: with the pane off nothing may reach it.
    two_sided <- ci_run(showSummary = TRUE)$text1$content
    one_sided <- ci_run(citype = "one_sided", showSummary = TRUE)$text1$content
    local_mocked_bindings(CIBinary = function(...) stop("engine ran"), .package = "kappaSize")
    expect_identical(ci_run(showSummary = FALSE)$text1$content, two_sided)
    expect_identical(ci_run(citype = "one_sided", showSummary = FALSE)$text1$content, one_sided)
    expect_error(ci_run(showSummary = TRUE), "engine ran")          # the pane still runs it
    # the ceiling refusal does not depend on the pane
    expect_error(ci_run(kappaL = 0.5995, kappaU = 0.6005, showSummary = FALSE), "too narrow to size")
})


test_that("props from the R wrapper as NULL, NA or a vector gets a real message or is read (2026-10-16)", {
    # These reached the user as "argument is of length zero" / "the condition has length > 1".
    expect_error(ClinicoPath::kappaSizeCI(props = NULL), "Proportions cannot be empty", fixed = TRUE)
    expect_error(ClinicoPath::kappaSizeCI(props = NA_character_), "Proportions cannot be empty", fixed = TRUE)
    expect_equal(ci_n(ClinicoPath::kappaSizeCI(props = c(0.2, 0.8))), 118L)
    expect_equal(ci_n(ClinicoPath::kappaSizeCI(props = c("0.2", "0.8"))), 118L)
})


test_that("a narrow no-break space (U+202F) separates proportions, as in kappaSizeFixedN (2026-10-16)", {
    # French typography puts U+202F before ";"; it is not in [:space:], so the token "0.20<U+202F>" was
    # "not a valid number".
    expect_equal(ci_n(ci_run(props = "0.20\u202f; 0.80")), 118L)
    expect_equal(ci_n(ci_run(props = "0.20\u202f0.80")), 118L)
})
