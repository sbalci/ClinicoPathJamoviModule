# Regression tests from the `kappaSizeCI` release review.
#
# Sample sizes are checked against the kappaSize package directly. The module reproduced kappaSize
# exactly across 2,560 swept comparisons including the one-sided path, so what needed fixing was
# everything around the engine call.

ci_run <- function(...) {
    args <- utils::modifyList(
        list(outcome = "2", citype = "two_sided", kappa0 = 0.60, kappaL = 0.40,
             kappaU = 0.80, props = "0.20, 0.80", raters = "2", alpha = 0.05),
        list(...))
    do.call(ClinicoPath::kappaSizeCI, args)
}
ci_n <- function(res) {
    l <- strsplit(res$text1$content, "\n")[[1]][1]
    # .fmtN() prints a thousands separator, so "[0-9]+" alone stops at the comma and
    # turns "Required sample size: 1,625" into 1. Match the separators, then strip them.
    as.integer(gsub(",", "", regmatches(l, regexpr("[0-9][0-9,]*", l)), fixed = TRUE))
}


test_that("the sample size matches kappaSize exactly, two-sided and one-sided", {
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
    expect_match(msg, "15,037,011 subjects", fixed = TRUE)
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
    n <- ci_n(ci_run(outcome = "3", kappa0 = 0.90, kappaL = 0.50, kappaU = 0.95,
                     props = "0.9997, 0.0002, 0.0002"))
    p <- c(0.9997, 0.0002, 0.0002)
    expect_equal(n, as.integer(ceiling(kappaSize::CI3Cats(kappa0 = 0.90, kappaL = 0.50,
        kappaU = 0.95, props = p / sum(p), raters = 2, alpha = 0.05)$n)))
    expect_lt(n, 215974L)

    # and renormalising is bit-identical for anything that already sums to one
    expect_equal(ci_n(ci_run()), 118L)
})


test_that("the proportion ORDER cannot decide whether the analysis converges", {
    # kappaSize's binary engine keeps props[1] and evaluates polynomials in it; those cancel
    # differently for p and 1 - p, so 0.9999/0.0001 with 5 raters ran 18 s and was then refused
    # while 0.0001/0.9999 -- the same study -- returned in 1.9 s. Agreement is symmetric under
    # relabelling, so the module hands the engine the smaller proportion first.
    skip_if_not_installed("kappaSize")
    t0 <- Sys.time()
    big   <- ci_n(ci_run(props = "0.9999, 0.0001", raters = "5"))
    small <- ci_n(ci_run(props = "0.0001, 0.9999", raters = "5"))
    expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 15)
    expect_equal(big, small)
    expect_equal(big, 88673L)
})


test_that("the notices panel never gives opposite rater advice in one run", {
    # "use fewer raters" (sparse) and "increase the number of raters" (large n) used to render
    # in adjacent boxes with no trade-off statement.
    skip_if_not_installed("kappaSize")
    txt <- ci_run(props = "0.05, 0.95", raters = "4",
                  kappa0 = 0.60, kappaL = 0.55, kappaU = 0.65)$notices$content
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
    expect_equal(n, 23073L)

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
    # not work (R's sub() has no lazy quantifier, so ".*?" spans everything), and taking it by
    # line does not either -- the message is split across paste0() string literals, so
    # "Use fewer raters" never appears contiguously in the source. Strip the R string syntax
    # from the block and collapse whitespace, then assert on the rendered prose.
    start <- grep("extreme for the kappaSize engine", src, fixed = TRUE)
    expect_length(start, 1L)
    block <- paste(src[(start - 2):(start + 8)], collapse = " ")
    prose <- gsub("\\s+", " ", gsub('"', "", gsub('",\\s*"', "", block)))
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
    # .predictedN calls private$.gofCells, so it must be bound off an instance rather than
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
    # .predictedN calls private$.gofCells, so it must be bound off an instance rather than
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
    expect_equal(ci_n(ci_run(kappaL = 0.55, kappaU = 0.65)), 1625L)
    expect_equal(ci_n(ci_run(kappaL = 0.58, kappaU = 0.62)), 9707L)
    expect_equal(ci_n(ci_run(kappaL = 0.59, kappaU = 0.61)), 38203L)
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
    # kappaSize sizes on whichever limit is NEARER kappa0: with kappaL = 0.55 the answer is 1625
    # for every kappaU from 0.65 to 0.99, so "Precision width" was the wrong quantity to show.
    skip_if_not_installed("kappaSize")
    for (ku in c(0.65, 0.80, 0.99))
        expect_equal(ci_n(ci_run(kappaL = 0.55, kappaU = ku)), 1625L, label = paste("kappaU", ku))

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

test_that("sparseness is judged on the agreement-pattern cells, not the outcome marginals", {
    # kappaSize's own check is `props[i] * n < 5`, and for a binary outcome the engine has
    # already reduced props to props[1] -- so it never looks at (1 - p) at all. The chi-square
    # the engine actually runs is over agreement patterns, whose expected counts go sparse far
    # earlier. Measured over 10 realistic designs, the marginal rule missed 7 of them.
    skip_if_not_installed("kappaSize")

    # six raters on the DEFAULT prevalence: 6 of 7 pattern cells below 5, minimum 0.013,
    # and kappaSize prints nothing
    res <- ci_run(raters = "6")
    expect_false(grepl("less than five", res$text_summary$content))   # engine stays silent
    expect_match(res$notices$content, "Sparse categories")
    expect_match(res$notices$content, "agreement-pattern")
    expect_match(res$notices$content, "of 7 cells are below 5")

    for (design in list(list(props = "0.05, 0.95", raters = "4"),
                        list(props = "0.10, 0.90", raters = "6"),
                        list(outcome = "3", props = "0.20, 0.60, 0.20", raters = "3")))
        expect_match(do.call(ci_run, design)$notices$content, "Sparse categories",
                     info = paste(unlist(design), collapse = "/"))

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
    gof <- ClinicoPath:::kappaSizeCIClass$private_methods$.gofCells

    res <- ci_run(kappa0 = 0.60, kappaL = 0.30, kappaU = 0.80, props = "0.05, 0.95", raters = "2")
    n <- ci_n(res)
    expect_equal(n, 181L)

    per <- lapply(c(0.30, 0.80), function(rho) {
        e <- gof(2, 2, c(0.05, 0.95), rho) * n
        list(min = min(e), below5 = sum(e < 5), total = length(e))
    })
    # each limit alone has exactly one cell below 5 -- so must the notice
    expect_equal(vapply(per, function(x) x$below5, integer(1)), c(1L, 1L))
    expect_match(res$notices$content, "1 of 3 cells are below 5", fixed = TRUE)
    expect_false(grepl("2 of 3 cells are below 5", res$notices$content, fixed = TRUE))

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
    # pathologists; rounding to fixed decimals instead would print "0.000", so it says
    # "below 0.01".
    skip_if_not_installed("kappaSize")
    fmt <- ClinicoPath:::kappaSizeCIClass$private_methods$.fmtCount
    expect_equal(fmt(8.9e-06), "below 0.01")
    expect_equal(fmt(0.0013),  "below 0.01")
    expect_equal(fmt(0.013),   "0.013")
    expect_equal(fmt(4.9),     "4.9")
    expect_equal(fmt(NA_real_), "unavailable")

    fired <- 0L; sci <- 0L
    for (pr in c("0.02, 0.98", "0.05, 0.95", "0.20, 0.80"))
        for (ra in c("2", "4", "6")) {
            r <- ci_run(props = pr, raters = ra, kappaL = 0.45)
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
