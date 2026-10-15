# Regression tests from the `kappaSizePower` release review.
#
# The sample sizes are checked against the kappaSize package directly, not against the module's
# own arithmetic. Since 2026-10-09 the module recomputes N with kappaSize's formula from stable
# cells and an exact lambda (kappaSize cancels to N = 0 at p >= 0.999 with 5-6 raters, and its
# bisection can leave N a subject off), so parity below holds only where kappaSize is
# numerically sound; test-kappaSizePower-validation.R checks N against an independent oracle.

first_line <- function(res) trimws(strsplit(res$text1$content, "\n")[[1]][1])
# text2 is wrapped at ~78 columns at render time; compare it with line breaks collapsed
flat <- function(x) gsub("\\s+", " ", x)
n_from <- function(res) {
    m <- regmatches(first_line(res), regexpr("[0-9]+", first_line(res)))
    as.integer(m)
}

run_kp <- function(...) {
    args <- utils::modifyList(
        list(outcome = "2", kappa0 = 0.4, kappa1 = 0.6, props = "0.30, 0.70",
             raters = "2", alpha = 0.05, power = 0.80),
        list(...))
    do.call(ClinicoPath::kappaSizePower, args)
}


test_that("alpha at or above power is refused instead of hanging", {
    # kappaSize's root finder (kappaSize:::.hichi) never converges when alpha >= power: a direct
    # PowerBinary(alpha = 0.90, power = 0.20) call was still running after 60 seconds and could
    # not be interrupted, while alpha = 0.05 / power = 0.80 returns instantly. The option bounds
    # allow alpha up to 0.99 and power down to 0.01, so a jamovi user could freeze the analysis
    # with no way to recover.
    # Defence in depth. The alpha option is now bounded 0.001-0.20 in the .a.yaml, so the
    # original hang case (alpha 0.90) is rejected by the generated wrapper before the backend is
    # even reached -- assert that too, because it is what makes the worst case unreachable.
    expect_error(run_kp(alpha = 0.90, power = 0.20), "alpha must be between")

    # These are inside the permitted bounds and still violate alpha < power, so they are the
    # cases the backend guard actually has to catch.
    expect_error(run_kp(alpha = 0.15, power = 0.10), "must be below the power")
    expect_error(run_kp(alpha = 0.20, power = 0.20), "must be below the power")
    expect_error(run_kp(alpha = 0.05, power = 0.01), "must be below the power")

    # and it is refused fast -- if this ever regresses the test suite hangs rather than fails,
    # so assert on elapsed time too
    t0 <- Sys.time()
    try(run_kp(alpha = 0.15, power = 0.10), silent = TRUE)
    expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 20)
})


test_that("the sample size matches kappaSize exactly", {
    skip_if_not_installed("kappaSize")
    cases <- list(
        list(outcome = "2", props = "0.30, 0.70", pv = c(0.30, 0.70), raters = "2", fn = kappaSize::PowerBinary),
        list(outcome = "2", props = "0.30, 0.70", pv = c(0.30, 0.70), raters = "4", fn = kappaSize::PowerBinary),
        list(outcome = "3", props = "0.20, 0.30, 0.50", pv = c(0.20, 0.30, 0.50), raters = "2", fn = kappaSize::Power3Cats),
        list(outcome = "4", props = "0.20, 0.20, 0.30, 0.30", pv = c(0.20, 0.20, 0.30, 0.30), raters = "2", fn = kappaSize::Power4Cats),
        list(outcome = "5", props = "0.10, 0.20, 0.20, 0.20, 0.30", pv = c(0.10, 0.20, 0.20, 0.20, 0.30), raters = "3", fn = kappaSize::Power5Cats)
    )
    for (cs in cases) {
        got <- n_from(run_kp(outcome = cs$outcome, props = cs$props, raters = cs$raters))
        ref <- ceiling(cs$fn(kappa0 = 0.4, kappa1 = 0.6, props = cs$pv,
                            raters = as.integer(cs$raters), alpha = 0.05, power = 0.80)$N)
        expect_equal(got, as.integer(ref), label = paste("outcome", cs$outcome, "raters", cs$raters))
    }
    # the headline default case, spelled out
    expect_equal(n_from(run_kp(props = "0.20, 0.80")), 241L)
})


test_that("proportions parse the same way as in the sibling analyses", {
    # The parser split on commas only, so "0.30 0.70" -- which kappaSizeFixedN accepts -- was
    # rejected here. Three analyses in one menu taking the same field in three formats is a trap.
    n <- n_from(run_kp(props = "0.30, 0.70"))
    for (ps in c("0.30 0.70", "0.30;0.70", "0.30 , 0.70", "0.30,0.70"))
        expect_equal(n_from(run_kp(props = ps)), n, label = ps)

    # a decimal comma is still an error, but the message now says why
    expect_error(run_kp(props = "0,30 0,70"), "decimal comma")
})


test_that("a binary prevalence may be given as one value or two", {
    # kappaSize::PowerBinary discards the second proportion after checking it sums to 1
    # (props <- props[1]), so both entry styles are the same computation.
    expect_equal(n_from(run_kp(props = "0.30")), n_from(run_kp(props = "0.30, 0.70")))
    # and it is symmetric in p vs 1 - p
    expect_equal(n_from(run_kp(props = "0.30, 0.70")), n_from(run_kp(props = "0.70, 0.30")))
})


test_that("an alternative kappa below the null is flagged rather than answered silently", {
    # It is accepted by the engine and returns a number, but it sizes a study to show agreement
    # is WORSE than the null -- and the n differs from the mirrored alternative.
    down <- run_kp(kappa0 = 0.6, kappa1 = 0.4)
    up   <- run_kp(kappa0 = 0.4, kappa1 = 0.6)
    # the warning lives in the Notes panel at the top of the output, not the last pane
    expect_match(down$notices$content, "BELOW the null")
    expect_match(down$notices$content, "swap the two values")
    expect_false(grepl("BELOW the null", up$notices$content))
    expect_false(grepl("BELOW the null", up$text2$content))
    # the two are genuinely different questions, so the sizes differ
    expect_false(identical(n_from(down), n_from(up)))
})


test_that("the study explanation reports the answer and reads correctly", {
    # It described the inputs but never the computed sample size, and the multi-category
    # sentence read "the prevalence of the categories are".
    two <- run_kp(props = "0.30, 0.70")$text2$content
    expect_match(flat(two), "The required sample size is [0-9]+ subjects")
    # With two proportions the user never said which category is the trait, and "0.80, 0.20"
    # used to read "the prevalence of the trait is 0.8"; the sentence now names both.
    expect_match(flat(two), "two categories occur in 0.3 and 0.7", fixed = TRUE)
    expect_match(flat(run_kp(props = "0.30")$text2$content), "prevalence of the trait is 0.3",
                 fixed = TRUE)

    three <- run_kp(outcome = "3", props = "0.20, 0.30, 0.50")$text2$content
    expect_match(flat(three), "prevalences of the categories are 0.2, 0.3 and 0.5", fixed = TRUE)
    expect_false(grepl("prevalence of the categories are", flat(three), fixed = TRUE))
    # the number quoted in the explanation must be the number in the result
    expect_match(flat(three), paste0("is ", n_from(run_kp(outcome = "3", props = "0.20, 0.30, 0.50")),
                               " subjects"))
})


test_that("the study explanation wraps so it fits the results panel", {
    # Preformatted panes do not wrap; the first paragraph was a single 300-character line
    # that ran off the right edge of the jamovi results panel.
    lines <- strsplit(run_kp()$text2$content, "\n")[[1]]
    expect_lte(max(nchar(lines)), 80L)
    expect_gt(length(lines), 6L)
})


test_that("text1 shows the headline sentence rather than an object dump", {
    res <- run_kp(outcome = "5", props = "0.10, 0.20, 0.20, 0.20, 0.30")
    expect_match(first_line(res), "^A minimum of [0-9]+ subjects")
    # print() and summary() overlap, but text1 must not be a raw list/structure dump
    expect_false(grepl("^\\$|List of|attr\\(", res$text1$content))
})


test_that("invalid proportions are rejected with readable messages", {
    expect_error(run_kp(props = "0.3, 0.3"), "sum to 1")
    expect_error(run_kp(props = "0.3, -0.1"), "between 0 and 1")
    expect_error(run_kp(props = "0.3, 1.2"), "between 0 and 1")
    expect_error(run_kp(outcome = "3", props = "0.3, 0.7"), "exactly 3 proportions")
    expect_error(run_kp(props = "abc"), "could not be read as a number")
    expect_error(run_kp(kappa0 = 0.5, kappa1 = 0.5), "must differ")
})


test_that("the kappaSize family agrees on option meaning and bounds", {
    # Three analyses in one menu described the same argument three ways, and kappaSize's own
    # docs say two of them were backwards: PowerBinary's kappa0 is "the null hypothesis for the
    # kappa hypothesis test", while CIBinary's and FixedNBinary's is "the preliminary value of
    # kappa". kappaSizePower called its null "Expected value" and kappaSizeCI called its
    # anticipated value "the null hypothesis value" -- each the other's meaning.
    yml <- function(f) paste(readLines(file.path("../../jamovi", f), warn = FALSE), collapse = "\n")
    blk <- function(f, opt) {
        s <- yml(f); i <- regexpr(sprintf("    - name: %s\n", opt), s)
        substr(s, i, i + attr(i, "match.length") + 700)
    }
    expect_match(blk("kappaSizePower.a.yaml", "kappa0"), "null hypothesis value of kappa")
    expect_match(blk("kappaSizeCI.a.yaml", "kappa0"), "preliminary \\(anticipated\\) value")
    expect_match(blk("kappaSizeFixedN.a.yaml", "kappa0"), "preliminary \\(anticipated\\) value")
    # power must NOT describe its null as the expected/anticipated value
    expect_false(grepl("Expected value of kappa", blk("kappaSizePower.a.yaml", "kappa0")))

    # alpha bounds identical across the three
    for (f in c("kappaSizePower.a.yaml", "kappaSizeCI.a.yaml", "kappaSizeFixedN.a.yaml")) {
        b <- blk(f, "alpha")
        expect_match(b, "min: 0.001", fixed = TRUE, info = f)
        expect_match(b, "max: 0.20", fixed = TRUE, info = f)
    }
})


test_that("each analysis states which question it is sizing for", {
    # Power and CI give materially different sample sizes for the same study because they
    # answer different questions, and neither said so.
    p <- run_kp()
    expect_match(flat(p$text2$content), "POWER calculation")
    expect_match(flat(p$text2$content), "kappaSizeCI")

    ci <- ClinicoPath::kappaSizeCI(outcome = "2", kappa0 = 0.6, kappaL = 0.4, kappaU = 0.8,
                                   props = "0.30, 0.70", raters = "2", alpha = 0.05)
    expect_match(ci$text2$content, "CONFIDENCE-INTERVAL calculation")
    expect_match(ci$text2$content, "kappaSizePower")
    # and it no longer calls its anticipated kappa a null hypothesis value
    expect_match(ci$text2$content, "Anticipated kappa")
    expect_false(grepl("Null hypothesis kappa", ci$text2$content, fixed = TRUE))
})


test_that("kappaSizeFixedN accepts a binary prevalence the same way its siblings do", {
    # It demanded exactly two proportions, so "0.30" -- fine in kappaSizePower and kappaSizeCI,
    # and fine for kappaSize::FixedNBinary itself -- was rejected.
    one <- ClinicoPath::kappaSizeFixedN(outcome = "2", kappa0 = 0.6, props = "0.30",
                                        raters = "2", alpha = 0.05, n = 100)
    two <- ClinicoPath::kappaSizeFixedN(outcome = "2", kappa0 = 0.6, props = "0.30, 0.70",
                                        raters = "2", alpha = 0.05, n = 100)
    expect_equal(one$text1$content, two$text1$content)
    # two values that do not sum to 1 are still caught
    expect_error(ClinicoPath::kappaSizeFixedN(outcome = "2", kappa0 = 0.6, props = "0.3, 0.3",
                                              raters = "2", alpha = 0.05, n = 100), "sum to 1")
})


test_that("sparse expected cells are lifted into the Notes panel and not repeated", {
    # kappaSize prints its cell-count warning once per sparse category; with five levels at
    # n = 10 that was five identical lines in text1 and five more in the summary, and nothing
    # in a place a user would look first.
    res <- run_kp(outcome = "5", kappa0 = 0.2, kappa1 = 0.8, props = "0.02, 0.08, 0.3, 0.3, 0.3")
    expect_match(res$notices$content, "Sparse categories")
    expect_equal(sum(grepl("expected cell count", strsplit(res$text1$content, "\n")[[1]])), 1L)
    expect_equal(sum(grepl("expected cell count",
                           strsplit(res$text_summary$content, "\n")[[1]])), 1L)
    # the binary engine checks only p, never 1 - p: the rare category must still be caught
    rare <- run_kp(kappa0 = 0.2, kappa1 = 0.8, props = "0.95, 0.05")
    expect_match(rare$notices$content, "Sparse categories")
    expect_false(grepl("expected cell count", rare$text1$content))
    # and a comfortable design carries only the methodology block
    ok <- run_kp()
    expect_false(grepl("Sparse categories", ok$notices$content))
    expect_match(ok$notices$content, "Methodology")
    expect_match(ok$notices$content, paste0("<b>", n_from(ok), "</b> subjects"))
})


test_that("sparse goodness-of-fit cells are judged on the cells of the published test", {
    # The category marginals (kappaSize's own warning) miss sparse goodness-of-fit cells, so the
    # Notes judge the chi-square cells. With a binary outcome and 3+ raters those are the cells of
    # the 1-df test the N is sized for, which pools every split call into one disagreement cell
    # (Altaye, Donner & Klar 2001 p.586): all absent / any disagreement / all present. Until
    # 2026-10-11 the unpooled 'exactly k of n' cells were judged, and 6 raters with a 5% finding
    # (unpooled cells 0.41, 0.016, 0.0003) was flagged although its smallest pooled count is 6.3.
    six <- run_kp(kappa0 = 0.4, kappa1 = 0.6, props = "0.05", raters = "6")
    expect_equal(n_from(six), 316L)
    expect_false(grepl("Sparse categories", six$notices$content))
    # a genuinely sparse pooled cell is still flagged: 6 raters, 5% finding, kappa 0.2 -> 0.5,
    # N = 137, all-present cell 0.01 x 136.2 = 1.36, so 1 of the 3 cells is below 5
    sp <- run_kp(kappa0 = 0.2, kappa1 = 0.5, props = "0.05", raters = "6")
    expect_equal(n_from(sp), 137L)
    expect_match(sp$notices$content, "Sparse categories")
    expect_match(sp$notices$content, "goodness-of-fit cells")
    expect_match(sp$notices$content, "enriching the case series")
    expect_match(sp$notices$content, "smallest expected count is 1.4 and 1 of 3 cells")
    # prevalence 0.30 with 4, 5 and 6 raters: smallest pooled counts 10.1, 8.3 and 7.4
    for (r in c("4", "5", "6"))
        expect_false(grepl("Sparse categories", run_kp(raters = r)$notices$content), label = r)
    # the engine's own marginal warning is absent for this design, so the Notes panel is
    # the only place a sparse cell could be seen
    expect_false(grepl("expected cell count", six$text1$content))
    # 3 categories, 2 raters: the "both agree on category j" cells are p_j^2 + k p_j (1 - p_j)
    three <- run_kp(outcome = "3", kappa0 = 0.2, kappa1 = 0.5, props = "0.05, 0.45, 0.50")
    expect_match(three$notices$content, "Sparse categories")
    expect_match(three$notices$content, "collapsing rare categories")
    # more than two raters with several categories: the "all raters agree on j" cell is the
    # Dirichlet-multinomial product prod (p (1-k) + i k) / ((1-k) + i k), i = 0..n-1, which
    # matches kappaSize's Power3/4/5Cats polynomials to 1e-15. This design has N = 99 and its
    # smallest category marginal is 0.058 * 99 = 5.7 (no warning from the marginal rule), while
    # four of the five chi-square cells are below 5 (1.8, 0.46, 0.005, 0.2).
    six4 <- run_kp(outcome = "4", raters = "6", kappa0 = 0.0858, kappa1 = 0.183,
                   props = "0.417, 0.292, 0.058, 0.233")
    expect_equal(n_from(six4), 99L)
    expect_match(six4$notices$content, "Sparse categories")
    expect_match(six4$notices$content, "all raters agreeing on one category")
    # a comfortable design stays clean: default cells at N = 241 are all >= 42
    expect_false(grepl("Sparse categories", run_kp()$notices$content))
})


test_that("degenerate designs inside the option ranges are flagged, not answered silently", {
    # alpha just below power converges instantly to N = 1.4 -> "A minimum of 2 subjects".
    tiny <- run_kp(alpha = 0.19, power = 0.20)
    expect_lt(n_from(tiny), 10L)
    expect_match(tiny$notices$content, "Very small sample size")
    expect_match(tiny$notices$content, "Low power")
    # below 10 subjects every chi-square cell is sparse, but "enrich the case series" would
    # point at the wrong cause, so the sparse block stays quiet and the small-n block speaks
    expect_false(grepl("Sparse categories", tiny$notices$content))
    # a 0.01 gap between the kappas asks for ~96,000 subjects: the other transposition.
    # Two separate notices: the gap itself, and the size it produces.
    huge <- run_kp(kappa0 = 0.40, kappa1 = 0.41)
    expect_gt(n_from(huge), 2000L)
    expect_match(huge$notices$content, "Small kappa difference")
    expect_match(huge$notices$content, "differ by only 0.01", fixed = TRUE)
    expect_match(huge$notices$content, "Very large sample size")
    # a rare finding with the DEFAULT kappas also needs thousands (3,429 at 1% prevalence);
    # that notice must not blame the kappa gap, which is the conventional 0.2
    rare <- run_kp(props = "0.01")
    expect_gt(n_from(rare), 2000L)
    expect_match(rare$notices$content, "Very large sample size")
    expect_match(rare$notices$content, "rare finding")
    expect_false(grepl("Small kappa difference", rare$notices$content))
    # a close gap BELOW the null must not be told kappa1 should be an "improvement"
    down_close <- run_kp(kappa0 = 0.41, kappa1 = 0.40)$notices$content
    expect_match(down_close, "BELOW the null")
    expect_false(grepl("improvement over it", down_close))
    # n = 1 is written as "1 subject", not "1 subjects"
    one <- run_kp(kappa0 = 0.01, kappa1 = 0.99, raters = "6")
    expect_equal(n_from(one), 1L)
    expect_match(one$notices$content, "only 1 subject\\.")
    expect_match(flat(one$text2$content), "is 1 subject\\.")
    expect_match(flat(run_kp()$text2$content), "is 191 subjects\\.")
    # and the default design carries none of them
    ok <- run_kp()$notices$content
    expect_false(grepl("Very small|Low power|Very large|Small kappa", ok))
    # the estimand of all four method papers: intraclass kappa, no rater bias (2026-10-11)
    expect_match(ok, "unweighted intraclass kappa for nominal categories", fixed = TRUE)
    expect_match(ok, "each rated by all 2 raters")
})


test_that("proportions pasted with non-breaking spaces or piped decimal commas are understood", {
    # U+00A0 is what Word and Excel paste in place of a space; [:space:] does not match it.
    nbsp <- run_kp(props = "0.30\u00A00.70")
    expect_equal(n_from(nbsp), n_from(run_kp(props = "0.30, 0.70")))
    # the decimal-comma re-read accepts the same separators as the main parser
    expect_error(run_kp(props = "0,3|0,7"), "decimal point, not a decimal comma")
})


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/kappaSizePower.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    backend <- paste(readLines("../../R/kappaSizePower.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})


test_that("expected counts never reach the reader in scientific notation", {
    # signif() pasted into prose rendered the small tail as "1.4e-06" in a sentence aimed at
    # pathologists. Pinned directly rather than only through a swept design, so the floor
    # cannot drift: the same three assertions live in test-kappasizeci- and
    # test-kappasizefixedn-, and the three .fmtCount implementations must stay identical.
    # .fmtCount calls .() for its two literal returns, and jmvcore's .() looks up `self` in the
    # calling frame -- lifted out of $private_methods it is unbound and throws "object 'self'
    # not found". Bind it off an instance instead.
    an  <- ClinicoPath:::kappaSizePowerClass$new(options = ClinicoPath:::kappaSizePowerOptions$new())
    fmt <- an$.__enclos_env__$private$.fmtCount
    expect_equal(fmt(8.9e-06), "below 0.0001")
    expect_equal(fmt(0.0013),  "0.0013")
    expect_equal(fmt(0.013),   "0.013")
    expect_equal(fmt(4.9),     "4.9")
    expect_equal(fmt(NA_real_), "unavailable")
    # the six-rater 5%-finding design is the one the swept prose came from: 0.00034 must print
    # as itself, not be swallowed by the floor
    expect_equal(fmt(0.00034), "0.00034")
})


test_that("the marginal and the agreement-pattern warnings can both appear", {
    # They describe different quantities -- how empty the chi-square's cells are, versus how
    # few cases of the rare category the study will contain at all -- and the marginal block
    # used to be gated on the pattern block NOT having fired, which silenced it on exactly the
    # designs where both are true. Only the shared remedy sentence is dropped from the second.
    an <- ClinicoPath:::kappaSizePowerClass$new(options = ClinicoPath:::kappaSizePowerOptions$new())
    build <- an$.__enclos_env__$private$.buildNotices

    both <- build(176, sparse = TRUE, outcome = 5L, raters = 2L, kappa0 = 0.80, kappa1 = 0.90,
                  power = 0.80, sparse_min = 2.8, sparse_below5 = 1L, sparse_total = 6L,
                  marg_min = 3.5, marg_sparse = TRUE)
    expect_match(both, "Sparse categories", fixed = TRUE)
    expect_match(both, "Rare outcome category", fixed = TRUE)
    expect_equal(lengths(regmatches(both, gregexpr("collapsing rare categories", both)))[[1]], 1L)

    # marginal alone (Cochran silent) still speaks, and carries the remedy itself
    marg <- build(176, sparse = FALSE, outcome = 5L, raters = 2L, kappa0 = 0.80, kappa1 = 0.90,
                  power = 0.80, marg_min = 3.5, marg_sparse = TRUE)
    expect_false(grepl("Sparse categories", marg, fixed = TRUE))
    expect_match(marg, "Rare outcome category", fixed = TRUE)
    expect_match(marg, "collapsing rare categories", fixed = TRUE)

    # below 10 subjects the "Very small sample size" block owns the explanation
    expect_false(grepl("Rare outcome category",
                       build(4, sparse = TRUE, outcome = 5L, raters = 2L, kappa0 = 0.80,
                             kappa1 = 0.90, power = 0.80, marg_min = 0.1, marg_sparse = TRUE),
                       fixed = TRUE))
})


test_that("the required sample size is its own Notes block, not the last sentence of the Methodology", {
    # The bold figure is what gets copied into a protocol. It used to close a 250-400 word
    # "Methodology." paragraph (six raters: 15 sentences), so a reader had to finish the method
    # statement to find the answer. Its own block sits after the warnings and before the
    # Methodology; the siblings keep their N out of the Methodology too.
    blocks <- function(res) strsplit(res$notices$content, "</div>", fixed = TRUE)[[1]]
    for (res in list(run_kp(),                                            # plain
                     run_kp(props = "0.05", raters = "6"))) {             # simulated N shown
        b   <- blocks(res)
        req <- grep("Required sample size:", b, fixed = TRUE)
        met <- grep("Methodology.", b, fixed = TRUE)
        expect_length(req, 1L)
        expect_length(met, 1L)
        expect_false(grepl("Methodology.", b[req], fixed = TRUE))
        expect_false(grepl("Required sample size:", b[met], fixed = TRUE))
        expect_lt(req, met)
        # the warnings still come first
        if (grepl("Simulated power below", res$notices$content, fixed = TRUE))
            expect_lt(grep("Simulated power below", b, fixed = TRUE), req)
    }
    # no N, no block
    an <- ClinicoPath:::kappaSizePowerClass$new(options = ClinicoPath:::kappaSizePowerOptions$new())
    none <- an$.__enclos_env__$private$.buildNotices(NA_real_, sparse = FALSE, outcome = 2L,
                                                     raters = 2L, kappa0 = 0.4, kappa1 = 0.6, power = 0.8)
    expect_false(grepl("Required sample size", none, fixed = TRUE))
    expect_length(strsplit(none, "</div>", fixed = TRUE)[[1]], 1L)
})


test_that("the stated power prints with two decimals in every sentence", {
    # The simulation sentences printed "0.80" while the 3-5 category note, the "Low power" note and the
    # Study Explanation printed the same option as "0.8". A typed 0.825 keeps its third decimal.
    expect_match(flat(run_kp()$text2$content), "with power 0.80, in a study", fixed = TRUE)
    expect_match(run_kp(power = 0.30)$notices$content, "The requested power is 0.30.", fixed = TRUE)
    cat3 <- run_kp(outcome = "3", props = "0.05, 0.05, 0.9", raters = "4")
    expect_match(cat3$notices$content, "the stated power of 0.80 at", fixed = TRUE)
    expect_match(flat(cat3$text2$content), "fall well short of power 0.80;", fixed = TRUE)
    expect_match(flat(run_kp(power = 0.825)$text2$content), "with power 0.825, in a study", fixed = TRUE)
})


test_that("the kappa inputs carry the kappa0 / kappa1 names the output uses", {
    # Every note says kappa0 / kappa1 ("check that kappa0 is the agreement you want to rule OUT", "swap the two
    # values"); the controls were labelled "Null hypothesis" / "Alternative hypothesis" alone. The siblings
    # label theirs "Anticipated kappa (kappa0)".
    f <- file.path("../../jamovi", "kappaSizePower.u.yaml")
    skip_if_not(file.exists(f), "jamovi/kappaSizePower.u.yaml not found from the test directory")
    labels <- unlist(lapply(yaml::read_yaml(f)$children, function(x) rapply(x, function(v) v, how = "unlist")))
    expect_true("Null kappa (kappa0)" %in% labels)
    expect_true("Alternative kappa (kappa1)" %in% labels)
})


test_that("the two kappaSize panes say they hold the published-formula N", {
    # Both print kappaSize's "A minimum of N subjects are required", which is the published formula. When the
    # Notes give a larger simulated N (binary, 6 raters, 5% finding: 316 vs about 572), a pane titled
    # "Analysis result" read as the answer. Needs jmvtools::prepare() to reach R/kappaSizePower.h.R.
    res <- run_kp(props = "0.05", raters = "6")
    expect_equal(res$text1$title, "Result by the published formula (kappaSize)")
    expect_equal(res$text_summary$title, "Summary by the published formula (kappaSize)")
    expect_match(res$text1$content, "A minimum of 316 subjects", fixed = TRUE)
})
