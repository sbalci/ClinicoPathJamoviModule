# Regression tests from the `kappaSizePower` release review.
#
# The sample sizes are checked against the kappaSize package directly, not against the module's
# own arithmetic. The module is a thin wrapper and reproduced kappaSize exactly across 540
# swept combinations; what needed fixing was everything around the engine call.

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


test_that("sparse agreement-pattern cells are detected, not just sparse categories", {
    # kappaSize sizes the study with a chi-square over agreement patterns (exactly j of n
    # raters positive), but both its own warning and the module's first rule looked only at
    # the category marginals. With 6 raters and a 5% finding the marginal is 0.05 * 316 = 16
    # while three of the seven pattern cells have expected counts of 0.41, 0.016 and 0.0003.
    six <- run_kp(kappa0 = 0.4, kappa1 = 0.6, props = "0.05", raters = "6")
    expect_equal(n_from(six), 316L)
    expect_match(six$notices$content, "Sparse categories")
    expect_match(six$notices$content, "agreement-pattern cell")
    expect_match(six$notices$content, "enriching the case series")
    # the engine's own marginal warning is absent for this design, so the Notes panel is
    # the only place it can be seen
    expect_false(grepl("expected cell count", six$text1$content))
    # 4 raters at 10% prevalence: N = 218, one cell at 0.47
    expect_match(run_kp(kappa0 = 0.4, kappa1 = 0.6, props = "0.10", raters = "4")$notices$content,
                 "Sparse categories")
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
    expect_match(ok, "unweighted \\(nominal-category\\) kappa")
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
