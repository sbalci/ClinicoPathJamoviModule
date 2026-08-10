# Regression tests from the `kappaSizePower` release review.
#
# The sample sizes are checked against the kappaSize package directly, not against the module's
# own arithmetic. The module is a thin wrapper and reproduced kappaSize exactly across 540
# swept combinations; what needed fixing was everything around the engine call.

first_line <- function(res) trimws(strsplit(res$text1$content, "\n")[[1]][1])
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
    expect_match(down$text2$content, "BELOW the null")
    expect_match(down$text2$content, "swap the two values")
    expect_false(grepl("BELOW the null", up$text2$content))
    # the two are genuinely different questions, so the sizes differ
    expect_false(identical(n_from(down), n_from(up)))
})


test_that("the study explanation reports the answer and reads correctly", {
    # It described the inputs but never the computed sample size, and the multi-category
    # sentence read "the prevalence of the categories are".
    two <- run_kp(props = "0.30, 0.70")$text2$content
    expect_match(two, "The required sample size is [0-9]+ subjects")
    expect_match(two, "prevalence of the trait is")

    three <- run_kp(outcome = "3", props = "0.20, 0.30, 0.50")$text2$content
    expect_match(three, "prevalences of the categories are 0.2, 0.3 and 0.5", fixed = TRUE)
    expect_false(grepl("prevalence of the categories are", three, fixed = TRUE))
    # the number quoted in the explanation must be the number in the result
    expect_match(three, paste0("is ", n_from(run_kp(outcome = "3", props = "0.20, 0.30, 0.50")),
                               " subjects"))
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
    expect_match(blk("kappasizepower.a.yaml", "kappa0"), "null hypothesis value of kappa")
    expect_match(blk("kappasizeci.a.yaml", "kappa0"), "preliminary \\(anticipated\\) value")
    expect_match(blk("kappasizefixedn.a.yaml", "kappa0"), "preliminary \\(anticipated\\) value")
    # power must NOT describe its null as the expected/anticipated value
    expect_false(grepl("Expected value of kappa", blk("kappasizepower.a.yaml", "kappa0")))

    # alpha bounds identical across the three
    for (f in c("kappasizepower.a.yaml", "kappasizeci.a.yaml", "kappasizefixedn.a.yaml")) {
        b <- blk(f, "alpha")
        expect_match(b, "min: 0.001", fixed = TRUE, info = f)
        expect_match(b, "max: 0.20", fixed = TRUE, info = f)
    }
})


test_that("each analysis states which question it is sizing for", {
    # Power and CI give materially different sample sizes for the same study because they
    # answer different questions, and neither said so.
    p <- run_kp()
    expect_match(p$text2$content, "POWER calculation")
    expect_match(p$text2$content, "kappaSizeCI")

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


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/kappasizepower.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    backend <- paste(readLines("../../R/kappasizepower.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})
