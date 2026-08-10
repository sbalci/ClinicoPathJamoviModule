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
    as.integer(regmatches(l, regexpr("[0-9]+", l)))
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
    skip_on_cran()   # the guard deliberately spends its wall-clock budget before rejecting
    t0 <- Sys.time()
    expect_error(ci_run(kappaL = 0.5995, kappaU = 0.6005), "too narrow to size")
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    expect_lt(elapsed, 60)   # bounded; if this regresses the suite hangs rather than fails

    # the message must tell the user what to change
    msg <- tryCatch(ci_run(kappaL = 0.5995, kappaU = 0.6005), error = conditionMessage)
    expect_match(msg, "Widen the interval")
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
    expect_match(res$notices$content, "large-sample approximation")

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
    expect_match(txt, "nearer limit")
    expect_match(txt, "not the full width", fixed = TRUE)
    expect_false(grepl("Precision width", txt, fixed = TRUE))
})


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/kappasizeci.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    backend <- paste(readLines("../../R/kappasizeci.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})
