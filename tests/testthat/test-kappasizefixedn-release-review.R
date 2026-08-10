# Regression tests from the `kappaSizeFixedN` release review.
#
# The bound is checked against the kappaSize package directly. The module reproduced kappaSize
# exactly across 2,000 swept cells (max difference 6e-16, pure print rounding), so what needed
# fixing was everything around the engine call.

fn_run <- function(...) {
    args <- utils::modifyList(
        list(outcome = "2", kappa0 = 0.60, props = "0.30, 0.70",
             raters = "2", alpha = 0.05, n = 100),
        list(...))
    do.call(ClinicoPath::kappaSizeFixedN, args)
}
fn_line <- function(res) trimws(strsplit(res$text1$content, "\n")[[1]][1])


test_that("the lower bound matches kappaSize exactly", {
    skip_if_not_installed("kappaSize")
    expect_match(fn_line(fn_run()),
                 sprintf("lower limit for kappa of %s",
                         kappaSize::FixedNBinary(kappa0 = 0.60, n = 100, props = c(0.30, 0.70),
                                                 alpha = 0.05, raters = 2)$kappaL),
                 fixed = TRUE)
    expect_match(fn_line(fn_run(outcome = "3", props = "0.20, 0.30, 0.50")),
                 sprintf("lower limit for kappa of %s",
                         kappaSize::FixedN3Cats(kappa0 = 0.60, n = 100,
                                                props = c(0.20, 0.30, 0.50),
                                                alpha = 0.05, raters = 2)$kappaL),
                 fixed = TRUE)
})


test_that("n = Inf is refused instead of hanging the engine", {
    # The old guard was `is.na(n) || n < 2 || n != round(n)`, and Inf passes all three
    # (is.na(Inf) FALSE, Inf < 2 FALSE, Inf != round(Inf) FALSE). Inside the engine the test
    # statistic becomes NaN, the while loop never terminates, and jamovi cannot abort it.
    t0 <- Sys.time()
    expect_error(fn_run(n = Inf), "between 11 and 1e\\+06")
    expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 20)

    # A whole-number check is still the backend's job: `n` is a Number option, so 100.5 is
    # inside the compiled bounds and only the backend stops it before the engine sees it.
    expect_error(fn_run(n = 100.5), "whole number")
    expect_error(fn_run(n = 2e6), "between 11 and 1e\\+06")

    # NA/NaN die in jmvcore's own range check on `if (value < min)` with "missing value where
    # TRUE/FALSE needed". That message is poor but jmvcore-wide, not this analysis's to fix.
    expect_error(fn_run(n = NA_real_))
    expect_error(fn_run(n = NaN))
})


test_that("n below the engine's floor is refused with a reason, not a vendor string", {
    # Every kappaSize FixedN* engine contains
    #   if (n <= 10) stop("Sorry, your study should enroll at least 10 subjects.")
    # while the option allowed n down to 2, so 2..10 reached the engine only to bounce back as
    # a raw vendor message (whose own wording is off by one).
    # The compiled `min: 11` now stops these at the option layer, so the message a user sees is
    # jmvcore's; the backend clause below it remains as a backstop if the bound is ever relaxed.
    for (bad in c(2, 5, 10))
        expect_error(fn_run(n = bad), "between 11 and", info = paste("n =", bad))
    expect_no_error(fn_run(n = 11))
    # never the raw vendor string, whose own wording is off by one ("at least 10")
    expect_false(grepl("Sorry", tryCatch(fn_run(n = 5), error = conditionMessage), fixed = TRUE))

    # `n` is quoted in the yaml ('n'), because a bare n is a YAML 1.1 boolean and R's yaml
    # package hands back FALSE where jamovi's js-yaml hands back the string. Accept either form
    # so this pins the bound, not the quoting style.
    a_yaml <- paste(readLines("../../jamovi/kappasizefixedn.a.yaml", warn = FALSE), collapse = "\n")
    blk <- regmatches(a_yaml, regexpr("(?s)    - name: '?n'?\\n.*?(?=\\n    - name: |\\Z)",
                                      a_yaml, perl = TRUE))
    expect_length(blk, 1L)
    expect_match(blk, "min: 11", fixed = TRUE)

    # and the compiled wrapper actually carries it
    h <- paste(readLines("../../R/kappasizefixedn.h.R", warn = FALSE), collapse = "\n")
    expect_match(h, "min=11", fixed = TRUE)
})


test_that("a lower bound outside kappa's range is refused, not printed", {
    # kappaSize decrements rho from kappa0 by 0.001 with no floor, so an underpowered design
    # walks past -1: kappa0 = 0.01, n = 11, prevalence 0.02, alpha = 0.001 returns -23.78.
    # Cohen's kappa is bounded below by -1, so that is not a number to show a clinician.
    skip_if_not_installed("kappaSize")
    raw <- kappaSize::FixedNBinary(kappa0 = 0.01, n = 11, props = c(0.02, 0.98),
                                   alpha = 0.001, raters = 2)$kappaL
    expect_lt(raw, -1)                                  # the engine really does return it
    expect_error(fn_run(kappa0 = 0.01, n = 11, props = "0.02, 0.98", alpha = 0.001),
                 "cannot be below -1")

    # a legitimately NEGATIVE bound (>= -1) is still reported, with an explanation
    res <- fn_run(kappa0 = 0.20, n = 20, props = "0.30, 0.70", alpha = 0.001)
    expect_match(res$text2$content, "-0.293", fixed = TRUE)
    expect_match(res$text2$content, "no better than chance")
})


test_that("stale results do not survive a rejected re-run", {
    o <- ClinicoPath:::kappaSizeFixedNOptions$new(outcome = "2", kappa0 = 0.60,
             props = "0.30, 0.70", raters = "2", alpha = 0.05, n = 100)
    a <- ClinicoPath:::kappaSizeFixedNClass$new(options = o, data = data.frame(x = 1))
    p <- a$.__enclos_env__$private
    a$init()
    p$.run()
    expect_match(a$results$text1$content, "lower limit for kappa")

    op <- o$option("props"); op$value <- "0.30, 0.60"     # no longer sums to 1
    expect_error(p$.run(), "sum to 1")
    expect_equal(a$results$text1$content, "")             # previous numbers must be gone
    expect_equal(a$results$text2$content, "")
})


test_that("a decimal comma is diagnosed as such, not as the wrong count", {
    # "0,30 0,70" splits into 0, 30, 0, 70 and used to be reported as four proportions.
    expect_error(fn_run(props = "0,30 0,70"), "decimal point, not a decimal comma")
    expect_error(fn_run(props = "0,5 0,5"), "decimal point, not a decimal comma")
})


test_that("the explanation states the answer and reads correctly for one prevalence", {
    two <- fn_run(props = "0.30, 0.70")$text2$content
    expect_match(two, "The expected lower bound for kappa is")
    expect_match(two, "proportions of the outcome categories are")

    one <- fn_run(props = "0.30")$text2$content
    expect_match(one, "prevalence of the trait is 0.30", fixed = TRUE)
    expect_false(grepl("proportions of the outcome categories", one, fixed = TRUE))
    # both entry styles are the same computation
    expect_equal(fn_line(fn_run(props = "0.30")), fn_line(fn_run(props = "0.30, 0.70")))
})


test_that("alpha is bounded consistently with the compiled option", {
    # The backend permitted (0,1) while the option compiles to 0.001-0.20, so an R caller could
    # reach the engine with alpha = 0.5 and get "missing value where TRUE/FALSE needed".
    expect_error(fn_run(alpha = 0.5), "between")
    expect_error(fn_run(alpha = 0.0005), "between")
    expect_no_error(fn_run(alpha = 0.001))
    expect_no_error(fn_run(alpha = 0.20))
})


test_that("the proportions sum tolerance matches the engine's", {
    # The module used all.equal(tolerance = 1e-3), which accepts |sum - 1| <= 0.001, while every
    # kappaSize engine rejects at abs(sum(props) - 1) >= 0.001. The module now uses the engine's
    # own predicate, so no sum can slip past the clear message and hit the vendor one.
    expect_error(fn_run(props = "0.302, 0.70"), "sum to 1")     # 1.002
    expect_error(fn_run(props = "0.31, 0.70"), "sum to 1")      # 1.010
    expect_no_error(fn_run(props = "0.3004, 0.70"))             # 1.0004, inside both tolerances
    expect_no_error(fn_run(props = "0.3005, 0.6995"))           # exactly 1

    # 1.001 is the knife edge and lands on the accept side of BOTH predicates, because
    # abs(1.001 - 1) is 0.0009999999999998899 in binary floating point. Module and engine agree,
    # which is the property that matters; pin it so a "tidy-up" to > or to a rounded compare
    # cannot silently reintroduce the divergence.
    expect_no_error(fn_run(props = "0.301, 0.70"))
    skip_if_not_installed("kappaSize")
    expect_no_error(kappaSize::FixedNBinary(kappa0 = 0.60, n = 100, props = c(0.301, 0.70),
                                            alpha = 0.05, raters = 2))
})


test_that("proportions parse on every separator the family accepts", {
    # kappaSizeCI takes "[,;|[:space:]]+"; this analysis took "[,;[:space:]]+", so a pipe-
    # separated list that worked in one member of the family failed in the next.
    base <- fn_line(fn_run(props = "0.30, 0.70"))
    for (ps in c("0.30 0.70", "0.30;0.70", "0.30|0.70", "0.30  ,  0.70", "0.30\t0.70"))
        expect_equal(fn_line(fn_run(props = ps)), base, label = ps)
})


test_that("the Notes panel states the method and the kappa0 hazard", {
    # kappa0 means the ANTICIPATED kappa here and the NULL kappa in kappaSizePower. Two
    # analyses in the same menu taking an identically-named argument with different meanings is
    # exactly the kind of thing that produces a confidently wrong sample size.
    a <- ClinicoPath:::kappaSizeFixedNClass$new(
        options = ClinicoPath:::kappaSizeFixedNOptions$new(
            outcome = "2", kappa0 = 0.60, props = "0.30, 0.70",
            raters = "2", alpha = 0.05, n = 100),
        data = data.frame(x = 1))
    build <- a$.__enclos_env__$private$.buildNotices

    plain <- build(0.44, sparse_cells = FALSE)
    expect_match(plain, "Methodology")
    expect_match(plain, "kappa0 here is the agreement you anticipate")
    expect_match(plain, "kappaSizePower", fixed = TRUE)
    expect_false(grepl("Sparse categories", plain, fixed = TRUE))
    expect_false(grepl("cannot demonstrate agreement", plain, fixed = TRUE))

    expect_match(build(0.44, sparse_cells = TRUE), "Sparse categories")

    # a bound at or below zero is the decisive clinical case and gets its own red block
    expect_match(build(-0.29), "cannot demonstrate agreement")
    expect_match(build(0), "cannot demonstrate agreement")
    expect_false(grepl("cannot demonstrate agreement", build(0.001), fixed = TRUE))
})


test_that("the sparse-cell caveat is detected from kappaSize's own summary text", {
    skip_if_not_installed("kappaSize")
    res <- kappaSize::FixedN5Cats(kappa0 = 0.60, n = 20, props = c(0.01, 0.04, 0.15, 0.30, 0.50),
                                  alpha = 0.05, raters = 2)
    txt <- paste(utils::capture.output(summary(res)), collapse = "\n")
    expect_true(grepl("expected cell count is less than five", txt, fixed = TRUE))

    dense <- paste(utils::capture.output(summary(
        kappaSize::FixedNBinary(kappa0 = 0.60, n = 500, props = c(0.30, 0.70),
                                alpha = 0.05, raters = 2))), collapse = "\n")
    expect_false(grepl("expected cell count is less than five", dense, fixed = TRUE))
})


test_that("the notices item is declared in .r.yaml with the same clearWith as the rest", {
    # Adding it requires jmvtools::prepare(); until that runs the analysis errors with
    # "'notices' does not exist in this results element", so this pins the yaml side.
    # The bool handlers matter: this analysis has an option literally named `n`, which is a
    # YAML 1.1 boolean, so R's yaml package (libyaml, 1.1) hands back FALSE where jamovi's
    # js-yaml (1.2) hands back the string "n". Without them this test compares against FALSE.
    y <- yaml::read_yaml("../../jamovi/kappasizefixedn.r.yaml",
                         handlers = list("bool#yes" = function(x) x, "bool#no" = function(x) x))
    items <- setNames(y$items, vapply(y$items, function(i) i$name, character(1)))
    expect_true("notices" %in% names(items))
    expect_equal(items$notices$type, "Html")
    opts <- c("outcome", "kappa0", "props", "raters", "alpha", "n")
    for (nm in names(items))
        expect_setequal(items[[nm]]$clearWith, opts)
})


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/kappasizefixedn.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    backend <- paste(readLines("../../R/kappasizefixedn.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})
