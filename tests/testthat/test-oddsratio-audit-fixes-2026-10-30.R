# Regressions for the 2026-10-30 jsurvival audit findings on oddsratio:
#   1. Syntax mode dropped the unset Level options, which the generated wrapper
#      requires, so the copied call failed with 'argument "predictorLevel" is missing'.
#   2. A jamovi restart signalled by private$.checkpoint() inside
#      .prepareRmsNomogram() was swallowed by its catch-all tryCatch.

.or_obj <- function(data, ...) {
    Opt <- get0("oddsratioOptions", inherits = TRUE)
    Cls <- get0("oddsratioClass", inherits = TRUE)
    if (is.null(Opt) || is.null(Cls))
        testthat::skip("oddsratio classes are not reachable from the test environment")
    a <- Cls$new(options = Opt$new(...), data = data)
    a$init()
    a
}

.or_data <- function() {
    set.seed(11)
    data.frame(x = factor(sample(c("a", "b"), 80, TRUE)),
               y = factor(sample(c("Dead", "Alive"), 80, TRUE)))
}

test_that("Syntax-mode call runs when the predictor levels are unset", {
    df <- .or_data()
    a <- .or_obj(df, explanatory = "x", outcome = "y",
                 outcomeLevel = "Dead", outcomeNegativeLevel = "Alive")
    suppressWarnings(a$run())
    src <- a$asSource()
    expect_match(src, "predictorLevel = NULL", fixed = TRUE)
    expect_match(src, "predictorNegativeLevel = NULL", fixed = TRUE)
    # Run the copied call itself, not just parse() it.
    wrapper <- get0("oddsratio", mode = "function", inherits = TRUE)
    call_src <- sub("^[A-Za-z.]+::", "", sub("data = data", "data = df", src, fixed = TRUE))
    res <- suppressWarnings(eval(parse(text = call_src), envir = list(df = df, oddsratio = wrapper)))
    expect_false(is.null(res))
})

test_that("a checkpoint restart inside .prepareRmsNomogram propagates", {
    df <- .or_data()
    a <- .or_obj(df, explanatory = "x", outcome = "y",
                 outcomeLevel = "Dead", outcomeNegativeLevel = "Alive")
    pr <- a$.__enclos_env__$private
    # Stand in for jamovi's checkpoint answering "restart": jmvcore raises exactly
    # this error. (The real one first flushes results through RProtoBuf, which
    # only exists inside the jamovi engine.)
    unlockBinding(".checkpoint", pr)
    pr$.checkpoint <- function(flush = TRUE)
        stop(jmvcore::createError("restarting", "restart"))
    err <- tryCatch(suppressWarnings(pr$.prepareRmsNomogram(df, "y", "x")),
                    error = function(e) e)
    expect_s3_class(err, "error")
    expect_identical(err$code, "restart")
})
