# ═══════════════════════════════════════════════════════════
# Regression: jmvcore::format() unbounded substitution loop
#
# jmvcore::format() re-scans the whole string from position 1 after every
# substitution. A value containing its OWN placeholder is therefore found and
# substituted again, forever. The loop does not poll R's interrupt handler, so
# setTimeLimit() cannot break it and the process must be SIGKILLed -- inside
# jamovi that freezes the analysis engine instead of raising an error.
#
# .fmt() (R/utils.R) is the guarded wrapper every .b.R now calls.
# ═══════════════════════════════════════════════════════════
library(testthat)

test_that(".fmt is byte-identical to jmvcore::format for brace-free values", {
    # The guarantee that makes the module-wide sweep safe: when no supplied value
    # contains a brace -- every ordinary call -- .fmt must delegate untouched.
    values <- list(5L, 100000L, 3.14, "text", sprintf("%.1f%%", 12.345),
                   "a b c", "", "50%", "a-b_c")
    formats <- c("n = {n}", "{n}", "x {n} y", "{n} and {n}")
    for (v in values) for (f in formats)
        expect_identical(ClinicoPath:::.fmt(f, n = v), jmvcore::format(f, n = v),
                         info = paste("format:", f, "value:", format(v)))
})

test_that(".fmt permits a placeholder named fmt", {
    # `fmt` used to be the wrapper's first formal argument. Passing fmt = "dmy"
    # therefore replaced the template itself and reduced the whole message to
    # "dmy", which hid swimmerplot's date-validation guidance.
    template <- "Dates could not be parsed with the selected format ({fmt})."
    expect_identical(
        ClinicoPath:::.fmt(template, fmt = "dmy"),
        "Dates could not be parsed with the selected format (dmy)."
    )
})

test_that(".fmt terminates when a value contains its own placeholder", {
    # Without the guard this call never returns. A plain expect_* would hang the
    # whole suite, so failure here shows up as a hung run, not a red test -- which
    # is precisely why the guard lives in a wrapper rather than at each call site.
    expect_identical(ClinicoPath:::.fmt("LR ({value})", value = "x {value} y"),
                     "LR (x (value) y)")
})

test_that(".fmt terminates for user data that looks like a placeholder", {
    # A jamovi column or factor level may legally be named "{n}".
    expect_identical(ClinicoPath:::.fmt("Variable {n} has missing data", n = "{n}"),
                     "Variable (n) has missing data")
    expect_identical(ClinicoPath:::.fmt("Error: {msg}", msg = "bad token {msg} here"),
                     "Error: bad token (msg) here")
})

test_that("no .b.R calls jmvcore::format directly", {
    # The sweep is only protective while it stays complete; a new direct call would
    # silently reintroduce the freeze. Catches the do.call() form too.
    skip_if_not(dir.exists("../../R"), "source tree not available")
    hits <- character(0)
    for (f in Sys.glob("../../R/*.b.R")) {
        src <- readLines(f, warn = FALSE)
        code <- src[!grepl("^\\s*#", src)]                 # ignore comment lines
        if (any(grepl("jmvcore::format", code, fixed = TRUE)))
            hits <- c(hits, basename(f))
    }
    expect_identical(hits, character(0))
})
