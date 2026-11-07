# Regressions from the 2026-10-30 jsurvival audit (lassocox).
# Runs against the umbrella (devtools::test) and an installed jsurvival
# (testthat::test_file(package = "jsurvival", load_package = "installed")).
#
# - todo got "" on a clean run; the jamovi client skips an empty Html update, so the
#   previous run's notes (or the welcome box) stayed on screen.
# - Syntax mode dropped an unset censorLevel (numeric 0/1 outcome), which the
#   wrapper requires, so the copied call failed "argument missing".

lassocox_af_data <- function() {
    set.seed(7); m <- 120
    x <- matrix(rnorm(m * 6), m); colnames(x) <- paste0("x", 1:6)
    t <- rexp(m, exp(0.7 * x[, 1] - 0.5 * x[, 2]) / 20); c <- runif(m, 5, 60)
    data.frame(time = pmax(pmin(t, c), 0.1), status = as.integer(t <= c), x)
}
lassocox_af_run <- function(data, ...) {
    skip_if_not_installed("glmnet")
    a <- lassocoxClass$new(options = lassocoxOptions$new(...), data = data)
    a$init(); suppressWarnings(a$run())
    a
}

test_that("a clean run sends a non-empty blank to todo and the text panels", {
    a <- lassocox_af_run(lassocox_af_data(), elapsedtime = "time", outcome = "status",
        outcomeLevel = "1", explanatory = paste0("x", 1:6))
    expect_identical(a$results$todo$content, "<div></div>")

    # Inputs removed after a run: .init() blanks every panel with non-empty content.
    b <- lassocox_af_run(lassocox_af_data(), elapsedtime = "time", outcome = "status",
        outcomeLevel = "1", explanatory = character(0), showRCode = TRUE, showSummary = TRUE)
    for (nm in c("summaryText", "suitabilityReport", "rCode"))
        expect_identical(b$results[[nm]]$content, "<div></div>", info = nm)
})

test_that("the Syntax-mode call keeps an unset censorLevel as NULL and runs", {
    d <- lassocox_af_data()
    a <- lassocox_af_run(d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
        explanatory = paste0("x", 1:6))
    src <- a$asSource()
    expect_match(src, "censorLevel = NULL", fixed = TRUE)
    call <- parse(text = sub("^[A-Za-z.]+::", "", src))[[1]]
    env <- new.env(parent = environment()); env$data <- d
    expect_error(suppressWarnings(eval(call, env)), NA)  # the copied call, as a user would run it
})
