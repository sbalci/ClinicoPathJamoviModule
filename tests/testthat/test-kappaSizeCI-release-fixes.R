# Release fixes 2026-10-21 (independent review F-01): a rejected re-run must not leave the previous
# run's n on screen. jamovi keeps an errored analysis's items (greyed), so .run() blanks them first.

test_that("a rejected re-run blanks the previous sample size and notices", {
    opts <- ClinicoPath:::kappaSizeCIOptions$new(props = "0.5, 0.6")
    a <- ClinicoPath:::kappaSizeCIClass$new(options = opts, data = data.frame())
    a$init()
    a$results$text1$setContent("Required sample size: 118")
    a$results$notices$setContent("<p>old notice</p>")
    expect_error(a$run())
    expect_identical(a$results$text1$content, "")
    # Html "" is skipped by the jamovi client, so the blank must be non-empty markup
    expect_identical(a$results$notices$content, "<div></div>")
})

test_that("F-02: the default view says which two-sided limit sets n", {
    # Defaults: [0.4, 0.8] around 0.6, n 118; the lower limit binds (expected limits [0.4003, 0.7489])
    r <- ClinicoPath::kappaSizeCI()
    expect_match(r$text1$content, "The lower limit (0.4) drives the sample size; the expected upper limit then falls at or inside 0.8.",
                 fixed = TRUE)
    # kappa0 0.75 in [0.60, 0.85]: the geometrically nearer upper limit does NOT bind (review 2026-10-15)
    r <- ClinicoPath::kappaSizeCI(kappa0 = 0.75, kappaL = 0.60, kappaU = 0.85)
    expect_match(r$text1$content, "The lower limit (0.6) drives the sample size", fixed = TRUE)
    r <- ClinicoPath::kappaSizeCI(kappa0 = 0.3, kappaL = 0.1, kappaU = 0.4)
    expect_match(r$text1$content, "The upper limit (0.4) drives the sample size; the expected lower limit then falls at or inside 0.1.",
                 fixed = TRUE)
    # one-sided: a single limit, nothing to name
    r <- ClinicoPath::kappaSizeCI(citype = "one_sided")
    expect_false(grepl("drives the sample size", r$text1$content, fixed = TRUE))
})
