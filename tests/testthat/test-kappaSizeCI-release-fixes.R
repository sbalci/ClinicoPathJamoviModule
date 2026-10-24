# library-audit 2026-10-23 meddecide [MEDIUM] DONE: a rejected re-run leaves the panels alone - jamovi
#   greys the errored analysis with its previous results in place (guide section 5); a successful run
#   rewrites every panel, so one switched off cannot keep an earlier run's text.
test_that("a rejected re-run leaves the panels to jamovi's error state; a success clears them", {
    opts <- ClinicoPath:::kappaSizeCIOptions$new(props = "0.5, 0.6")
    a <- ClinicoPath:::kappaSizeCIClass$new(options = opts, data = data.frame())
    a$init()
    a$results$text1$setContent("Required sample size: 118")
    expect_error(a$run())
    expect_identical(a$results$text1$content, "Required sample size: 118")

    ok <- ClinicoPath:::kappaSizeCIClass$new(options = ClinicoPath:::kappaSizeCIOptions$new(), data = data.frame())
    ok$init()
    ok$results$text_summary$setContent("old summary")
    ok$results$text2$setContent("old explanation")
    ok$run()
    expect_identical(ok$results$text_summary$content, "")
    expect_identical(ok$results$text2$content, "")
    expect_true(nzchar(ok$results$text1$content))
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
