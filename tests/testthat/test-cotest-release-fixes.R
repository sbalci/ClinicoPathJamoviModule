# Release fixes 2026-10-21 (module audit M1): a run with nothing to report must REPLACE the notices
# panel. The jamovi client skips an Html update whose content is "", so "" left the previous run's
# notices (e.g. the worked-example disclosure after switching back to Custom) on screen.

test_that("a run with no notices sends non-empty blank markup", {
    opts <- ClinicoPath:::cotestOptions$new()
    a <- ClinicoPath:::cotestClass$new(options = opts, data = data.frame(x = 1))
    a$init()
    a$results$notices$setContent("<p>Worked example in use</p>")
    a$run()
    expect_identical(a$results$notices$content, "<div></div>")
})
