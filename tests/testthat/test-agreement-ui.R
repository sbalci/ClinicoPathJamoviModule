test_that("agreement welcome uses stacked sections before raters are selected", {
    for (vars in list(NULL, "R1")) {
        data <- if (is.null(vars)) data.frame() else data.frame(R1 = factor(c("A", "B")))
        a <- agreement_run(data, vars = vars)
        html <- a$results$welcome$content
        expect_true(isTRUE(a$results$welcome$visible))
        expect_match(html, "Quick Start", fixed = TRUE)
        expect_match(html, "Available Methods", fixed = TRUE)
        expect_match(html, "<ol", fixed = TRUE)
        expect_match(html, "<ul", fixed = TRUE)
        expect_false(grepl("<table|<tr|<td", html))
    }
})
