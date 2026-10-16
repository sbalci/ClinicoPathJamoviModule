# R CMD check WARNs on any non-ASCII character in R code outside comments ("Portable packages must use only
# ASCII characters in their R code"). release_gate.py has no such check, so on 2026-10-16 four files carried
# raw en dashes, kappa and curly quotes inside strings for up to 12 days unnoticed. Write them as \uXXXX: R
# resolves the escape at parse time, so the string -- and any .() catalog msgid -- is unchanged.
# This runs R's own scanner, which skips comments, over the source tree.

test_that("R code is ASCII outside comments (R CMD check portability)", {
    root <- testthat::test_path("..", "..")
    skip_if_not(file.exists(file.path(root, "DESCRIPTION")) &&
                    length(Sys.glob(file.path(root, "R", "*.b.R"))) > 0,
                "needs the package source tree")
    flagged <- tools:::.check_package_ASCII_code(root)
    expect_identical(basename(as.character(flagged)), character(0))
})
