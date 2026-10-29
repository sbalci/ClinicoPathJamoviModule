# Regression tests for .pValueText() and .tidyFinalfitMetrics() (R/utils.R), used by survival
# and survivalcont. Hand-built strings read "p = < 0.001" and finalfit's metrics line "p = 0.000".

test_that(".pValueText() carries the comparison sign with the value", {
    expect_identical(.pValueText(0.0004), "p < 0.001")
    expect_identical(.pValueText(0.04213), "p = 0.042")
    expect_identical(.pValueText(1), "p = 1.000")
    expect_identical(.pValueText(NA_real_), "p not available")
    expect_identical(.pValueText(numeric(0)), "p not available")
})

test_that(".tidyFinalfitMetrics() fixes the zero p and the glued bracket only", {
    x <- "R-squared = 0.126( Max possible = 0.999), Likelihood ratio test = 30.501 (df = 3, p = 0.000)"
    expect_identical(.tidyFinalfitMetrics(x),
        "R-squared = 0.126 (max possible = 0.999), Likelihood ratio test = 30.501 (df = 3, p < 0.001)")
    expect_identical(.tidyFinalfitMetrics("(df = 3, p = 0.0004)"), "(df = 3, p = 0.0004)")
    expect_identical(.tidyFinalfitMetrics("(df = 3, p = 0.042)"), "(df = 3, p = 0.042)")
})
