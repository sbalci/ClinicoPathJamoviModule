# Regression tests for the 2026-10-21 independent review (development-ideas/
# sequentialtests-independent-review-2026-10-21.md), findings SEQ-21-F1 and SEQ-21-F4.

test_that("SEQ-21-F1: formulas panel conditions on disease status and names independence", {
    for (st in c("serial_positive", "serial_negative", "parallel")) {
        txt <- ClinicoPath::sequentialtests(strategy = st, show_formulas = TRUE)$formulas_text$content
        expect_match(txt, "conditionally independent given disease status", fixed = TRUE, info = st)
        expect_match(txt, "Among people with", fixed = TRUE, info = st)
    }
    sp <- ClinicoPath::sequentialtests(strategy = "serial_positive", show_formulas = TRUE)$formulas_text$content
    expect_false(grepl("<li>Given a positive Test 1", sp, fixed = TRUE))
    sn <- ClinicoPath::sequentialtests(strategy = "serial_negative", show_formulas = TRUE)$formulas_text$content
    expect_match(sn, "Among people without the disease, given a negative Test 1", fixed = TRUE)
    pa <- ClinicoPath::sequentialtests(strategy = "parallel", show_formulas = TRUE)$formulas_text$content
    expect_match(pa, "positive on both, assuming conditional independence", fixed = TRUE)
})

test_that("SEQ-21-F4: a preset that replaces values typed in an R call says so", {
    n <- ClinicoPath::sequentialtests(preset = "covid_screening_confirmation",
                                      test1_sens = 0.5, strategy = "parallel")$notices$content
    expect_match(n, "Teaching Example Replaced Entered Values", fixed = TRUE)
    expect_match(n, "Test 1 sensitivity: 0.5 -> 0.75", fixed = TRUE)
    expect_match(n, "Testing strategy: parallel -> serial_positive", fixed = TRUE)
    # values that agree with the preset (as the GUI's JavaScript writes them) are not listed
    agree <- ClinicoPath::sequentialtests(preset = "covid_screening_confirmation",
                                          test1_sens = 0.75, test1_spec = 0.95,
                                          test2_sens = 0.95, test2_spec = 0.99,
                                          prevalence = 0.08, strategy = "serial_positive")$notices$content
    expect_false(grepl("Replaced Entered Values", agree, fixed = TRUE))
    # and no note without a preset
    expect_false(grepl("Replaced Entered Values",
                       ClinicoPath::sequentialtests()$notices$content, fixed = TRUE))
})
