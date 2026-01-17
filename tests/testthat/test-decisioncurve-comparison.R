
context("Decision Curve Analysis - Model Comparison")

test_that("decisioncurve creates valid bootstrap comparisons", {
    testthat::skip_on_cran()
    skip_if_not_installed('jmvReadWrite')
    devtools::load_all()
    
    # Load data
    data("histopathology", package = "ClinicoPath")
    test_data <- histopathology
    
    # Create two models
    set.seed(123)
    test_data$mod1 <- plogis(scale(as.numeric(test_data$Age))[,1])
    test_data$mod2 <- plogis(scale(as.numeric(test_data$MeasurementA))[,1])
    
    # Run decisioncurve with comparison
    # Use a small number of bootstrap reps for speed in test
    result <- decisioncurve(
        data = test_data,
        outcome = "Death",
        outcomePositive = "1",
        models = c("mod1", "mod2"),
        compareModels = TRUE,
        comparisonMethod = "bootstrap",
        bootReps = 100,
        decisionRuleVar = NULL,
        decisionRulePositive = NULL
    )
    
    # Check table content
    comp_table <- result$comparisonTable$asDF
    
    expect_true(nrow(comp_table) >= 1)
    expect_true("p_value" %in% names(comp_table))
    expect_true("ci_lower" %in% names(comp_table))
    expect_true("ci_upper" %in% names(comp_table))
    
    # Check values are not NA (assuming bootstrap succeeds)
    print(comp_table)
    
    expect_true(!is.na(comp_table$p_value[1]), info = "p-value should not be NA")
    expect_true(comp_table$p_value[1] >= 0 && comp_table$p_value[1] <= 1)
    
    expect_true(!is.na(comp_table$ci_lower[1]), info = "CI lower should not be NA")
    expect_true(!is.na(comp_table$ci_upper[1]), info = "CI upper should not be NA")
    
    # CI should contain the observed difference (usually) or at least be ordered correctly
    expect_true(comp_table$ci_lower[1] <= comp_table$ci_upper[1])
})
