test_that("jjwithinstats handles presets, N reporting, and ggpubr correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    
    # Create test data
    set.seed(123)
    data <- data.frame(
        ID = 1:20,
        T1 = rnorm(20),
        T2 = rnorm(20),
        T3 = rnorm(20)
    )
    # Add some NAs to test reporting
    data$T2[1] <- NA
    data$T3[2] <- NA
    
    # Test 1: Retained N Reporting
    # ----------------------------
    # Should drop 2 cases (row 1 and 2), retaining 18
    results <- jjwithinstats(
        data = data,
        dep1 = "T1",
        dep2 = "T2",
        dep3 = "T3",
        clinicalpreset = "custom"
    )
    
    # Check messages in TODO (variable for messages)
    # The module now accumulates messages in todo output or warnings variable?
    # In .run(): self$results$todo$setContent(todo) AND self$results$todo$setContent(paste(private$.messages...))
    # It seems .accumulateMessage writes to todo content. Let's check todo content.
    
    # messages are now in warnings panel
    
    html_content <- results$warnings$content
    expect_match(html_content, "18 subjects retained")
    expect_match(html_content, "2 incomplete cases removed")
    
    # Test 2: Preset Feedback
    # -----------------------
    results_preset <- jjwithinstats(
        data = data,
        dep1 = "T1",
        dep2 = "T2",
        clinicalpreset = "biomarker"
    )
    
    html_preset <- results_preset$warnings$content
    expect_match(html_preset, "Biomarker Tracking \\(Guidance Only\\)")
    
    # Test 3: ggpubr execution (Paired)
    # ---------------------------------
    # We can't easily check the internal paired=TRUE call, but we can check if it runs without error.
    # And we can check if the plot is generated.
    
    results_ggpubr <- jjwithinstats(
        data = data,
        dep1 = "T1",
        dep2 = "T2",
        addGGPubrPlot = TRUE,
        ggpubrAddStats = TRUE
    )
    
    expect_true(results_ggpubr$ggpubrPlot$visible)
    # If implementation of listwise deletion was wrong, this might error or show different N
    
})
