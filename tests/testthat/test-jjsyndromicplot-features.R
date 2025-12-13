test_that("jjsyndromicplot handles missing data and reporting correctly", {
    
    # Create test data with NAs and Constant vars
    set.seed(123)
    data <- data.frame(
        A = rnorm(100),
        B = rnorm(100),
        C = rnorm(100)
    )
    # Introduce NAs
    data$A[1:10] <- NA
    # Add constant variable
    data$D <- 5
    
    # Test 1: Listwise Deletion (Default)
    # -----------------------------------
    results <- jjsyndromicplot(
        data = data,
        vars = c("A", "B", "C", "D"),
        missing = "listwise",
        showExplanations = TRUE
    )
    
    # Check Warnings for Data Summary
    html_content <- results$warnings$content
    expect_match(html_content, "Data Summary", fixed = TRUE)
    # Initial N=100, Analyzed N=90 (10 missing in A)
    expect_match(html_content, "Initial N: 100")
    expect_match(html_content, "Analyzed N: 90")
    expect_match(html_content, "Listwise Deletion")
    
    # Check Warnings for Constant Var Removal (D)
    expect_match(html_content, "zero variance")
    expect_match(html_content, "<code>D</code>")
    
    # Check Explanations for Visual Encoding Text
    expl_content <- results$explanations$content
    expect_match(expl_content, "High Color (e.g. Red)", fixed = TRUE)
    expect_match(expl_content, "arrow represents the variables", fixed = TRUE)
    
    
    # Test 2: Mean Imputation
    # -----------------------
    results_imp <- jjsyndromicplot(
        data = data,
        vars = c("A", "B", "C"),
        missing = "mean_imputation"
    )
    
    html_content_imp <- results_imp$warnings$content
    expect_match(html_content_imp, "Data Summary")
    # Initial N=100, Analyzed N=100 (Imputed)
    expect_match(html_content_imp, "Analyzed N: 100")
    expect_match(html_content_imp, "Mean Imputation")
    
    # Verify Imputation Logic correctness (simple check)
    # If imputation worked, results should differ from listwise
    # But mainly we check the reported N is 100.
    
    
    # Test 3: Preset Feedback
    # -----------------------
    results_preset <- jjsyndromicplot(
        data = data,
        vars = c("B", "C", "A"), # No missing in B/C, A has NAs but 'listwise' drops them
        clinicalPreset = "biomarker_discovery"
    )
    
    html_preset <- results_preset$warnings$content
    expect_match(html_preset, "Clinical Preset Applied: Biomarker Discovery")
    expect_match(html_preset, "Note: These settings override the UI controls")
    
})
