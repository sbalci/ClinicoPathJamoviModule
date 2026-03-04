
test_that("classification educational features work", {
  skip_if_not_installed('jmvReadWrite')
    
    # Load test data
    data("BreastCancer", package = "ClinicoPath")
    
    # Test 1: Natural Language Summary
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size", "Cell.shape"),
            classifier = "singleDecisionTree",
            testing = "split",
            testSize = 0.3,
            showSummary = TRUE
        )
    })
    
    # Verify summary content
    summary_html <- results$naturalSummary$content
    testthat::expect_true(!is.null(summary_html))
    testthat::expect_true(nchar(summary_html) > 0)
    testthat::expect_true(grepl("Clinical Summary", summary_html))
    testthat::expect_true(grepl("Analysis Type", summary_html))
    
    # Test 2: About Panel
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            testing = "split",
            showAbout = TRUE
        )
    })
    
    # Verify about content
    about_html <- results$aboutAnalysis$content
    testthat::expect_true(!is.null(about_html))
    testthat::expect_true(nchar(about_html) > 0)
    testthat::expect_true(grepl("About Clinical Classification Analysis", about_html))
    
    # Test 3: Glossary Panel
    testthat::expect_silent({
        results <- ClinicoPath::classification(
            data = BreastCancer,
            dep = "Class",
            indep = c("Cl.thickness", "Cell.size"),
            classifier = "singleDecisionTree",
            testing = "split",
            showGlossary = TRUE
        )
    })
    
    # Verify glossary content
    glossary_html <- results$glossaryPanel$content
    testthat::expect_true(!is.null(glossary_html))
    testthat::expect_true(nchar(glossary_html) > 0)
    testthat::expect_true(grepl("Statistical Glossary", glossary_html))
    testthat::expect_true(grepl("Sensitivity", glossary_html))
    testthat::expect_true(grepl("Specificity", glossary_html))
})
