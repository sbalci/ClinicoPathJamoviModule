
test_that("lollipop handles warnings and validation correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    
    # Test 1: Small Sample Size Warning
    # ---------------------------------
    set.seed(123)
    data_small <- data.frame(
        dep = rnorm(8),
        group = rep(c("A", "B"), 4)
    )
    
    results <- lollipop(
        data = data_small,
        dep = "dep",
        group = "group",
        highlight = NULL
    )
    
    # Check if warnings panel is visible and contains expected message
    expect_true(results$warnings$visible)
    html_content <- results$warnings$content
    expect_match(html_content, "Small sample size")
    
    # Test 2: Duplicate Observations Warning (Aggregation = None)
    # -----------------------------------------------------------
    data_dups <- data.frame(
        dep = c(1, 2, 3, 4), # 1 and 2 are both group A
        group = c("A", "A", "B", "B")
    )
    
    results_dups <- lollipop(
        data = data_dups,
        dep = "dep",
        group = "group",
        highlight = NULL,
        aggregation = "none"
    )
    
    html_content_dups <- results_dups$warnings$content
    expect_match(html_content_dups, "Multiple observations per group detected")
    expect_match(html_content_dups, "Consider using aggregation")
    
    # Test 3: No Duplicate Warning when Aggregation is Used
    # -----------------------------------------------------
    results_agg <- lollipop(
        data = data_dups,
        dep = "dep",
        group = "group",
        highlight = NULL,
        aggregation = "mean"
    )
    
    html_content_agg <- results_agg$warnings$content
    # Depending on implementation, it might be NULL or just missing the specific warning
    if (!is.null(html_content_agg)) {
        expect_no_match(html_content_agg, "Multiple observations per group detected")
    } else {
        expect_null(html_content_agg)
    }

    # Test 4: Conditional Coloring Note
    # ---------------------------------
    # Use larger data to avoid small sample warning
    data_large <- data.frame(
        dep = rnorm(20),
        group = rep(c("A", "B"), 10)
    )
    
    results_color <- lollipop(
        data = data_large,
        dep = "dep",
        group = "group",
        highlight = NULL,
        conditionalColor = TRUE,
        colorThreshold = 0
    )
    
    html_content_color <- results_color$warnings$content
    expect_match(html_content_color, "Conditional coloring applied")
    expect_match(html_content_color, "colored orange")
    
})
