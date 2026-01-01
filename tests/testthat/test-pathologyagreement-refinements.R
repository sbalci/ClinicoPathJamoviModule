
context("pathologyagreement refinements")

# Source necessary files
testthat::source_test_helpers(env = environment())
. <- function(x) x # Dummy translation function
source('/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/pathologyagreement.b.R', local = TRUE)
source('/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/pathologyagreement.h.R', local = TRUE)

test_that("Sample size warning triggers for N < 30", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    data <- data.frame(
        m1 = rnorm(20),
        m2 = rnorm(20) + 0.5
    )
    
    options <- pathologyagreementOptions$new(
        dep1 = "m1",
        dep2 = "m2"
    )
    
    results <- pathologyagreementResults$new(options = options)
    analysis <- pathologyagreementClass$new(options = options, data = data)
    analysis$run()
    
    expect_true(!is.null(analysis$results$warnings))
    content <- analysis$results$warnings$content
    expect_true(grepl("Sample size", content))
    expect_true(grepl("below recommended minimum", content))
})

test_that("Normality check warns for Pearson on non-normal data", {
    # Generate skewed data
    data <- data.frame(
        m1 = exp(rnorm(50)),
        m2 = exp(rnorm(50))
    )
    
    options <- pathologyagreementOptions$new(
        dep1 = "m1",
        dep2 = "m2",
        correlation_method = "pearson"
    )
    
    results <- pathologyagreementResults$new(options = options)
    analysis <- pathologyagreementClass$new(options = options, data = data)
    analysis$run()
    
    content <- analysis$results$warnings$content
    expect_true(grepl("Pearson correlation assumes normality", content))
    expect_true(grepl("Spearman rank correlation is recommended", content))
})

test_that("CCC interpretation is correct for high values", {
    # Perfect agreement
    m1 <- runif(50)
    data <- data.frame(
        m1 = m1,
        m2 = m1 # Identical
    )
    
    options <- pathologyagreementOptions$new(
        dep1 = "m1",
        dep2 = "m2"
    )
    
    results <- pathologyagreementResults$new(options = options)
    analysis <- pathologyagreementClass$new(options = options, data = data)
    analysis$run()
    
    table <- analysis$results$agreementtable
    # Row 2 is CCC
    interp <- table$getCell(rowKey = 2, col = "interpretation")$value
    expect_equal(interp, "Almost perfect")
})

test_that("Bland-Altman CIs are populated", {
    data <- data.frame(
        m1 = rnorm(50),
        m2 = rnorm(50)
    )
    
    options <- pathologyagreementOptions$new(
        dep1 = "m1",
        dep2 = "m2"
    )
    
    results <- pathologyagreementResults$new(options = options)
    analysis <- pathologyagreementClass$new(options = options, data = data)
    analysis$run()
    
    table <- analysis$results$agreementtable
    # Row 3 is BA Mean Diff (Bias)
    ci_lower <- table$getCell(rowKey = 3, col = "ci_lower")$value
    ci_upper <- table$getCell(rowKey = 3, col = "ci_upper")$value
    
    expect_true(!is.na(ci_lower))
    expect_true(!is.na(ci_upper))
})

test_that("Listwise deletion message occurs", {
    # Create data with missing values
    data <- data.frame(
        m1 = c(1, 2, 3, NA, 5, 6),
        m2 = c(1, 2, NA, 4, 5, 6)
    )
    
    options <- pathologyagreementOptions$new(
        dep1 = "m1",
        dep2 = "m2",
        missing_data = "listwise"
    )
    
    results <- pathologyagreementResults$new(options = options)
    analysis <- pathologyagreementClass$new(options = options, data = data)
    analysis$run()
    
    content <- analysis$results$warnings$content
    expect_true(grepl("Listwise deletion", content))
    expect_true(grepl("cases removed", content))
})
