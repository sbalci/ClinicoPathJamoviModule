context("pathagreement refinements")

# Source necessary files
testthat::source_test_helpers(env = environment())
. <- function(x) x # Dummy translation function

source('/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/pathagreement.b.R', local = TRUE)
source('/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/pathagreement.h.R', local = TRUE)

# Helper to create data
create_test_data <- function(n_rows = 50, n_raters = 3, type = "nominal", missing_prop = 0) {
    if (type == "nominal") {
        data <- as.data.frame(replicate(n_raters, sample(c("A", "B", "C"), n_rows, replace = TRUE), simplify = FALSE))
        for(i in 1:ncol(data)) data[[i]] <- as.factor(data[[i]])
    } else {
        data <- as.data.frame(replicate(n_raters, sample(1:5, n_rows, replace = TRUE), simplify = FALSE))
        for(i in 1:ncol(data)) data[[i]] <- as.ordered(data[[i]])
    }
    colnames(data) <- paste0("Rater", 1:n_raters)
    
    if (missing_prop > 0) {
        n_missing <- floor(n_rows * missing_prop)
        data[sample(1:n_rows, n_missing), 1] <- NA
    }
    
    return(data)
}

test_that("Warnings panel is populated", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    data <- create_test_data(n_rows = 50, n_raters = 2)
    options <- pathagreementOptions$new(
        vars = colnames(data),
        multiraterMethod = "fleiss" # Should trigger warning for 2 raters
    )
    
    results <- pathagreementResults$new(options = options)
    analysis <- pathagreementClass$new(options = options, data = data)
    analysis$run()
    
    # Check if warnings result exists and is populated
    expect_true(!is.null(analysis$results$warnings))
    content <- analysis$results$warnings$content
    expect_true(grepl("Fleiss' kappa requires 3 or more raters", content))
})

test_that("Weighted Kappa warns for nominal data", {
    # Create nominal data
    data <- create_test_data(n_rows = 50, n_raters = 2, type = "nominal")
    # Force them to be just factors, not ordered
    for(i in 1:ncol(data)) data[[i]] <- factor(data[[i]], ordered = FALSE)
    
    options <- pathagreementOptions$new(
        vars = colnames(data),
        wght = "detect" # or "linear" if available, but "unweighted" is default. 
        # Wait, option list is "unweighted", "squared", "equal". 
        # Let's try "equal" (linear)
    )
    # R6 object doesn't allow invalid options easily if validated, but internal logic checks it.
    # We need to manually set the private option value or pass valid one.
    # The options object validates. "equal" is valid option string.
    
    options <- pathagreementOptions$new(
        vars = colnames(data),
        wght = "equal"
    )

    results <- pathagreementResults$new(options = options)
    analysis <- pathagreementClass$new(options = options, data = data)
    analysis$run()
    
    content <- analysis$results$warnings$content
    expect_true(grepl("Analysis reverted to unweighted kappa", content) ||
                grepl("Weighted kappa requested but variables are not ordinal", content))
})

test_that("Missing data warning triggers", {
    data <- create_test_data(n_rows = 100, n_raters = 3, missing_prop = 0.25)
    
    options <- pathagreementOptions$new(
        vars = colnames(data)
    )
    
    results <- pathagreementResults$new(options = options)
    analysis <- pathagreementClass$new(options = options, data = data)
    analysis$run()
    
    content <- analysis$results$warnings$content
    expect_true(grepl("High missing data", content))
    expect_true(grepl("excluded due to missing ratings", content))
})

test_that("Clustering requirement warning", {
    # 1 rater (clustering needs 2)
    data <- create_test_data(n_rows = 50, n_raters = 2) 
    # Actually need < 2 raters to trigger the specific warning I added.
    # But I can't select < 2 vars in options easily without TODO showing up.
    # The check `n_vars_selected < 2` causes early return to todo.
    # So I can't test "Clustering with 1 rater" easily via `.run()` unless I mock `n_vars`.
    
    # Valid small data (n=4) but with variation to pass .validateData
    data <- data.frame(
        Rater1 = factor(c("A", "B", "A", "B")),
        Rater2 = factor(c("A", "A", "B", "B")),
        Rater3 = factor(c("B", "B", "A", "A"))
    )
    
    options <- pathagreementOptions$new(
        vars = colnames(data),
        performClustering = TRUE
    )
    
    results <- pathagreementResults$new(options = options)
    analysis <- pathagreementClass$new(options = options, data = data)
    analysis$run()
    
    content <- analysis$results$warnings$content
    expect_true(grepl("Too few cases", content))
    expect_true(grepl("reliable clustering. Skipped", content))
})
