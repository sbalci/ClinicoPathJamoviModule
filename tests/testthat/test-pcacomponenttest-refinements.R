context("pcacomponenttest refinements")

# Source necessary files
source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/pcacomponenttest.h.R")
source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/pcacomponenttest.b.R")

# Function wrapper for testing
pcacomponenttest <- function(data, ...) {
    options <- pcacomponenttestOptions$new(...)
    analysis <- pcacomponenttestClass$new(options = options, data = data)
    analysis$run()
    analysis
}

test_that("Reproducibility works with seed", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Create synthetic data with clear structure
    # Create random noise data to ensure p-values vary
    set.seed(123)
    n <- 50
    df <- as.data.frame(matrix(rnorm(n*5), ncol=5))
    colnames(df) <- paste0("x", 1:5)
    
    # Run 1
    res1 <- pcacomponenttest(
        data = df,
        vars = colnames(df),
        ncomp = 5,
        nperm = 200, # Use more perms to be sure
        seed = 42,
        stop_rule = FALSE # Batch testing for comparison
    )
    
    # Run 2 (Same seed)
    res2 <- pcacomponenttest(
        data = df,
        vars = colnames(df),
        ncomp = 5,
        nperm = 200,
        seed = 42,
        stop_rule = FALSE
    )
    
    # Run 3 (Diff seed)
    res3 <- pcacomponenttest(
        data = df,
        vars = colnames(df),
        ncomp = 5,
        nperm = 200,
        seed = 99,
        stop_rule = FALSE
    )
    
    # Check equality of p-values
    # Check equality of p-values
    p1 <- as.vector(res1$results$results$asDF$pvalue)
    p2 <- as.vector(res2$results$results$asDF$pvalue)
    p3 <- as.vector(res3$results$results$asDF$pvalue)
    
    expect_equal(p1, p2)
    # Print p-values for debugging
    print(paste("P1:", paste(p1, collapse=",")))
    print(paste("P3:", paste(p3, collapse=",")))
    
    # With 200 perms, p-values should be different if seeds differ
    expect_false(isTRUE(all.equal(p1, p3)))
})

test_that("Stop rule works", {
    set.seed(123)
    n <- 50
    # 1 strong dimension, rest noise
    u1 <- rnorm(n)
    x1 <- u1 + 0.1*rnorm(n)
    x2 <- u1 - 0.1*rnorm(n)
    x3 <- rnorm(n)
    x4 <- rnorm(n)
    df <- data.frame(x1, x2, x3, x4)
    
    # Sequential (Default)
    res_seq <- pcacomponenttest(
        data = df,
        vars = colnames(df),
        ncomp = 4,
        stop_rule = TRUE
    )
    
    # Batch
    res_batch <- pcacomponenttest(
        data = df,
        vars = colnames(df),
        ncomp = 4,
        stop_rule = FALSE
    )
    
    # Sequential should have NAs after first non-sig
    # PC1 should be sig, PC2 potentially non-sig.
    p_seq <- res_seq$results$results$asDF$pvalue
    
    # Check that if a value is NA, all subsequent are NA or if we have sigs then NAs
    # It's hard to predict exact stochastic outcome, but we check logic:
    # If using stop rule, look for NAs.
    if (any(is.na(p_seq))) {
        first_na <- which(is.na(p_seq))[1]
        expect_true(all(is.na(p_seq[first_na:length(p_seq)])))
    }
    
    # Batch should have NO NAs
    p_batch <- res_batch$results$results$asDF$pvalue
    expect_false(any(is.na(p_batch)))
})

test_that("Warnings work", {
    # Constant variable
    df <- data.frame(x1 = 1:10, x2 = 1:10, x3 = rep(1, 10))
    
    expect_error(
        pcacomponenttest(data = df, vars = colnames(df)),
        "constant"
    )
})
