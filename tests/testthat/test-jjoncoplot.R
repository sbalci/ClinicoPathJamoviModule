
test_that("jjoncoplot works for basic oncoplot", {
    set.seed(123)
    data <- data.frame(
        SampleID = paste0("S", 1:20),
        TP53 = sample(c(0, 1), 20, replace = TRUE),
        KRAS = sample(c(0, 1), 20, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- jjoncoplotClass$new(
        options = jjoncoplotOptions$new(
            sampleVar = "SampleID",
            geneVars = c("TP53", "KRAS"),
            plotType = "oncoplot"
        ),
        data = data
    )
    results$run()
    
    expect_true(!is.null(results$results$main))
    expect_equal(results$results$mutationSummary$rowCount, 2)
})

test_that("jjoncoplot works for frequency plot", {
    set.seed(123)
    data <- data.frame(
        SampleID = paste0("S", 1:20),
        TP53 = sample(c(0, 1), 20, replace = TRUE),
        KRAS = sample(c(0, 1), 20, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- jjoncoplotClass$new(
        options = jjoncoplotOptions$new(
            sampleVar = "SampleID",
            geneVars = c("TP53", "KRAS"),
            plotType = "frequency"
        ),
        data = data
    )
    results$run()
    
    expect_true(!is.null(results$results$main))
})

test_that("jjoncoplot works for co-occurrence plot", {
    set.seed(123)
    data <- data.frame(
        SampleID = paste0("S", 1:20),
        TP53 = sample(c(0, 1), 20, replace = TRUE),
        KRAS = sample(c(0, 1), 20, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- jjoncoplotClass$new(
        options = jjoncoplotOptions$new(
            sampleVar = "SampleID",
            geneVars = c("TP53", "KRAS"),
            plotType = "cooccurrence"
        ),
        data = data
    )
    results$run()
    
    expect_true(!is.null(results$results$main))
    expect_true(results$results$cooccurrence$rowCount > 0)
})

test_that("jjoncoplot handles clinical annotations", {
    set.seed(123)
    data <- data.frame(
        SampleID = paste0("S", 1:20),
        TP53 = sample(c(0, 1), 20, replace = TRUE),
        KRAS = sample(c(0, 1), 20, replace = TRUE),
        Stage = sample(c("I", "II"), 20, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- jjoncoplotClass$new(
        options = jjoncoplotOptions$new(
            sampleVar = "SampleID",
            geneVars = c("TP53", "KRAS"),
            clinicalVars = c("Stage"),
            showClinicalAnnotation = TRUE,
            plotType = "oncoplot"
        ),
        data = data
    )
    results$run()
    
    expect_true(!is.null(results$results$main))
    expect_true(results$results$clinicalSummary$visible)
})

test_that("jjoncoplot handles missing data gracefully", {
    data <- data.frame(
        SampleID = character(0),
        TP53 = numeric(0)
    )
    
    results <- jjoncoplotClass$new(
        options = jjoncoplotOptions$new(
            sampleVar = "SampleID",
            geneVars = c("TP53"),
            plotType = "oncoplot"
        ),
        data = data
    )
    results$run()
    
    # Should not crash, just return empty results or guidance
    expect_true(results$results$setupGuidance$visible)
})
