
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

test_that("jjoncoplot presets are applied (tumor profiling)", {
    set.seed(123)
    data <- data.frame(
        SampleID = paste0("S", 1:30),
        TP53 = sample(c(0, 1), 30, replace = TRUE),
        KRAS = sample(c(0, 1), 30, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- jjoncoplotClass$new(
        options = jjoncoplotOptions$new(
            sampleVar = "SampleID",
            geneVars = c("TP53", "KRAS"),
            clinicalPreset = "tumor_profiling"
        ),
        data = data
    )
    results$run()
    
    info <- results$results$plotInfo$asDF
    expect_true(any(info$parameter == "Plot Type" & info$value == "oncoplot"))
    # Preset should turn on mutation load
    expect_true(results$results$sampleSummary$visible)
})

test_that("jjoncoplot co-occurrence outputs include odds ratio and FDR", {
    set.seed(123)
    data <- data.frame(
        SampleID = paste0("S", 1:40),
        TP53 = sample(c(0, 1), 40, replace = TRUE),
        KRAS = sample(c(0, 1), 40, replace = TRUE),
        PIK3CA = sample(c(0, 1), 40, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- jjoncoplotClass$new(
        options = jjoncoplotOptions$new(
            sampleVar = "SampleID",
            geneVars = c("TP53", "KRAS", "PIK3CA"),
            plotType = "cooccurrence"
        ),
        data = data
    )
    results$run()
    
    cooc <- results$results$cooccurrence$asDF
    expect_true(all(c("odds_ratio", "fdr") %in% names(cooc)))
})

test_that("jjoncoplot clips non-binary mutation inputs safely", {
    data <- data.frame(
        SampleID = paste0("S", 1:10),
        TP53 = c(0, 1, 2, -1, NA, 1, 0, 3, 0, 1),
        stringsAsFactors = FALSE
    )
    
    results <- jjoncoplotClass$new(
        options = jjoncoplotOptions$new(
            sampleVar = "SampleID",
            geneVars = c("TP53"),
            plotType = "frequency"
        ),
        data = data
    )
    results$run()
    
    mut <- results$results$mutationSummary$asDF
    expect_true(mut$mutated_samples <= 10)
})
