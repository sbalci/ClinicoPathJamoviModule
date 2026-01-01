
test_that("recist works for basic PR case", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    data <- data.frame(
        patient = rep("P1", 6),
        time = rep(c(0, 6, 12), each = 2),
        lesion = rep(c("L1", "L2"), 3),
        type = "target",
        diameter = c(20, 30, 15, 20, 10, 15),
        stringsAsFactors = FALSE
    )
    
    results <- recistClass$new(
        options = recistOptions$new(
            patientId = "patient",
            assessmentTime = "time",
            lesionId = "lesion",
            lesionType = "type",
            lesionDiameter = "diameter"
        ),
        data = data
    )
    results$run()
    
    table <- results$results$bestResponseTable$asDF
    expect_equal(as.character(table$bestResponse), "PR")
    expect_equal(as.character(table$confirmed), "Yes")
})

test_that("recist handles missing target lesions as NE", {
    data <- data.frame(
        patient = rep("P6", 4),
        time = rep(c(0, 6), each = 2),
        lesion = rep(c("L1", "L2"), 2),
        type = "target",
        diameter = c(20, 20, 20, NA),
        stringsAsFactors = FALSE
    )
    
    results <- recistClass$new(
        options = recistOptions$new(
            patientId = "patient",
            assessmentTime = "time",
            lesionId = "lesion",
            lesionType = "type",
            lesionDiameter = "diameter"
        ),
        data = data
    )
    results$run()
    
    resp_table <- results$results$responseTable$asDF
    expect_equal(as.character(resp_table$overallResponse[2]), "NE")
    
    best_table <- results$results$bestResponseTable$asDF
    expect_equal(as.character(best_table$bestResponse), "NE")
})

test_that("recist handles max lesions per organ correctly", {
    data <- data.frame(
        patient = rep("P5", 6),
        time = rep(0, 6),
        lesion = paste0("L", 1:6),
        type = "target",
        diameter = c(10, 20, 30, 40, 50, 60),
        organ = rep("Liver", 6),
        stringsAsFactors = FALSE
    )
    
    results <- recistClass$new(
        options = recistOptions$new(
            patientId = "patient",
            assessmentTime = "time",
            lesionId = "lesion",
            lesionType = "type",
            lesionDiameter = "diameter",
            organ = "organ",
            maxPerOrgan = 2
        ),
        data = data
    )
    results$run()
    
    sum_table <- results$results$targetSumTable$asDF
    # Should pick 60 and 50 -> 110
    expect_equal(sum_table$targetSum, 110)
})

test_that("recist handles new lesions as PD", {
    data <- data.frame(
        patient = rep("P3", 4),
        time = c(0, 0, 6, 6),
        lesion = c("L1", "L2", "L1", "New1"),
        type = c("target", "target", "target", "new"),
        diameter = c(20, 30, 20, NA),
        stringsAsFactors = FALSE
    )
    
    results <- recistClass$new(
        options = recistOptions$new(
            patientId = "patient",
            assessmentTime = "time",
            lesionId = "lesion",
            lesionType = "type",
            lesionDiameter = "diameter"
        ),
        data = data
    )
    results$run()
    
    resp_table <- results$results$responseTable$asDF
    expect_equal(as.character(resp_table$overallResponse[2]), "PD")
})
