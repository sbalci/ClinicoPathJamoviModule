
test_that("stagemigration works with basic analysis", {
    set.seed(123)
    n <- 200
    old_stage <- sample(c("I", "II", "III"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
    old_stage <- factor(old_stage, levels = c("I", "II", "III"))
    
    new_stage <- character(n)
    for (i in 1:n) {
        if (old_stage[i] == "I") {
            new_stage[i] <- sample(c("IA", "IB"), 1, prob = c(0.7, 0.3))
        } else if (old_stage[i] == "II") {
            new_stage[i] <- sample(c("IB", "IIA", "IIB"), 1, prob = c(0.2, 0.5, 0.3))
        } else {
            new_stage[i] <- sample(c("IIB", "III"), 1, prob = c(0.2, 0.8))
        }
    }
    new_stage <- factor(new_stage, levels = c("IA", "IB", "IIA", "IIB", "III"))
    
    time <- rexp(n, rate = 0.1)
    status <- sample(c(0, 1), n, replace = TRUE)
    
    data <- data.frame(
        old_stage = old_stage,
        new_stage = new_stage,
        time = time,
        status = status
    )
    
    results <- ClinicoPath::stagemigration(
        data = data,
        oldStage = "old_stage",
        newStage = "new_stage",
        survivalTime = "time",
        event = "status",
        analysisType = "basic",
        showMigrationMatrix = TRUE
    )
    
    expect_s3_class(results, "stagemigrationResults")
    expect_true(!is.null(results$migrationMatrix))
})

test_that("stagemigration calculates C-index correctly", {
    set.seed(123)
    n <- 200
    old_stage <- factor(sample(c("I", "II"), n, replace = TRUE))
    new_stage <- old_stage # No change
    
    # Perfect separation
    time <- ifelse(old_stage == "I", rexp(n, 0.01), rexp(n, 0.1))
    status <- rep(1, n)
    
    data <- data.frame(old_stage, new_stage, time, status)
    
    results <- ClinicoPath::stagemigration(
        data = data,
        oldStage = "old_stage",
        newStage = "new_stage",
        survivalTime = "time",
        event = "status",
        analysisType = "standard",
        showStatisticalComparison = TRUE
    )
    
    table <- results$statisticalComparison$asDF()
    c_old <- as.numeric(table$value[table$metric == "c_old"])
    expect_true(c_old > 0.5)
})

test_that("stagemigration detects Will Rogers phenomenon", {
    set.seed(456)
    n_I <- 100
    n_II <- 100
    n_mig <- 50
    
    time_I <- rexp(n_I, 1/50)
    time_II <- rexp(n_II, 1/10)
    time_mig <- rexp(n_mig, 1/25)
    
    time <- c(time_I, time_II, time_mig)
    status <- sample(c(0, 1), length(time), replace = TRUE, prob = c(0.1, 0.9))
    
    old_stage <- factor(c(rep("I", n_I), rep("II", n_II), rep("I", n_mig)), levels = c("I", "II"))
    new_stage <- factor(c(rep("I", n_I), rep("II", n_II), rep("II", n_mig)), levels = c("I", "II"))
    
    data <- data.frame(old_stage, new_stage, time, status)
    
    results <- ClinicoPath::stagemigration(
        data = data,
        oldStage = "old_stage",
        newStage = "new_stage",
        survivalTime = "time",
        event = "status",
        analysisType = "standard",
        showWillRogersAnalysis = TRUE,
        advancedMigrationAnalysis = TRUE
    )
    
    table <- results$willRogersAnalysis$asDF()
    expect_true(any(grepl("Will Rogers", table$Will_Rogers_Evidence)))
})