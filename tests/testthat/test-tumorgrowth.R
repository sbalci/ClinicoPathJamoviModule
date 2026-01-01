
test_that("tumorgrowth works with exponential model (nls)", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Create synthetic data
    set.seed(123)
    t <- 0:10
    V0 <- 10
    k <- 0.1
    size <- V0 * exp(k * t) + rnorm(length(t), 0, 1)
    data <- data.frame(time = t, size = size)
    
    # Run analysis
    results <- ClinicoPath::tumorgrowth(
        data = data,
        time = "time",
        tumorSize = "size",
        growthModel = "exponential",
        modelApproach = "nls"
    )
    
    # Check results
    expect_s3_class(results, "tumorgrowthResults")
    
    # Check model table
    table <- results$modelTable$asDF()
    expect_equal(nrow(table), 2) # V0 and k
    expect_true(any(grepl("V0", table$parameter)))
    expect_true(any(grepl("k", table$parameter)))
    
    # Check estimates are reasonable
    v0_est <- table$estimate[table$parameter == "V0"]
    k_est <- table$estimate[table$parameter == "k"]
    expect_equal(v0_est, 10, tolerance = 2)
    expect_equal(k_est, 0.1, tolerance = 0.05)
})

test_that("tumorgrowth works with gompertz model (nls)", {
    set.seed(123)
    t <- 0:20
    V0 <- 10
    alpha <- 1
    beta <- 0.1
    size <- V0 * exp(alpha/beta * (1 - exp(-beta * t))) + rnorm(length(t), 0, 1)
    data <- data.frame(time = t, size = size)
    
    results <- ClinicoPath::tumorgrowth(
        data = data,
        time = "time",
        tumorSize = "size",
        growthModel = "gompertz",
        modelApproach = "nls"
    )
    
    table <- results$modelTable$asDF()
    expect_equal(nrow(table), 3) # V0, alpha, beta
})

test_that("tumorgrowth works with logistic model (nls)", {
    set.seed(123)
    t <- 0:20
    K <- 100
    r <- 0.2
    t0 <- 10
    size <- K / (1 + exp(-r * (t - t0))) + rnorm(length(t), 0, 2)
    data <- data.frame(time = t, size = size)
    
    results <- ClinicoPath::tumorgrowth(
        data = data,
        time = "time",
        tumorSize = "size",
        growthModel = "logistic",
        modelApproach = "nls"
    )
    
    table <- results$modelTable$asDF()
    expect_equal(nrow(table), 3) # K, r, t0
})

test_that("tumorgrowth works with linear model (lm)", {
    set.seed(123)
    t <- 0:10
    V0 <- 10
    k <- 2
    size <- V0 + k * t + rnorm(length(t), 0, 1)
    data <- data.frame(time = t, size = size)
    
    results <- ClinicoPath::tumorgrowth(
        data = data,
        time = "time",
        tumorSize = "size",
        growthModel = "linear",
        modelApproach = "nls" # Should default to lm internally for linear
    )
    
    table <- results$modelTable$asDF()
    expect_equal(nrow(table), 2) # Intercept, time
})

test_that("tumorgrowth calculates doubling time", {
    set.seed(123)
    t <- 0:10
    V0 <- 10
    k <- 0.1
    size <- V0 * exp(k * t)
    data <- data.frame(time = t, size = size)
    
    results <- ClinicoPath::tumorgrowth(
        data = data,
        time = "time",
        tumorSize = "size",
        growthModel = "exponential",
        modelApproach = "nls",
        doubleTime = TRUE
    )
    
    dt_table <- results$doublingTimeTable$asDF()
    expect_equal(nrow(dt_table), 1)
    
    # Doubling time = ln(2)/k = 0.693/0.1 = 6.93
    expect_equal(dt_table$doubling_time[1], log(2)/0.1, tolerance = 0.1)
})

test_that("tumorgrowth handles nlme (mixed effects)", {
    skip_if_not_installed("nlme")
    
    # Create data for 3 patients
    set.seed(123)
    t <- rep(0:10, 3)
    patient <- rep(1:3, each = 11)
    V0 <- 10
    k <- 0.1
    # Random effect on V0
    V0_rand <- V0 + rnorm(3, 0, 1)[patient]
    size <- V0_rand * exp(k * t) + rnorm(length(t), 0, 0.5)
    data <- data.frame(time = t, size = size, patient = factor(patient))
    
    results <- ClinicoPath::tumorgrowth(
        data = data,
        time = "time",
        tumorSize = "size",
        patientId = "patient",
        growthModel = "exponential",
        modelApproach = "nlme"
    )
    
    expect_s3_class(results, "tumorgrowthResults")
    table <- results$modelTable$asDF()
    expect_true(nrow(table) > 0)
})

test_that("tumorgrowth handles errors gracefully", {
    # Insufficient data
    data <- data.frame(time = 1:5, size = 1:5)
    
    expect_error(
        ClinicoPath::tumorgrowth(
            data = data,
            time = "time",
            tumorSize = "size"
        ),
        "Insufficient data"
    )
})
