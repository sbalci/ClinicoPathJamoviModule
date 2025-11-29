
# Verification script for tumorgrowth
library(jmvcore)
library(R6)
library(ggplot2)
library(nlme)

# Source the function files
source("R/tumorgrowth.h.R")
source("R/tumorgrowth.b.R")

# Helper function to run analysis
run_analysis <- function(data, options) {
    analysis <- tumorgrowthClass$new(
        options = options,
        data = data
    )
    analysis$run()
    return(analysis)
}

print("Testing Exponential Model (nls)...")
set.seed(123)
t <- 0:10
V0 <- 10
k <- 0.1
size <- V0 * exp(k * t) + rnorm(length(t), 0, 1)
data_exp <- data.frame(time = t, size = size)

options_exp <- tumorgrowthOptions$new(
    time = "time",
    tumorSize = "size",
    growthModel = "exponential",
    modelApproach = "nls",
    doubleTime = TRUE,
    confidenceLevel = 95,
    predictionTime = 30,
    mcmcSamples = 5000,
    plotWidth = 600,
    plotHeight = 450,
    initialSize = 10,
    maxSize = 100
)

analysis_exp <- run_analysis(data_exp, options_exp)
print(analysis_exp$results$modelTable$asDF)
print(analysis_exp$results$doublingTimeTable$asDF)

print("Testing Gompertz Model (nls)...")
set.seed(123)
t <- 0:20
V0 <- 10
alpha <- 1
beta <- 0.1
size <- V0 * exp(alpha/beta * (1 - exp(-beta * t))) + rnorm(length(t), 0, 1)
data_gomp <- data.frame(time = t, size = size)

options_gomp <- tumorgrowthOptions$new(
    time = "time",
    tumorSize = "size",
    growthModel = "gompertz",
    modelApproach = "nls",
    confidenceLevel = 95,
    predictionTime = 30,
    mcmcSamples = 5000,
    plotWidth = 600,
    plotHeight = 450,
    initialSize = 10,
    maxSize = 100
)

analysis_gomp <- run_analysis(data_gomp, options_gomp)
print(analysis_gomp$results$modelTable$asDF)

print("Testing Logistic Model (nls)...")
set.seed(123)
t <- 0:20
K <- 100
r <- 0.2
t0 <- 10
size <- K / (1 + exp(-r * (t - t0))) + rnorm(length(t), 0, 2)
data_log <- data.frame(time = t, size = size)

options_log <- tumorgrowthOptions$new(
    time = "time",
    tumorSize = "size",
    growthModel = "logistic",
    modelApproach = "nls",
    confidenceLevel = 95,
    predictionTime = 30,
    mcmcSamples = 5000,
    plotWidth = 600,
    plotHeight = 450,
    initialSize = 10,
    maxSize = 100
)

analysis_log <- run_analysis(data_log, options_log)
print(analysis_log$results$modelTable$asDF)

print("Testing Linear Model (lm)...")
set.seed(123)
t <- 0:10
V0 <- 10
k <- 2
size <- V0 + k * t + rnorm(length(t), 0, 1)
data_lin <- data.frame(time = t, size = size)

options_lin <- tumorgrowthOptions$new(
    time = "time",
    tumorSize = "size",
    growthModel = "linear",
    modelApproach = "nls",
    confidenceLevel = 95,
    predictionTime = 30,
    mcmcSamples = 5000,
    plotWidth = 600,
    plotHeight = 450,
    initialSize = 10,
    maxSize = 100
)

analysis_lin <- run_analysis(data_lin, options_lin)
print(analysis_lin$results$modelTable$asDF)

print("Testing Mixed Effects (nlme)...")
set.seed(123)
t <- rep(0:10, 3)
patient <- rep(1:3, each = 11)
V0 <- 10
k <- 0.1
V0_rand <- V0 + rnorm(3, 0, 1)[patient]
size <- V0_rand * exp(k * t) + rnorm(length(t), 0, 0.5)
data_nlme <- data.frame(time = t, size = size, patient = factor(patient))

options_nlme <- tumorgrowthOptions$new(
    time = "time",
    tumorSize = "size",
    patientId = "patient",
    growthModel = "exponential",
    modelApproach = "nlme",
    confidenceLevel = 95,
    predictionTime = 30,
    mcmcSamples = 5000,
    plotWidth = 600,
    plotHeight = 450,
    initialSize = 10,
    maxSize = 100
)

analysis_nlme <- run_analysis(data_nlme, options_nlme)
print(analysis_nlme$results$modelTable$asDF)

print("Verification complete!")
