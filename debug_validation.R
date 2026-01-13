library(ClinicoPath)
set.seed(123)
n <- 50
data <- data.frame(
  time_var = runif(n, 1, 100),
  status_var = sample(c(0, 1), n, replace = TRUE),
  risk_score = runif(n, 1, 100),
  covariates1 = sample(c('A', 'B'), n, replace = TRUE),
  covariates2 = sample(c('A', 'B'), n, replace = TRUE),
  covariates3 = sample(c('A', 'B'), n, replace = TRUE),
  stratification_var = sample(c('A', 'B'), n, replace = TRUE)
)

# Explicitly convert factors
data$covariates1 <- as.factor(data$covariates1)
data$covariates2 <- as.factor(data$covariates2)
data$covariates3 <- as.factor(data$covariates3)
data$stratification_var <- as.factor(data$stratification_var)

tryCatch({
  res <- survivalmodelvalidation(
    data = data,
    time_var = 'time_var',
    status_var = 'status_var',
    risk_score = 'risk_score',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    stratification_var = 'stratification_var',
    validation_type = 'internal_bootstrap',
    bootstrap_samples = 100
  )
  print("Success!")
  print(class(res))
}, error = function(e) {
  print(paste("Caught error:", e$message))
})
