
devtools::load_all()
library(survival)

set.seed(42)
n <- 200
test_data <- data.frame(
    survival_time = rexp(n, rate = 0.05) * 12,
    event_occurred = rbinom(n, 1, 0.6),
    age = rnorm(n, 65, 12),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE))
)

print("Running coxdiagnostics...")
tryCatch({
    results <- ClinicoPath::coxdiagnostics(
        data = test_data,
        time = "survival_time",
        event = "event_occurred",
        covariates = c("age", "sex")
    )
    print("Run completed.")
    print(results)
}, warning = function(w) {
    print(paste("WARNING:", w$message))
}, error = function(e) {
    print(paste("ERROR:", e$message))
})
