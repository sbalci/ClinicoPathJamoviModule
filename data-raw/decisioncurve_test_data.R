# Data generation script for decisioncurve (Decision Curve Analysis)
# Generates realistic binary clinical outcome and predicted probabilities

set.seed(42)

n <- 300

# True underlying status based on logistic risk model
age <- round(rnorm(n, mean = 60, sd = 10))
biomarker <- rnorm(n, mean = 2, sd = 1.2)
grade <- sample(c("Low", "High"), size = n, replace = TRUE, prob = c(0.6, 0.4))

linear_pred <- -2.5 + 0.03 * age + 0.8 * biomarker + 0.6 * (grade == "High")
prob_true <- 1 / (1 + exp(-linear_pred))
outcome <- factor(rbinom(n, size = 1, prob = prob_true), levels = c(0, 1), labels = c("No", "Yes"))

# Models with different discrimination abilities
model_basic <- plogis(-2.0 + 0.03 * age)
model_biomarker <- plogis(-2.2 + 0.03 * age + 0.7 * biomarker)
model_full <- plogis(linear_pred + rnorm(n, 0, 0.2)) # Well-calibrated full model

decisioncurve_test_data <- data.frame(
  id = seq_len(n),
  outcome = outcome,
  age = age,
  biomarker = biomarker,
  grade = grade,
  model_basic = model_basic,
  model_biomarker = model_biomarker,
  model_full = model_full
)

save(decisioncurve_test_data, file = "data/decisioncurve_test_data.rda", compress = "xz")
write.csv(decisioncurve_test_data, file = "inst/extdata/decisioncurve_test_data.csv", row.names = FALSE)

message("decisioncurve test data generated successfully.")
