# Synthetic RCT-style cohort for subgroup forest analysis: a survival outcome with
# a treatment effect that varies across three clinically meaningful subgroups
# (a genuine treatment x biomarker interaction is built into the high-biomarker arm).
set.seed(2026)
n <- 500
treatment <- factor(sample(c("Control", "Treatment"), n, TRUE),
                    levels = c("Control", "Treatment"))
sex   <- factor(sample(c("Female", "Male"), n, TRUE))
age_group <- factor(sample(c("<65", ">=65"), n, TRUE, c(0.55, 0.45)))
biomarker <- factor(sample(c("Low", "High"), n, TRUE, c(0.6, 0.4)))

# Baseline hazard modifiers; Treatment helps overall but much more in biomarker-High
lp <- 0.4 * (treatment == "Treatment") +
      0.25 * (age_group == ">=65") +
      0.15 * (sex == "Male") +
     -0.9 * (treatment == "Treatment") * (biomarker == "High")
time  <- rexp(n, rate = 0.05 * exp(lp))
cens  <- rexp(n, rate = 0.03)
os_time   <- pmin(time, cens)
os_event  <- as.integer(time <= cens)

# A binary outcome (response) and a continuous outcome (biomarker change) too,
# so the analysis can be exercised in all three outcome modes.
response <- rbinom(n, 1, plogis(-0.4 + 0.5 * (treatment == "Treatment") +
                                0.6 * (biomarker == "High")))
change   <- 5 - 3 * (treatment == "Treatment") + rnorm(n, 0, 4)

subgroupforest_test_data <- data.frame(
    os_time = round(os_time, 2),
    os_event = os_event,
    response = response,
    change = round(change, 2),
    treatment = treatment,
    sex = sex,
    age_group = age_group,
    biomarker = biomarker)

usethis::use_data(subgroupforest_test_data, overwrite = TRUE)
write.csv(subgroupforest_test_data, "data/subgroupforest_test_data.csv", row.names = FALSE)
