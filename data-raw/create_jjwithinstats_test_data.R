# Create test data for jjwithinstats function
# This script generates various datasets to test within-subjects repeated measures functionality

library(usethis)

# Basic pre-post treatment data
set.seed(123)
jjwithinstats_prepost <- data.frame(
  subject_id = 1:30,
  pre_treatment = rnorm(30, mean = 85, sd = 12),
  post_treatment = rnorm(30, mean = 75, sd = 10)
)
# Add some correlation between pre and post
jjwithinstats_prepost$post_treatment <- jjwithinstats_prepost$pre_treatment - 
  rnorm(30, mean = 8, sd = 5)

# Therapy effectiveness study (3 time points)
set.seed(456)
jjwithinstats_therapy <- data.frame(
  patient_id = 1:25,
  baseline_anxiety = rnorm(25, mean = 65, sd = 15),
  week_6_anxiety = rnorm(25, mean = 50, sd = 12),
  week_12_anxiety = rnorm(25, mean = 35, sd = 10)
)
# Add realistic decreasing trend
for(i in 1:25) {
  jjwithinstats_therapy$week_6_anxiety[i] <- jjwithinstats_therapy$baseline_anxiety[i] - 
    runif(1, 8, 18)
  jjwithinstats_therapy$week_12_anxiety[i] <- jjwithinstats_therapy$week_6_anxiety[i] - 
    runif(1, 10, 20)
}

# Clinical trial with 4 time points
set.seed(789)
jjwithinstats_clinical <- data.frame(
  participant_id = 1:40,
  baseline_score = rnorm(40, mean = 100, sd = 20),
  month_3_score = rnorm(40, mean = 85, sd = 18),
  month_6_score = rnorm(40, mean = 70, sd = 16),
  month_12_score = rnorm(40, mean = 55, sd = 15)
)
# Add realistic progressive improvement
for(i in 1:40) {
  decline_factor <- runif(1, 0.8, 0.95)
  jjwithinstats_clinical$month_3_score[i] <- jjwithinstats_clinical$baseline_score[i] * decline_factor
  jjwithinstats_clinical$month_6_score[i] <- jjwithinstats_clinical$month_3_score[i] * decline_factor
  jjwithinstats_clinical$month_12_score[i] <- jjwithinstats_clinical$month_6_score[i] * decline_factor
}

# Pain management study
set.seed(1001)
jjwithinstats_pain <- data.frame(
  patient_id = 1:35,
  pain_baseline = sample(6:10, 35, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, 0.1)),
  pain_week_2 = sample(4:8, 35, replace = TRUE, prob = c(0.15, 0.25, 0.3, 0.25, 0.05)),
  pain_week_4 = sample(2:6, 35, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.15, 0.05)),
  pain_week_8 = sample(1:5, 35, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.1, 0.05))
)

# Educational assessment repeated measures
set.seed(2002)
jjwithinstats_education <- data.frame(
  student_id = 1:50,
  pretest_score = rnorm(50, mean = 60, sd = 15),
  midterm_score = rnorm(50, mean = 70, sd = 12),
  final_score = rnorm(50, mean = 80, sd = 10)
)
# Add learning progression
for(i in 1:50) {
  improvement <- runif(1, 5, 15)
  jjwithinstats_education$midterm_score[i] <- jjwithinstats_education$pretest_score[i] + improvement
  jjwithinstats_education$final_score[i] <- jjwithinstats_education$midterm_score[i] + improvement
}
# Ensure scores don't exceed 100
jjwithinstats_education$midterm_score <- pmin(jjwithinstats_education$midterm_score, 100)
jjwithinstats_education$final_score <- pmin(jjwithinstats_education$final_score, 100)

# Exercise performance over time
set.seed(3003)
jjwithinstats_exercise <- data.frame(
  athlete_id = 1:28,
  week_0_performance = rnorm(28, mean = 120, sd = 25),
  week_4_performance = rnorm(28, mean = 135, sd = 22),
  week_8_performance = rnorm(28, mean = 150, sd = 20),
  week_12_performance = rnorm(28, mean = 165, sd = 18)
)
# Add realistic training progression
for(i in 1:28) {
  base_performance <- jjwithinstats_exercise$week_0_performance[i]
  improvement_rate <- runif(1, 1.08, 1.15)
  jjwithinstats_exercise$week_4_performance[i] <- base_performance * improvement_rate
  jjwithinstats_exercise$week_8_performance[i] <- jjwithinstats_exercise$week_4_performance[i] * improvement_rate
  jjwithinstats_exercise$week_12_performance[i] <- jjwithinstats_exercise$week_8_performance[i] * improvement_rate
}

# Blood pressure medication study
set.seed(4004)
jjwithinstats_bloodpressure <- data.frame(
  subject_id = 1:32,
  systolic_baseline = rnorm(32, mean = 160, sd = 20),
  systolic_week_4 = rnorm(32, mean = 145, sd = 18),
  systolic_week_8 = rnorm(32, mean = 130, sd = 15)
)
# Add realistic BP reduction
for(i in 1:32) {
  reduction_week4 <- runif(1, 10, 25)
  reduction_week8 <- runif(1, 15, 35)
  jjwithinstats_bloodpressure$systolic_week_4[i] <- jjwithinstats_bloodpressure$systolic_baseline[i] - reduction_week4
  jjwithinstats_bloodpressure$systolic_week_8[i] <- jjwithinstats_bloodpressure$systolic_baseline[i] - reduction_week8
}
# Ensure realistic BP values
jjwithinstats_bloodpressure$systolic_week_4 <- pmax(jjwithinstats_bloodpressure$systolic_week_4, 100)
jjwithinstats_bloodpressure$systolic_week_8 <- pmax(jjwithinstats_bloodpressure$systolic_week_8, 90)

# Cognitive training study
set.seed(5005)
jjwithinstats_cognitive <- data.frame(
  participant_id = 1:45,
  memory_pre = rnorm(45, mean = 75, sd = 18),
  memory_post_1month = rnorm(45, mean = 85, sd = 16),
  memory_post_3month = rnorm(45, mean = 90, sd = 14),
  memory_post_6month = rnorm(45, mean = 88, sd = 15)  # Some decline over time
)
# Add realistic cognitive improvement with some decline
for(i in 1:45) {
  initial_improvement <- runif(1, 8, 18)
  continued_improvement <- runif(1, 3, 10)
  maintenance_decline <- runif(1, 0, 5)
  
  jjwithinstats_cognitive$memory_post_1month[i] <- jjwithinstats_cognitive$memory_pre[i] + initial_improvement
  jjwithinstats_cognitive$memory_post_3month[i] <- jjwithinstats_cognitive$memory_post_1month[i] + continued_improvement
  jjwithinstats_cognitive$memory_post_6month[i] <- jjwithinstats_cognitive$memory_post_3month[i] - maintenance_decline
}

# Drug dosage optimization study
set.seed(6006)
jjwithinstats_dosage <- data.frame(
  patient_id = 1:20,
  efficacy_5mg = rnorm(20, mean = 40, sd = 12),
  efficacy_10mg = rnorm(20, mean = 65, sd = 15),
  efficacy_15mg = rnorm(20, mean = 80, sd = 18),
  efficacy_20mg = rnorm(20, mean = 85, sd = 20)  # Diminishing returns
)
# Add dose-response relationship
for(i in 1:20) {
  base_response <- jjwithinstats_dosage$efficacy_5mg[i]
  jjwithinstats_dosage$efficacy_10mg[i] <- base_response + runif(1, 15, 30)
  jjwithinstats_dosage$efficacy_15mg[i] <- jjwithinstats_dosage$efficacy_10mg[i] + runif(1, 10, 20)
  jjwithinstats_dosage$efficacy_20mg[i] <- jjwithinstats_dosage$efficacy_15mg[i] + runif(1, 2, 8)  # Diminishing returns
}
# Ensure realistic efficacy values (0-100)
jjwithinstats_dosage[, 2:5] <- lapply(jjwithinstats_dosage[, 2:5], function(x) pmax(pmin(x, 100), 0))

# Recovery time study with missing values
set.seed(7007)
jjwithinstats_recovery <- data.frame(
  patient_id = 1:30,
  recovery_day_1 = rnorm(30, mean = 80, sd = 15),
  recovery_day_7 = rnorm(30, mean = 60, sd = 12),
  recovery_day_14 = rnorm(30, mean = 40, sd = 10),
  recovery_day_21 = rnorm(30, mean = 20, sd = 8)
)
# Add realistic recovery progression
for(i in 1:30) {
  daily_improvement <- runif(1, 2, 4)
  jjwithinstats_recovery$recovery_day_7[i] <- jjwithinstats_recovery$recovery_day_1[i] - (daily_improvement * 6)
  jjwithinstats_recovery$recovery_day_14[i] <- jjwithinstats_recovery$recovery_day_7[i] - (daily_improvement * 7)
  jjwithinstats_recovery$recovery_day_21[i] <- jjwithinstats_recovery$recovery_day_14[i] - (daily_improvement * 7)
}
# Ensure non-negative values
jjwithinstats_recovery[, 2:5] <- lapply(jjwithinstats_recovery[, 2:5], function(x) pmax(x, 0))
# Add some missing values
missing_indices <- sample(1:30, 5)
jjwithinstats_recovery$recovery_day_7[missing_indices[1:2]] <- NA
jjwithinstats_recovery$recovery_day_14[missing_indices[3:4]] <- NA
jjwithinstats_recovery$recovery_day_21[missing_indices[5]] <- NA

# Performance test data (larger dataset)
set.seed(8008)
jjwithinstats_performance <- data.frame(
  subject_id = 1:100,
  baseline_metric = rnorm(100, mean = 500, sd = 100),
  followup_1_metric = rnorm(100, mean = 520, sd = 95),
  followup_2_metric = rnorm(100, mean = 540, sd = 90)
)
# Add realistic performance changes
for(i in 1:100) {
  improvement_1 <- rnorm(1, mean = 20, sd = 15)
  improvement_2 <- rnorm(1, mean = 15, sd = 12)
  jjwithinstats_performance$followup_1_metric[i] <- jjwithinstats_performance$baseline_metric[i] + improvement_1
  jjwithinstats_performance$followup_2_metric[i] <- jjwithinstats_performance$followup_1_metric[i] + improvement_2
}

# Edge cases test data
set.seed(9009)
jjwithinstats_edge_cases <- data.frame(
  case_id = 1:15,
  extreme_low = c(rep(0.001, 5), rep(0.01, 5), rep(0.1, 5)),
  extreme_high = c(rep(1000000, 5), rep(999999, 5), rep(1000001, 5)),
  identical_values = rep(50, 15),
  minimal_variance = rnorm(15, mean = 100, sd = 0.1)
)

# Save all datasets
usethis::use_data(jjwithinstats_prepost, overwrite = TRUE)
usethis::use_data(jjwithinstats_therapy, overwrite = TRUE)
usethis::use_data(jjwithinstats_clinical, overwrite = TRUE)
usethis::use_data(jjwithinstats_pain, overwrite = TRUE)
usethis::use_data(jjwithinstats_education, overwrite = TRUE)
usethis::use_data(jjwithinstats_exercise, overwrite = TRUE)
usethis::use_data(jjwithinstats_bloodpressure, overwrite = TRUE)
usethis::use_data(jjwithinstats_cognitive, overwrite = TRUE)
usethis::use_data(jjwithinstats_dosage, overwrite = TRUE)
usethis::use_data(jjwithinstats_recovery, overwrite = TRUE)
usethis::use_data(jjwithinstats_performance, overwrite = TRUE)
usethis::use_data(jjwithinstats_edge_cases, overwrite = TRUE)

# Print summary information
cat("Test datasets created for jjwithinstats:\\n")
cat("1. jjwithinstats_prepost: Basic pre-post treatment data (n=30)\\n")
cat("2. jjwithinstats_therapy: Therapy effectiveness study, 3 time points (n=25)\\n")
cat("3. jjwithinstats_clinical: Clinical trial with 4 time points (n=40)\\n")
cat("4. jjwithinstats_pain: Pain management study, ordinal data (n=35)\\n")
cat("5. jjwithinstats_education: Educational assessment progression (n=50)\\n")
cat("6. jjwithinstats_exercise: Exercise performance improvement (n=28)\\n")
cat("7. jjwithinstats_bloodpressure: Blood pressure medication study (n=32)\\n")
cat("8. jjwithinstats_cognitive: Cognitive training with decline (n=45)\\n")
cat("9. jjwithinstats_dosage: Drug dosage optimization, 4 levels (n=20)\\n")
cat("10. jjwithinstats_recovery: Recovery study with missing values (n=30)\\n")
cat("11. jjwithinstats_performance: Large dataset for performance testing (n=100)\\n")
cat("12. jjwithinstats_edge_cases: Edge cases with extreme values (n=15)\\n")

# Print sample from each dataset
cat("\\n=== Sample Data ===\\n")
cat("\\nPre-Post Treatment (first 5 rows):\\n")
print(head(jjwithinstats_prepost, 5))

cat("\\nClinical Trial (first 5 rows):\\n")
print(head(jjwithinstats_clinical, 5))

cat("\\nPain Management (first 5 rows):\\n")
print(head(jjwithinstats_pain, 5))

cat("\\nEdge Cases (first 5 rows):\\n")
print(head(jjwithinstats_edge_cases, 5))