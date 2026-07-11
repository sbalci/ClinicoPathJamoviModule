# Test data for standardizedratio (SIR/SMR): age-stratified cohort with observed events,
# person-years, and reference-population rates (events per person-year).
set.seed(2026)
standardizedratio_test_data <- data.frame(
    age_group      = c("40-49", "50-59", "60-69", "70-79", "80+"),
    observed       = c(8, 15, 22, 18, 7),
    person_years   = c(12000, 15000, 11000, 6000, 2000),
    reference_rate = c(0.0004, 0.0009, 0.0018, 0.0030, 0.0035))
usethis::use_data(standardizedratio_test_data, overwrite = TRUE)
write.csv(standardizedratio_test_data, "data/standardizedratio_test_data.csv", row.names = FALSE)
