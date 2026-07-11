# Patient-referral flow between care levels, as an edge list (from / to / count).
set.seed(7)
circos_test_data <- data.frame(
    from_site  = c("Primary", "Primary", "Regional", "Regional", "Tertiary", "Primary", "Regional"),
    to_site    = c("Regional", "Tertiary", "Tertiary", "Primary", "Regional", "Tertiary", "Regional"),
    n_patients = c(120, 45, 80, 15, 20, 30, 25))
usethis::use_data(circos_test_data, overwrite = TRUE)
write.csv(circos_test_data, "data/circos_test_data.csv", row.names = FALSE)
