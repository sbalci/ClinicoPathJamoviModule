# IHC Threshold Example Data - Ki-67 staining
set.seed(42)

# Simulate 6 sample areas tested at 3 threshold levels
areas <- rep(paste0("Area_", 1:6), each = 3)
thresholds <- rep(c("Low", "Medium", "High"), times = 6)

# Simulate cell counts
# Threshold "Medium" should be optimal with most consistent ratios
data_list <- list()
for (i in seq_along(areas)) {
  area <- areas[i]
  threshold <- thresholds[i]

  if (threshold == "Low") {
    # Too low - many false positives, high variance
    positive <- round(rnorm(1, mean = 150, sd = 40))
    negative <- round(rnorm(1, mean = 50, sd = 20))
  } else if (threshold == "Medium") {
    # Optimal - consistent ratios across areas
    positive <- round(rnorm(1, mean = 80, sd = 10))
    negative <- round(rnorm(1, mean = 120, sd = 10))
  } else {
    # Too high - many false negatives, inconsistent
    positive <- round(rnorm(1, mean = 30, sd = 25))
    negative <- round(rnorm(1, mean = 170, sd = 30))
  }

  data_list[[i]] <- data.frame(
    sample_area = area,
    threshold_level = threshold,
    positive_cells = max(0, positive),
    negative_cells = max(0, negative),
    stringsAsFactors = FALSE
  )
}

ihc_ki67_data <- do.call(rbind, data_list)

# Save
save(ihc_ki67_data, file = "data/ihc_ki67_data.rda", compress = "bzip2")
write.csv(ihc_ki67_data, file = "data/ihc_ki67_data.csv", row.names = FALSE)
cat("✅ IHC threshold data created\n")
