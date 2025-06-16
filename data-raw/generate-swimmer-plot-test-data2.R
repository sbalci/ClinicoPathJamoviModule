# Generate and save the datasets for inclusion in the package

# Load necessary packages
library(usethis)

# Generate numeric dataset
set.seed(123)
patientTimelines <- generate_swimmerplot_data(
  n_patients = 30,
  missing_prop = 0.1,
  date_format = "numeric"
)

# Generate date dataset
set.seed(123)
patientTimelinesDates <- generate_swimmerplot_data(
  n_patients = 30,
  missing_prop = 0.1,
  date_format = "date",
  start_date = as.Date("2020-01-01")
)

# Format factors
patientTimelines$BestResponse <- factor(
  patientTimelines$BestResponse,
  levels = c("CR", "PR", "SD", "PD", "NE")
)

patientTimelinesDates$BestResponse <- factor(
  patientTimelinesDates$BestResponse,
  levels = c("CR", "PR", "SD", "PD", "NE")
)

patientTimelines$Risk <- factor(
  patientTimelines$Risk,
  levels = c("High", "Medium", "Low")
)

patientTimelinesDates$Risk <- factor(
  patientTimelinesDates$Risk,
  levels = c("High", "Medium", "Low")
)

write.csv(patientTimelines, "./data/patientTimelines.csv", row.names = FALSE)
write.csv(patientTimelinesDates, "./data/patientTimelinesDates.csv", row.names = FALSE)

# Save datasets
usethis::use_data(patientTimelines, overwrite = TRUE)
usethis::use_data(patientTimelinesDates, overwrite = TRUE)

# View datasets to verify
head(patientTimelines)
head(patientTimelinesDates)

# Show structure
str(patientTimelines)
str(patientTimelinesDates)

# Summary statistics
summary(patientTimelines$EndTime)
summary(patientTimelinesDates$EndDate)
