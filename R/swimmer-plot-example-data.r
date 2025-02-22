# Generate two example datasets - one with raw numeric times and one with dates
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Function to generate random events with realistic progression
generate_events <- function(n, duration) {
  events <- rep(NA, n)
  for(i in 1:n) {
    if(duration[i] < 3) {
      # Early progression more likely
      events[i] <- sample(c("PD", "SD"), 1, prob = c(0.8, 0.2))
    } else if(duration[i] > 24) {
      # Long duration more likely to be response
      events[i] <- sample(c("CR", "PR"), 1, prob = c(0.6, 0.4))
    } else {
      # Medium duration mix of outcomes
      events[i] <- sample(c("CR", "PR", "SD", "PD"), 1, prob = c(0.2, 0.3, 0.3, 0.2))
    }
  }
  return(events)
}

# 1. Raw numeric time dataset
n_patients <- 40

raw_data <- data.frame(
  PatientID = paste0("PT", sprintf("%03d", 1:n_patients)),
  StartTime = sample(0:6, n_patients, replace = TRUE),  # Some delayed starts
  Duration = round(rlnorm(n_patients, log(12), 0.7)), # Log-normal distribution for duration
  Age = round(runif(n_patients, 35, 85)),
  Weight = round(rnorm(n_patients, 70, 15)),
  ECOG = sample(0:2, n_patients, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  PriorTherapy = sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.7, 0.3))
)

raw_data$EndTime <- raw_data$StartTime + raw_data$Duration
raw_data$BestResponse <- generate_events(n_patients, raw_data$Duration)

# 2. Date/time dataset
start_date <- ymd("2020-01-01")

date_data <- data.frame(
  PatientID = paste0("PT", sprintf("%03d", 1:n_patients)),
  StartDate = start_date + days(sample(0:90, n_patients, replace = TRUE)),  # Random enrollment over 3 months
  Duration = round(rlnorm(n_patients, log(12), 0.7) * 30.44), # Convert to approximate days
  Age = round(runif(n_patients, 35, 85)),
  Weight = round(rnorm(n_patients, 70, 15)),
  ECOG = sample(0:2, n_patients, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  PriorTherapy = sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.7, 0.3))
)

date_data$EndDate <- date_data$StartDate + days(date_data$Duration)
date_data$BestResponse <- generate_events(n_patients, date_data$Duration/30.44)  # Convert back to months for event generation

# 3. Create different date formats
date_formats <- date_data
date_formats$StartDate_YMD <- format(date_formats$StartDate, "%Y-%m-%d")
date_formats$StartDate_DMY <- format(date_formats$StartDate, "%d-%m-%Y")
date_formats$StartDate_MDY <- format(date_formats$StartDate, "%m-%d-%Y")
date_formats$EndDate_YMD <- format(date_formats$EndDate, "%Y-%m-%d")
date_formats$EndDate_DMY <- format(date_formats$EndDate, "%d-%m-%Y")
date_formats$EndDate_MDY <- format(date_formats$EndDate, "%m-%d-%Y")

# Print sample of each dataset
cat("\nRaw numeric time dataset (first 5 rows):\n")
print(head(raw_data, 5))

cat("\nDate/time dataset (first 5 rows):\n")
print(head(date_formats[c("PatientID", "StartDate_YMD", "EndDate_YMD", "BestResponse", "Age", "ECOG")], 5))

# Summary statistics
cat("\nSummary statistics for raw data durations (months):\n")
print(summary(raw_data$Duration))

cat("\nSummary statistics for date data durations (days):\n")
print(summary(date_data$Duration))

# Response distribution
cat("\nResponse distribution in raw data:\n")
print(table(raw_data$BestResponse))

cat("\nResponse distribution in date data:\n")
print(table(date_data$BestResponse))

save(raw_data, file = "./data/swimmer_data_raw.rda")
write.csv(raw_data, file = "./data/swimmer_data_raw.csv")

save(date_formats, file = "./data/swimmer_data_date_formats.rda")
write.csv(date_formats, file = "./data/swimmer_data_date_formats.csv")
