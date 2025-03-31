# Set seed for reproducibility
set.seed(123)

# Create base patient data
n_patients <- 30

# Generate treatment start and end dates
base_data <- data.frame(
    PatientID = paste0("PT", sprintf("%03d", 1:n_patients)),
    StartDate = as.Date("2020-01-01") + sample(0:90, n_patients, replace = TRUE),
    ECOG = sample(0:2, n_patients, replace = TRUE, prob = c(0.5, 0.3, 0.2))
)

# Function to add months safely
add_months <- function(date, n) {
    lubridate::ymd(date) %m+% months(n)
}

# Generate milestones for each patient
milestone_data <- lapply(1:n_patients, function(i) {
    patient_start <- base_data$StartDate[i]

    # Create multiple milestones per patient
    milestones <- data.frame(
        PatientID = base_data$PatientID[i],

        # Response evaluation dates (every 2 months)
        MilestoneDate = c(
            add_months(patient_start, 2),  # First evaluation
            add_months(patient_start, 4),  # Second evaluation
            add_months(patient_start, 6),  # Third evaluation
            add_months(patient_start, 8)   # Fourth evaluation
        ),

        # Milestone types
        MilestoneType = c(
            sample(c("Response Evaluation", "Disease Progression",
                    "Adverse Event", "Treatment Change"),
                   4, replace = TRUE,
                   prob = c(0.4, 0.2, 0.2, 0.2))
        ),

        # Milestone details
        MilestoneDetail = NA
    )

    # Add milestone details based on type
    milestones$MilestoneDetail <- sapply(milestones$MilestoneType, function(type) {
        switch(type,
            "Response Evaluation" = sample(c("PR", "CR", "SD", "PD"), 1),
            "Disease Progression" = "PD Confirmed",
            "Adverse Event" = sample(c("Grade 2 Fatigue", "Grade 3 Neutropenia",
                                     "Grade 2 Nausea", "Grade 3 Anemia"), 1),
            "Treatment Change" = sample(c("Dose Reduction", "Regimen Change",
                                        "Treatment Hold", "Treatment Discontinuation"), 1)
        )
    })

    return(milestones)
})

# Combine all milestone data
milestone_data <- do.call(rbind, milestone_data)

# Calculate end dates based on milestones
end_dates <- sapply(base_data$PatientID, function(id) {
    pt_milestones <- milestone_data[milestone_data$PatientID == id,]
    last_date <- max(pt_milestones$MilestoneDate)

    # If progression occurred, use that as end date
    prog_dates <- pt_milestones$MilestoneDate[pt_milestones$MilestoneDetail %in%
                                             c("PD", "PD Confirmed")]
    if(length(prog_dates) > 0) {
        return(min(prog_dates))
    }

    return(last_date)
})

# Add end dates to base data
base_data$EndDate <- as.Date(end_dates)

# Add best response
base_data$BestResponse <- sapply(base_data$PatientID, function(id) {
    pt_milestones <- milestone_data[milestone_data$PatientID == id,]
    responses <- pt_milestones$MilestoneDetail[pt_milestones$MilestoneType == "Response Evaluation"]
    if("CR" %in% responses) return("CR")
    if("PR" %in% responses) return("PR")
    if("SD" %in% responses) return("SD")
    return("PD")
})


milestone_data2 <- base_data %>%
    dplyr::left_join(milestone_data, by = "PatientID")

# Print sample of the data
cat("\nBase patient data (first 5 rows):\n")
print(head(base_data, 5))

cat("\nMilestone events (first 10 rows):\n")
print(head(milestone_data, 10))

cat("\nMilestone events (first 10 rows):\n")
print(head(milestone_data2, 10))


write.csv(base_data, "./data/swimmer_plot_base_data.csv", row.names = FALSE)
write.csv(milestone_data, "./data/swimmer_plot_milestone_data.csv", row.names = FALSE)
