# Test swimmerplot with sample data
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(ClinicoPathDescriptives)

# Read sample data
data <- read.csv("data/swimmerplot_sample.csv", stringsAsFactors = FALSE)

# View data structure
str(data)
head(data)

# Basic swimmerplot with minimal configuration
ClinicoPathDescriptives::swimmerplot(
    data = data,
    patientID = "PatientID",
    startTime = "StartTime", 
    endTime = "EndTime"
)

# Full swimmerplot with all features
ClinicoPathDescriptives::swimmerplot(
    data = data,
    patientID = "PatientID",
    startTime = "StartTime", 
    endTime = "EndTime",
    responseVar = "Response",
    milestone1Name = "Surgery",
    milestone1Date = "Surgery",
    milestone2Name = "Treatment Start", 
    milestone2Date = "TreatmentStart",
    milestone3Name = "Response Assessment",
    milestone3Date = "ResponseAssessment",
    milestone4Name = "Progression",
    milestone4Date = "Progression",
    showEventMarkers = TRUE,
    eventVar = "EventType",
    eventTimeVar = "EventTime",
    showInterpretation = TRUE,
    personTimeAnalysis = TRUE,
    responseAnalysis = TRUE,
    sortOrder = "response"
)
