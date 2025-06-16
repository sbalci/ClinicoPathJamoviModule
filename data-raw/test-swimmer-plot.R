# Testing the swimmer plot function with generated test data

# Generate numeric data
set.seed(123)
numeric_data <- generate_swimmerplot_data(
  n_patients = 15, 
  missing_prop = 0.1,
  date_format = "numeric"
)

# Look at the structure of the data
str(numeric_data)
head(numeric_data)

# Basic swimmer plot with numeric data
swimmerplot(
  data = numeric_data,
  patientID = "PatientID",
  start = "StartTime",
  end = "EndTime",
  event = "BestResponse"
)

# Add milestone information
swimmerplot(
  data = numeric_data,
  patientID = "PatientID",
  start = "StartTime",
  end = "EndTime",
  event = "BestResponse",
  milestone1Name = "Surgery",
  milestone1Date = "Surgery",
  milestone2Name = "Treatment Start",
  milestone2Date = "TreatmentStart",
  milestone3Name = "Response",
  milestone3Date = "ResponseAssessment",
  milestone4Name = "Progression",
  milestone4Date = "Progression",
  milestone5Name = "Death",
  milestone5Date = "Death"
)

# Add reference lines and sorting
swimmerplot(
  data = numeric_data,
  patientID = "PatientID",
  start = "StartTime",
  end = "EndTime",
  event = "BestResponse",
  milestone1Name = "Surgery",
  milestone1Date = "Surgery",
  milestone2Name = "Treatment Start",
  milestone2Date = "TreatmentStart",
  milestone3Name = "Response",
  milestone3Date = "ResponseAssessment",
  sortVariable = "ResponseDuration",
  referenceLines = "protocol"
)

# Generate date-based data
date_data <- generate_swimmerplot_data(
  n_patients = 15, 
  missing_prop = 0.1,
  date_format = "date",
  start_date = as.Date("2020-01-01")
)

# Look at the structure of date data
str(date_data)
head(date_data)

# Test with date data and relative time
swimmerplot(
  data = date_data,
  patientID = "PatientID",
  start = "StartDate",
  end = "EndDate",
  event = "BestResponse",
  timetype = "datetime",
  timetypedata = "ymd",
  timetypeoutput = "months",
  startType = "relative",
  milestone1Name = "Surgery",
  milestone1Date = "Surgery",
  milestone2Name = "Treatment Start",
  milestone2Date = "TreatmentStart",
  milestone3Name = "Response",
  milestone3Date = "ResponseAssessment"
)

# Test with date data and absolute time
swimmerplot(
  data = date_data,
  patientID = "PatientID",
  start = "StartDate",
  end = "EndDate",
  event = "BestResponse",
  timetype = "datetime",
  timetypedata = "ymd",
  timetypeoutput = "months",
  startType = "absolute",
  milestone1Name = "Surgery",
  milestone1Date = "Surgery",
  milestone2Name = "Treatment Start",
  milestone2Date = "TreatmentStart",
  milestone3Name = "Response",
  milestone3Date = "ResponseAssessment",
  referenceLines = "median"
)

# Test with custom bar height and reference line
swimmerplot(
  data = numeric_data,
  patientID = "PatientID",
  start = "StartTime",
  end = "EndTime",
  event = "BestResponse",
  barHeight = 5,
  referenceLines = "custom",
  customReferenceTime = 6
)
