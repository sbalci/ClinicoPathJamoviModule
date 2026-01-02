# Load the necessary libraries
library(jmv)
library(jmv)
if (requireNamespace("devtools")) devtools::load_all(".") else devtools::load_all()

# Create a simple test data frame
stage_migration_test_data <- data.frame(
  OldStage = factor(c("I", "II", "I", "III", "II", "I", "III", "II")),
  NewStage = factor(c("II", "II", "I", "IV", "III", "II", "III", "II")),
  SurvivalTime = c(10, 20, 15, 30, 25, 12, 35, 22),
  Event = c(1, 0, 1, 1, 0, 1, 0, 1)
)

# Run the stage migration analysis with a minimal set of options
results <- ClinicoPath::stagemigration(
    data = stage_migration_test_data,
    oldStage = "OldStage",
    newStage = "NewStage",
    survivalTime = "SurvivalTime",
    event = "Event",
    eventLevel = "1"
)

# Print the results to check for errors
print(results)