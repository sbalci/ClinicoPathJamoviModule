
test_that("Will Rogers Phenomenon is correctly detected", {
  library(survival)
  library(dplyr)
  
  set.seed(123)
  n_stayers_A <- 100
  n_migrators <- 50
  n_stayers_B <- 100
  
  # Survival times (Exponential distribution)
  # High mean = Good survival
  # Stage A (Good): Mean = 20
  # Migrators (Intermediate): Mean = 10
  # Stage B (Bad): Mean = 5
  
  time_A <- rexp(n_stayers_A, rate = 1/20)
  time_Mig <- rexp(n_migrators, rate = 1/10)
  time_B <- rexp(n_stayers_B, rate = 1/5)
  
  # Create dataset
  data <- data.frame(
    id = 1:(n_stayers_A + n_migrators + n_stayers_B),
    time = c(time_A, time_Mig, time_B),
    # Add some censoring (mostly events to keep median calculation simple)
    event = c(rep(1, n_stayers_A - 5), rep(0, 5), 
              rep(1, n_migrators - 2), rep(0, 2),
              rep(1, n_stayers_B - 5), rep(0, 5)),
    
    # Old System: Migrators are in A
    old_stage = factor(c(
      rep("Stage A", n_stayers_A),
      rep("Stage A", n_migrators),
      rep("Stage B", n_stayers_B)
    ), levels = c("Stage A", "Stage B")),
    
    # New System: Migrators are in B
    new_stage = factor(c(
      rep("Stage A", n_stayers_A),
      rep("Stage B", n_migrators),
      rep("Stage B", n_stayers_B)
    ), levels = c("Stage A", "Stage B"))
  )
  
  # Verify the phenomenon manually first
  # Old A: Stayers A + Migrators
  median_old_A <- median(c(time_A, time_Mig))
  # New A: Stayers A
  median_new_A <- median(time_A)
  # Expect New A > Old A (Improvement)
  
  # Old B: Stayers B
  median_old_B <- median(time_B)
  # New B: Stayers B + Migrators
  median_new_B <- median(c(time_B, time_Mig))
  # Expect New B > Old B (Improvement)
  
  # Migrators Median
  median_mig <- median(time_Mig)
  
  print(paste("Old A Median:", median_old_A))
  print(paste("New A Median:", median_new_A))
  print(paste("Old B Median:", median_old_B))
  print(paste("New B Median:", median_new_B))
  print(paste("Migrators Median:", median_mig))
  
  # Condition for detection in code:
  # Migrators < Old Stage (Aggregate) AND Migrators > New Stage (Aggregate)
  # Migrators (10) < Old A (approx 15) -> TRUE
  # Migrators (10) > New B (approx 7.5) -> TRUE
  
  # Run stagemigration function
  # We need to mock the class or load the package. 
  # Assuming this runs in the package environment via devtools::test()
  
  result <- stagemigration(
    data = data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "event",
    eventLevel = "1",
    analysisType = "comprehensive", # Needed to trigger advanced analysis?
    advancedMigrationAnalysis = TRUE, # Trigger Will Rogers
    showWillRogersAnalysis = TRUE,
    institutionVariable = NULL,
    continuousCovariates = NULL,
    categoricalCovariates = NULL,
    competingEventVar = NULL,
    continuousStageVariable = NULL,
    shapCovariates = NULL,
    competingRisksCovariates = NULL,
    stateVariable = NULL,
    transitionTimeVariable = NULL,
    multiStateCovariates = NULL,
    forestCovariates = NULL,
    cureCovariates = NULL,
    intervalCensoringLeftTime = NULL,
    intervalCensoringRightTime = NULL,
    intervalCensoringAdjustVariables = NULL,
    informativeCensoringCovariates = NULL,
    informativeCensoringIPWVariables = NULL,
    concordanceProbabilityAdjustVariables = NULL,
    winRatioDeathVariable = NULL,
    winRatioSecondaryEndpoint = NULL,
    winRatioTertiaryEndpoint = NULL,
    winRatioTimeVariables = NULL,
    frailtyClusterVariable = NULL
  )
  
  # Check results
  print(paste("Result class:", class(result)))
  print(names(result))
  
  # If result is the results object, access directly
  if ("willRogersAnalysis" %in% names(result)) {
      wr_table <- result$willRogersAnalysis$asDF
  } else if ("results" %in% names(result) && "willRogersAnalysis" %in% names(result$results)) {
      wr_table <- result$results$willRogersAnalysis$asDF
  } else {
      stop("Could not find willRogersAnalysis in result")
  }
  
  expect_true(!is.null(wr_table))
  
  print("Will Rogers Table Content:")
  print(wr_table)
  
  # Look for the migration row: Stage A -> Stage B
  migration_row <- wr_table[wr_table$Migration_Pattern == "Stage A \u2192 Stage B", ] # Arrow might be used? Or check rowKey
  
  # Try finding by rowKey if available or flexible matching
  if (nrow(migration_row) == 0) {
      print("Trying alternative row matching...")
      # Check if there is a row with "Stage A" and "Stage B" in it
      migration_row <- wr_table[grepl("Stage A", wr_table$Migration_Pattern) & grepl("Stage B", wr_table$Migration_Pattern), ]
  }
  
  # Check if row exists
  expect_true(nrow(migration_row) == 1)
  
  # Check Evidence
  # The code returns "Strong - Classic Will Rogers pattern"
  print(migration_row)
  expect_equal(as.character(migration_row$Will_Rogers_Evidence), "Strong - Classic Will Rogers pattern")
  
})
