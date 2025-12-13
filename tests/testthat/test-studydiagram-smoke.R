
context("StudyDiagram Smoke Test")

test_that("Step Summary Logic", {
  # Mock Data
  df <- data.frame(
     step = c("Screened", "Eligible", "Enrolled"),
     count = c(100, 80, 75)
  )
  
  # Initialize
  module <- studydiagram(
     data = df,
     data_format = "step_summary",
     participant_count = "count",
     step_name = "step",
     exclusion_reason_summary = "step",
     # Unused args must be NULL
     participant_id = NULL,
     step_excluded = NULL,
     exclusion_reason_participant = NULL,
     participant_id_mapping = NULL,
     exclusion_reason_mapping = NULL,
     step1_exclusions = NULL,
     step2_exclusions = NULL,
     step3_exclusions = NULL,
     step4_exclusions = NULL,
     step5_exclusions = NULL
  )
  

  
  # Check flowSummary table
  # Step 1: 100, Excluded 0
  # Step 2: 80, Excluded 20
  # Step 3: 75, Excluded 5
  
  # Note: jamovi tables are populated in Results
  # We need to inspect the populated table.
  # The table is module$flowSummary
  
  # Since we can't easily inspect row objects in test mode without jmvcore internals, 
  # we check if the diagram description contains the summary.
  # "Initial N: 100", "Final N: 75", "Retention: 75%"
  
  # Wait, show_interpretation defaults to FALSE. We should enable it to check text.
  module <- studydiagram(
     data = df,
     data_format = "step_summary",
     participant_count = "count",
     step_name = "step",
     show_interpretation = TRUE,
     participant_id = NULL,
     step_excluded = NULL,
     exclusion_reason_participant = NULL,
     participant_id_mapping = NULL,
     exclusion_reason_mapping = NULL,
     step1_exclusions = NULL,
     step2_exclusions = NULL,
     step3_exclusions = NULL,
     step4_exclusions = NULL,
     step5_exclusions = NULL
  )

  
  desc <- module$diagram$content
  expect_match(desc, "Initial N: 100")
  expect_match(desc, "Final N: 75")
  expect_match(desc, "Retention: 75%")
})

test_that("Exclusion Mapping Logic (Sequential)", {
  # 10 participants
  # 1-10. 
  # Step 1 Excl: ID 1, 2 (Reason A)
  # Step 2 Excl: ID 2, 3 (Reason B) -> ID 2 already out! Should not double count.
  
  df <- data.frame(
     id = 1:10,
     reason = c("A", "A", "B", "B", "C", "C", "D", "D", "E", "E")
  )
  # Levels: A, B, C, D, E
  # Step1 Excl: A (ID 1, 2)
  # Step2 Excl: B (ID 3, 4) - Note ID 2 had A, but here distinct IDs for simplicity of setup first.
  
  # Let's make ID 2 have "A" and ID 3 have "B".
  # If we want to test overlap, we need a dataset where one PID could trigger multiple exclusions?
  # But the data format "Exclusion Mapping" assumes one row per participant with ONE reason column?
  # Ah, yes. "Exclusion Reason Variable". So each participant has ONE main reason recorded in data.
  # In that case, sequential logic just means:
  # "Removes those with Reason A first". "Then removes those with Reason B from the remainder".
  # If reasons are mutually exclusive per row (factor levels), then order doesn't change counts
  # UNLESS a person has valid reason "A" and "B"?? No, factor variable.
  
  # Wait, the "Exclusion Mapping" format usually implies:
  # "Variable indicating exclusion reason". 
  # If a person is excluded for "A", they are out.
  # If the user sets Step 1 to remove "A" and Step 2 to remove "B".
  # The counts will be correct (sum(A), sum(B)).
  # The sequential part only matters if we were applying *filtering criteria* on multiple variables.
  # BUT here we are mapping levels of a SINGLE variable.
  # So actually, sequential or not, the sets are disjoint (a row has only 1 reason).
  # UNLESS the user maps the SAME level to multiple steps? (e.g. "Other" in Step 1 and Step 2).
  # Then sequential prevents double counting!
  
  df$reason <- factor(df$reason)
  
  module <- studydiagram(
     data = df,
     data_format = "exclusion_mapping",
     participant_id_mapping = "id",
     exclusion_reason_mapping = "reason",
     step1_exclusions = "A", # Removes 2 (ID 1,2)
     step2_exclusions = "A",  # Try to remove "A" AGAIN in Step 2. Should be 0 found!
     participant_id = NULL,
     step_excluded = NULL,
     exclusion_reason_participant = NULL,
     step_name = NULL,
     participant_count = NULL,
     exclusion_reason_summary = NULL,
     step3_exclusions = NULL,
     step4_exclusions = NULL,
     step5_exclusions = NULL
  )
  

  
  # Check Diagram Content
  # Start: 10
  # Step 2 N: 10 - 2 = 8.
  # Step 3 N: 8 - 0 = 8. (Since A's are already gone).
  # If it wasn't sequential/filtered, it might find 2 "A"s again.
  
  # Enabled interpretation to check N's
  module <- studydiagram(
     data = df,
     data_format = "exclusion_mapping",
     participant_id_mapping = "id",
     exclusion_reason_mapping = "reason",
     step1_exclusions = "A", 
     step2_exclusions = "A",
     show_interpretation = TRUE,
     participant_id = NULL,
     step_excluded = NULL,
     exclusion_reason_participant = NULL,
     step_name = NULL,
     participant_count = NULL,
     exclusion_reason_summary = NULL,
     step3_exclusions = NULL,
     step4_exclusions = NULL,
     step5_exclusions = NULL
  )
  module$run()
  
  desc <- module$diagram$content
  # Retention: 8/10 = 80%
  expect_match(desc, "Final N: 8")
  expect_match(desc, "Retention: 80%")
})
