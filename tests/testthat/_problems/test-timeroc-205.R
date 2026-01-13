# Extracted from test-timeroc.R:205

# prequel ----------------------------------------------------------------------
context("Time-Dependent ROC Analysis")
library(testthat)
library(timeROC)
cancer_data_path <- file.path("..", "..", "data", "timeroc_cancer_biomarker.rda")
cv_data_path <- file.path("..", "..", "data", "timeroc_cardiovascular_risk.rda")
multi_data_path <- file.path("..", "..", "data", "timeroc_multi_biomarker.rda")
edge_data_path <- file.path("..", "..", "data", "timeroc_edge_cases.rda")
if (file.exists(cancer_data_path)) load(cancer_data_path)
if (file.exists(cv_data_path)) load(cv_data_path)
if (file.exists(multi_data_path)) load(multi_data_path)
if (file.exists(edge_data_path)) load(edge_data_path)
source("../../R/timeroc.b.R")
source("../../R/timeroc.h.R")

# test -------------------------------------------------------------------------
result <- timeroc(
    data = timeroc_edge_cases,
    elapsedtime = "time_months", 
    outcome = "event_status",
    marker = "biomarker",
    timepoints = "6, 12"
  )
