# Extracted from test-timeroc.R:433

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
data_clean <- na.omit(timeroc_cancer_biomarker[c("follow_up_months", "death_event", "tumor_biomarker")])
direct_result <- timeROC::timeROC(
    T = data_clean$follow_up_months,
    delta = data_clean$death_event,
    marker = data_clean$tumor_biomarker,
    cause = 1,
    times = c(36),
    ROC = TRUE
  )
wrapper_result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    marker = "tumor_biomarker",
    timepoints = "36"
  )
