# Synthetic waterfall data with confirmation + ongoing annotation columns.
# For OncoPath waterfall enhancement testing (issue #1: sort, baseline, markers).
set.seed(42)
n <- 30
waterfall_annotation_test_data <- data.frame(
    PatientID    = sprintf("PT%03d", seq_len(n)),
    Response     = round(runif(n, min = -80, max = 45), 1),
    Confirmation = factor(sample(c("Confirmed", "Unconfirmed"), n, replace = TRUE, prob = c(0.7, 0.3))),
    Ongoing      = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.4, 0.6)),
    Arm          = factor(sample(c("Arm A", "Arm B"), n, replace = TRUE)),
    stringsAsFactors = FALSE
)
write.csv(waterfall_annotation_test_data, "data/waterfall_annotation_test_data.csv", row.names = FALSE)
