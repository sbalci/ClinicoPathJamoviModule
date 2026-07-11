# Synthetic waterfall data with confirmation + ongoing annotation columns and a
# Response Category override column. For OncoPath waterfall enhancement testing
# (issue #1: sort, baseline, markers, and new-lesion PD override).
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

# Response Category: mostly consistent with the % value, but with a deliberate
# new-lesion PD case (a patient whose target lesion shrinks but is still PD).
cat_from_pct <- function(p) ifelse(p <= -30, "PR", ifelse(p >= 20, "PD", "SD"))
waterfall_annotation_test_data$Category <- cat_from_pct(waterfall_annotation_test_data$Response)
shrink_idx <- which(waterfall_annotation_test_data$Response < -30)[1]
waterfall_annotation_test_data$Category[shrink_idx] <- "PD"  # new lesion despite shrinkage

write.csv(waterfall_annotation_test_data, "data/waterfall_annotation_test_data.csv", row.names = FALSE)
cat("new-lesion PD patient:", waterfall_annotation_test_data$PatientID[shrink_idx],
    "response =", waterfall_annotation_test_data$Response[shrink_idx], "\n")
