# Create sample IHC data
set.seed(123)  # For reproducibility

# Generate sample data
n_samples <- 100

# Create factor levels for different scoring systems
scoring_4level <- factor(c("-", "1+", "2+", "3+"), ordered = TRUE)
scoring_3level <- factor(c("-", "1+", "2+"), ordered = TRUE)
scoring_binary <- factor(c("-", "+"), ordered = TRUE)

# Create sample data frame
ihc_data <- data.frame(
    SampleID = paste0("S", sprintf("%03d", 1:n_samples)),

    # Common IHC markers with 4-level scoring (-, 1+, 2+, 3+)
    ER = sample(scoring_4level, n_samples, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    PR = sample(scoring_4level, n_samples, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2)),
    HER2 = sample(scoring_4level, n_samples, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),

    # Markers with 3-level scoring (-, 1+, 2+)
    Ki67 = sample(scoring_3level, n_samples, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    p53 = sample(scoring_3level, n_samples, replace = TRUE, prob = c(0.4, 0.3, 0.3)),

    # Binary markers (-, +)
    CD3 = sample(scoring_binary, n_samples, replace = TRUE, prob = c(0.4, 0.6)),
    CD20 = sample(scoring_binary, n_samples, replace = TRUE, prob = c(0.5, 0.5)),
    CD45 = sample(scoring_binary, n_samples, replace = TRUE, prob = c(0.3, 0.7))
)

# Add some patterns/correlations
# Make some ER+/PR+ cases more likely
er_positive <- ihc_data$ER %in% c("2+", "3+")
pr_modify <- sample(c(TRUE, FALSE), sum(er_positive), replace = TRUE, prob = c(0.8, 0.2))
ihc_data$PR[er_positive][pr_modify] <- sample(c("2+", "3+"), sum(pr_modify), replace = TRUE)

# Make high Ki67 more likely with HER2 positive
her2_positive <- ihc_data$HER2 %in% c("2+", "3+")
ihc_data$Ki67[her2_positive] <- sample(scoring_3level, sum(her2_positive),
                                      replace = TRUE, prob = c(0.1, 0.3, 0.6))

# Output first few rows
head(ihc_data)

# Save data
write.csv(ihc_data, "./data/ihc_test_data.csv", row.names = FALSE)
