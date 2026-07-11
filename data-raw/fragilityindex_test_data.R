# Test data for fragilityindex: raw 2x2 (group + binary outcome), one row per subject.
set.seed(2026)
mk <- function(events, n, grp, outcome_yes = "Event", outcome_no = "No event") {
    data.frame(arm = grp,
               outcome = factor(c(rep(outcome_yes, events), rep(outcome_no, n - events)),
                                levels = c(outcome_no, outcome_yes)),
               stringsAsFactors = FALSE)
}
fragilityindex_test_data <- rbind(
    mk(10, 100, "Treatment"),
    mk(25, 100, "Control"))
fragilityindex_test_data$arm <- factor(fragilityindex_test_data$arm,
                                        levels = c("Control", "Treatment"))
fragilityindex_test_data <- fragilityindex_test_data[sample(nrow(fragilityindex_test_data)), ]
rownames(fragilityindex_test_data) <- NULL
usethis::use_data(fragilityindex_test_data, overwrite = TRUE)
write.csv(fragilityindex_test_data, "data/fragilityindex_test_data.csv", row.names = FALSE)
