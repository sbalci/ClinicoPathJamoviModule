# Patient-level adverse-event test data for the aeplot butterfly analysis.
set.seed(7)
terms <- c("Fatigue", "Nausea", "Neutropenia", "Anemia", "Diarrhea",
           "Rash", "Fever", "Headache")
arms <- c("Experimental", "Control")
rows <- list()
for (arm in arms) {
    for (subj in seq_len(120)) {
        id <- sprintf("%s-%03d", substr(arm, 1, 3), subj)
        n_ae <- rpois(1, lambda = if (arm == "Experimental") 2.5 else 1.8)
        if (n_ae == 0) next
        for (k in seq_len(n_ae)) {
            rows[[length(rows) + 1]] <- data.frame(
                SubjectID = id,
                Arm = arm,
                AETerm = sample(terms, 1),
                Grade = sample(1:5, 1, prob = c(0.35, 0.30, 0.20, 0.10, 0.05)),
                stringsAsFactors = FALSE
            )
        }
    }
}
aeplot_test_data <- do.call(rbind, rows)
write.csv(aeplot_test_data, "data/aeplot_test_data.csv", row.names = FALSE)
cat("rows:", nrow(aeplot_test_data), "\n")
