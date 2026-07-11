# Synoptic-report completeness audit data: one row per report, 8 required CAP-style
# elements coded present ("Reported") or absent (NA), by subspecialty, over 24 months,
# with completeness improving over time (e.g. after a template rollout).
set.seed(2026)
nr <- 240
month <- sample(1:24, nr, TRUE)
subspecialty <- factor(sample(c("GI", "GU", "Breast", "Gyn"), nr, TRUE))
base_p <- pmin(0.99, 0.70 + 0.010 * month)
elem_names <- c("tumor_site", "histologic_type", "grade", "tumor_size",
                "margins", "lymphovascular_invasion", "nodes_examined", "stage")
mk <- function() ifelse(rbinom(nr, 1, base_p) == 1, "Reported", NA_character_)
d <- data.frame(report_id = sprintf("R%04d", seq_len(nr)),
                subspecialty = subspecialty, month = month,
                stringsAsFactors = FALSE)
for (e in elem_names) d[[e]] <- mk()

synopticcompleteness_test_data <- d
usethis::use_data(synopticcompleteness_test_data, overwrite = TRUE)
write.csv(synopticcompleteness_test_data, "data/synopticcompleteness_test_data.csv", row.names = FALSE)
