# Tumor-budding cohort: bud counts (one hotspot field per case) with a survival outcome
# whose hazard increases with budding grade. Field area = standard 0.785 mm2.
set.seed(2026)
n <- 180
# draw bud counts spanning all three ITBCC grades
buds <- rpois(n, lambda = sample(c(2, 7, 13), n, TRUE, c(0.45, 0.30, 0.25)))
grade_int <- ifelse(buds <= 4, 1L, ifelse(buds <= 9, 2L, 3L))
os_time  <- round(rexp(n, rate = 0.04 * exp(0.5 * (grade_int - 1))), 1)
os_event <- rbinom(n, 1, 0.7)
site <- factor(sample(c("Colon", "Rectum"), n, TRUE))

tumorbudding_test_data <- data.frame(
    case_id = sprintf("C%03d", seq_len(n)),
    bud_count = buds,
    tumor_site = site,
    os_months = os_time,
    os_event = os_event)

usethis::use_data(tumorbudding_test_data, overwrite = TRUE)
write.csv(tumorbudding_test_data, "data/tumorbudding_test_data.csv", row.names = FALSE)
