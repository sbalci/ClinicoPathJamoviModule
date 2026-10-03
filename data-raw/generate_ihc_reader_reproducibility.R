# Entirely synthetic independent readings; no patient information.
withr::with_seed(20261003, {
  n <- 180L
  patient <- rep(seq_len(n / 2L), each = 2L)
  gold <- rep(sample(c("Absent", "Present"), n / 2L, TRUE), each = 2L)
  signal <- ifelse(gold == "Present", 58, 34) + rep(rnorm(n / 2L, 0, 10), each = 2L)
  ihc_reader_reproducibility <- data.frame(
    case_id = sprintf("Fictional-C%03d", seq_len(n)),
    patient_id = sprintf("Fictional-P%03d", patient),
    reference = factor(gold, levels = c("Absent", "Present")))
  for (j in 1:3) {
    score <- pmax(0, pmin(100, round(signal + c(-3, 0, 4)[j] + rnorm(n, 0, 9))))
    score[seq.int(12 + j, n, 29)] <- NA_real_
    ihc_reader_reproducibility[[paste0("score", j)]] <- score
    ihc_reader_reproducibility[[paste0("grade", j)]] <- cut(score,
      breaks = c(-Inf, 24, 49, 74, Inf), labels = c("Absent", "Weak", "Moderate", "Strong"),
      ordered_result = TRUE)
    label <- ifelse(is.na(score), NA, ifelse(score >= 50, "Positive", "Negative"))
    label[c(20, 70) + j] <- "Indeterminate"
    ihc_reader_reproducibility[[paste0("call", j)]] <- factor(label,
      levels = c("Negative", "Positive", "Indeterminate"))
  }
  save(ihc_reader_reproducibility, file = "data/ihc_reader_reproducibility.rda", compress = "xz")
  utils::write.csv(ihc_reader_reproducibility,
    "data-raw/non-rda/ihc_reader_reproducibility.csv", row.names = FALSE, na = "")
  jmvReadWrite::write_omv(ihc_reader_reproducibility,
    "data-raw/non-rda/ihc_reader_reproducibility.omv", frcWrt = TRUE)
})
