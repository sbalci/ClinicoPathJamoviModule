# Synthetic, independent-patient fixture for psychopdaROC release checks.
# Run from the repository root. Contains no patient information.
generate_psychopdaROC_release_data <- function(output = "data/psychopdaROC_release.csv",
  rda_output = sub("[.]csv$", ".rda", output)) {
  set.seed(86010)
  n <- 240L
  positive <- rep(c(FALSE, TRUE), c(160L, 80L))
  marker <- rnorm(n, mean = 1.1 * positive, sd = 1)
  candidate <- 0.5 * marker + rnorm(n, mean = 0.6 * positive, sd = 0.8)
  candidate[c(4, 22, 63, 116, 174, 213)] <- NA_real_
  data <- data.frame(
    patient_id = sprintf("SYN%03d", seq_len(n)),
    outcome = ifelse(positive, "Positive", "Negative"),
    marker_a = marker,
    marker_b = candidate,
    marker_affine = 100 * marker + 50,
    marker_inverse = -marker,
    risk_score = plogis(-1 + marker),
    site = rep(c("Site A", "Site B"), length.out = n),
    check.names = FALSE
  )
  data[["Tümör score (µg)"]] <- marker
  data[["A+B [assay]"]] <- candidate
  utils::write.csv(data, output, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  psychopdaROC_release <- data
  save(psychopdaROC_release, file = rda_output, compress = "xz", version = 2)
  invisible(data)
}

if (sys.nframe() == 0L) generate_psychopdaROC_release_data()
