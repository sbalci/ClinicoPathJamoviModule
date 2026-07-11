# Paired primary / metastasis biomarker data in long format (2 rows per case):
# three markers with differing concordance (HER2 high, ER moderate, PDL1 lower).
set.seed(2026)
ncase <- 90
mk_pair <- function(conc_prob) {
    prim <- sample(c("Positive", "Negative"), ncase, TRUE)
    met  <- ifelse(rbinom(ncase, 1, conc_prob) == 1, prim,
                   ifelse(prim == "Positive", "Negative", "Positive"))
    list(prim = prim, met = met)
}
her2 <- mk_pair(0.92); er <- mk_pair(0.80); pdl1 <- mk_pair(0.68)

long <- data.frame(
    case_id = rep(sprintf("Case%03d", seq_len(ncase)), each = 2),
    sample = rep(c("Primary", "Metastasis"), times = ncase),
    HER2 = as.vector(rbind(her2$prim, her2$met)),
    ER   = as.vector(rbind(er$prim, er$met)),
    PDL1 = as.vector(rbind(pdl1$prim, pdl1$met)),
    stringsAsFactors = FALSE)

multifocalconcordance_test_data <- long
usethis::use_data(multifocalconcordance_test_data, overwrite = TRUE)
write.csv(multifocalconcordance_test_data, "data/multifocalconcordance_test_data.csv", row.names = FALSE)
