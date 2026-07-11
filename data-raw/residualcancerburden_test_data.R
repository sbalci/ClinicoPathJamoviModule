# RCB cohort: 200 post-neoadjuvant breast cancer cases with a pCR fraction,
# survival linked to RCB class. Validates against Symmans (2007) worked example (RCB=3.03).
set.seed(2026); n <- 200
pcr <- rbinom(n, 1, 0.25)
d1 <- ifelse(pcr == 1, 0, round(runif(n, 2, 50)))
d2 <- ifelse(pcr == 1, 0, round(d1 * runif(n, 0.6, 1)))
ca <- ifelse(pcr == 1, 0, round(runif(n, 5, 90)))
cis <- round(runif(n, 0, 20))
ln <- ifelse(pcr == 1, 0, rpois(n, 1.5))
dmet <- ifelse(ln == 0, 0, round(runif(n, 1, 20)))
rcb_index <- function(d1, d2, ca, cis, LN, dmet) {
    dprim <- sqrt(d1 * d2); finv <- pmax((1 - cis/100) * (ca/100), 0)
    pb <- finv * dprim; pt <- ifelse(pb <= 0, 0, 1.4 * pb^0.17)
    mi <- 4 * (1 - 0.75^LN) * dmet; mt <- ifelse(mi <= 0, 0, mi^0.17); pt + mt }
idx <- rcb_index(d1, d2, ca, cis, ln, dmet)
cls <- cut(idx, c(-Inf, 0, 1.36, 3.28, Inf), c("RCB-0","RCB-I","RCB-II","RCB-III"), right = TRUE)
hr <- c("RCB-0"=0.5,"RCB-I"=0.8,"RCB-II"=1.3,"RCB-III"=2.2)[as.character(cls)]
time <- round(rexp(n, 0.03 * hr), 1); status <- rbinom(n, 1, 0.7)
residualcancerburden_test_data <- data.frame(
    patient_id = sprintf("P%03d", 1:n),
    tumor_dim1 = d1, tumor_dim2 = d2, pct_cellularity = ca, pct_insitu = cis,
    n_pos_nodes = ln, largest_met_mm = dmet,
    followup_months = time,
    event = factor(ifelse(status == 1, "Event", "Censored"), levels = c("Censored","Event")))
usethis::use_data(residualcancerburden_test_data, overwrite = TRUE)
write.csv(residualcancerburden_test_data, "data/residualcancerburden_test_data.csv", row.names = FALSE)

