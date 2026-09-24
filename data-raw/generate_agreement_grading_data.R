# Example data for agreement's Pathologist Grading Analysis section.
#
# A simulated prostate biopsy reader study: 150 core biopsies, 6 pathologists, ISUP grade
# groups 1-5, cases from 3 institutions, about 15% of readings missing (an incomplete design,
# which the grading section and the headline Fleiss kappa both use in full). Drawn from the
# cumulative-probit model the section fits, with KNOWN truth, so the example doubles as a
# recovery check:
#   latent_ij = u_i + w_k(i) + v_j + e_ij,  e ~ N(0, 1)
#   case SD 1.6 (s2u 2.56), institution SD 0.4 (s2c 0.16), rater effects v below
#   population latent ICC(2,1) = (s2u + s2c) / (s2u + s2c + var(v) + 1)
#   Pathologist P4 grades HIGHER (v = +0.6) and P2 grades LOWER (v = -0.4); the others 0.
# Run from the package root:  Rscript --vanilla data-raw/generate_agreement_grading_data.R
set.seed(20260924)
n_cases <- 150L
raters <- paste0("P", 1:6)
v <- c(P1 = 0, P2 = -0.4, P3 = 0, P4 = 0.6, P5 = 0, P6 = 0)
institution <- sample(c("Hospital A", "Hospital B", "Hospital C"), n_cases, replace = TRUE,
                      prob = c(0.45, 0.35, 0.20))
w <- c("Hospital A" = 0.3, "Hospital B" = -0.2, "Hospital C" = -0.3)
u <- stats::rnorm(n_cases, 0, 1.6)
thr <- c(-0.9, 0.5, 1.5, 2.4)                         # ISUP GG1..GG5
grades <- paste0("GG", 1:5)
ratings <- sapply(raters, function(j) {
    z <- u + w[institution] + v[[j]] + stats::rnorm(n_cases)
    grades[findInterval(z, thr) + 1L]
})
# About 15% of readings missing, never leaving a case with fewer than 3 readings.
miss <- matrix(stats::runif(n_cases * 6) < 0.15, n_cases, 6)
for (i in seq_len(n_cases)) while (sum(!miss[i, ]) < 3) miss[i, which(miss[i, ])[1]] <- FALSE
ratings[miss] <- NA

agreement_grading <- data.frame(
    case_id = sprintf("PB-%03d", seq_len(n_cases)),
    institution = factor(institution, levels = c("Hospital A", "Hospital B", "Hospital C")),
    stringsAsFactors = FALSE
)
for (j in raters) agreement_grading[[j]] <- factor(ratings[, j], levels = grades, ordered = TRUE)

share <- max(table(unlist(lapply(agreement_grading[raters], as.character)))) / sum(!is.na(ratings))
stopifnot(share < 0.45)                                # stays inside the section's skew limit
usethis::use_data(agreement_grading, overwrite = TRUE)
utils::write.csv(agreement_grading, "data/agreement_grading.csv", row.names = FALSE)
cat(sprintf("agreement_grading: %d cases x %d pathologists, %d of %d readings present, largest grade share %.2f\n",
            n_cases, length(raters), sum(!is.na(ratings)), length(ratings), share))
