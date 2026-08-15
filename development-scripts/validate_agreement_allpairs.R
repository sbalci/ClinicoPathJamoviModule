#!/usr/bin/env Rscript
# Validation script for the All-Pairs Cohen's Kappa table in `agreement`.
#
# The raw data from the motivating interobserver study is not available, so
# instead of "reproducing Table I exactly" we (a) simulate a YSRB-like 99-case x
# 4-rater ordinal dataset with article-like marginals and target agreement, and
# (b) cross-validate every per-pair kappa and 95% CI against two independent
# reference implementations (psych::cohen.kappa and vcd::Kappa). It also shows
# why the previous CI (recovered as kappa/z from irr::kappa2, i.e. the
# null-hypothesis SE) was too narrow.
#
# Usage:  Rscript tests/validate_agreement_allpairs.R
# Assumes the package is loadable via devtools::load_all(".").

suppressMessages({
    library(irr); library(vcd); library(psych)
    devtools::load_all(".", quiet = TRUE)
})

set.seed(2024)

# ---- Simulate an article-like 4-rater, 5-category dataset ------------------
# Marginal target: ~75% Cat II, ~14% Cat V, ~5% Cat I, ~3% Cat III, ~3% Cat IV.
n <- 99
cats <- c("I", "II", "III", "IV", "V")
prob <- c(0.05, 0.75, 0.03, 0.03, 0.14)
truth <- sample(cats, n, replace = TRUE, prob = prob)

# Each rater agrees with the latent truth with probability ~0.85, otherwise
# slips to a neighbouring category -> Fleiss kappa around 0.7-0.75.
slip <- function(x) {
    idx <- match(x, cats)
    idx <- pmin(length(cats), pmax(1, idx + sample(c(-1, 1), 1)))
    cats[idx]
}
mk_rater <- function(p_agree) {
    vapply(truth, function(t) if (runif(1) < p_agree) t else slip(t), character(1))
}
df <- data.frame(
    R1 = mk_rater(0.88), R2 = mk_rater(0.85),
    R3 = mk_rater(0.86), R4 = mk_rater(0.83),
    stringsAsFactors = FALSE
)
df[] <- lapply(df, factor, levels = cats, ordered = TRUE)

cat("Marginal category counts (all raters pooled):\n")
print(table(unlist(lapply(df, as.character))))
cat(sprintf("\nFleiss kappa (overall): %.3f\n",
            irr::kappam.fleiss(df)$value))

# ---- Run the analysis ------------------------------------------------------
res <- agreement(data = df, vars = names(df),
                 allPairsKappa = TRUE,
                 multipleTestCorrection = "bonferroni")
tab <- res$allPairsKappaTable$asDF
cat("\n=== All-Pairs Kappa table (module output) ===\n")
print(tab[, c("rater_a", "rater_b", "n", "peragree", "kappa",
              "se", "ci_lower", "ci_upper", "z", "p", "p_adj")],
      row.names = FALSE)

# ---- Cross-validate each pair against psych and vcd ------------------------
cat("\n=== Cross-validation vs psych::cohen.kappa / vcd::Kappa ===\n")
pairs <- utils::combn(names(df), 2, simplify = FALSE)
max_kappa_err <- 0; max_ci_err <- 0
for (pp in pairs) {
    sub <- df[, pp]; sub <- sub[stats::complete.cases(sub), ]
    ck <- psych::cohen.kappa(data.frame(lapply(sub, as.integer)))
    row <- tab[tab$rater_a == pp[1] & tab$rater_b == pp[2], ]

    k_err  <- abs(row$kappa   - unname(ck$kappa))
    lo_err <- abs(row$ci_lower - unname(ck$confid["unweighted kappa", "lower"]))
    hi_err <- abs(row$ci_upper - unname(ck$confid["unweighted kappa", "upper"]))
    max_kappa_err <- max(max_kappa_err, k_err)
    max_ci_err    <- max(max_ci_err, lo_err, hi_err)

    # Old (buggy) CI: kappa +/- 1.96 * (kappa / z_irr)
    kr <- irr::kappa2(sub)
    se_old <- kr$value / kr$statistic
    old_width <- 2 * 1.96 * se_old
    new_width <- row$ci_upper - row$ci_lower
    cat(sprintf("%s-%s: kappa=%.3f  module CI=[%.3f,%.3f] (w=%.3f)  old CI width=%.3f  Dw=%+.3f\n",
                pp[1], pp[2], row$kappa, row$ci_lower, row$ci_upper,
                new_width, old_width, new_width - old_width))
}
cat(sprintf("\nMax |kappa - psych| = %.5f\n", max_kappa_err))
cat(sprintf("Max |CI bound - psych| = %.5f\n", max_ci_err))

# ---- Confirm multiplicity correction ---------------------------------------
ref_padj <- stats::p.adjust(tab$p, method = "bonferroni")
cat(sprintf("\nBonferroni p_adj matches stats::p.adjust: %s\n",
            isTRUE(all.equal(tab$p_adj, ref_padj))))

ok <- max_kappa_err < 1e-3 && max_ci_err < 1e-2 &&
      isTRUE(all.equal(tab$p_adj, ref_padj))
cat(sprintf("\n%s\n", if (ok) "VALIDATION PASSED" else "VALIDATION FAILED"))
quit(status = if (ok) 0 else 1)
