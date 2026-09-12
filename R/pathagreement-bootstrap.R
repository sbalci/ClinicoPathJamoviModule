# Internal bootstrap interval calculation shared by the pathagreement outputs.
# Keeping this numerical primitive outside the analysis class makes it possible
# to review and test the BCa implementation without loading the 6,000-line
# jamovi result orchestration class.
.pathagreementBootstrapInterval <- function(
        bootstrap_values, original, jackknife_values, method = "bca") {
    values <- bootstrap_values[is.finite(bootstrap_values)]
    percentile <- if (length(values) >= 20) {
        stats::quantile(values, c(0.025, 0.975), names = FALSE)
    } else {
        c(NA_real_, NA_real_)
    }
    if (!identical(method, "bca")) {
        return(list(ci = percentile, method = "percentile", fallback = FALSE))
    }

    jack <- jackknife_values[is.finite(jackknife_values)]
    if (length(values) < 20 || length(jack) < 3 || !is.finite(original)) {
        return(list(ci = percentile, method = "percentile", fallback = TRUE))
    }
    probability <- mean(values < original)
    probability <- min(
        max(probability, 1 / (2 * length(values))),
        1 - 1 / (2 * length(values))
    )
    z0 <- stats::qnorm(probability)
    influence <- mean(jack) - jack
    denominator <- 6 * sum(influence^2)^(3 / 2)
    if (!is.finite(denominator) || denominator == 0) {
        return(list(ci = percentile, method = "percentile", fallback = TRUE))
    }
    acceleration <- sum(influence^3) / denominator
    z_alpha <- stats::qnorm(c(0.025, 0.975))
    adjusted <- stats::pnorm(
        z0 + (z0 + z_alpha) / (1 - acceleration * (z0 + z_alpha))
    )
    if (any(!is.finite(adjusted)) || adjusted[1] >= adjusted[2]) {
        return(list(ci = percentile, method = "percentile", fallback = TRUE))
    }
    adjusted <- pmin(
        pmax(adjusted, 1 / (length(values) + 1)),
        length(values) / (length(values) + 1)
    )
    list(
        ci = stats::quantile(values, adjusted, names = FALSE),
        method = "BCa",
        fallback = FALSE
    )
}
