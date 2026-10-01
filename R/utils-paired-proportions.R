# Paired difference of two proportions, with Newcombe's hybrid score interval (method 10;
# Newcombe 1998, Stat Med 17:2635-50): Wilson intervals for the two marginal proportions,
# combined through the correlation of the paired results, with Newcombe's correction of
# that correlation (its numerator AD - BC is reduced by n/2, floored at 0, when positive).
#
# Callers: decisioncompare (two tests' sensitivity, specificity and accuracy) and
# decisioncombine (a combination strategy against each test it combines). It was a private
# method of decisioncompare; moved here so the two analyses cannot drift apart.
#
# Fixed on the move: sum() of a logical returns an INTEGER, so the four-margin product
# overflowed R's integer range once the margins reached a few hundred - (200, 100, 0, 200)
# gives 300 * 200 * 200 * 300 = 3.6e9 - and became NA, after which `if (NA)` stopped the
# analysis. The products are now formed from doubles (n and the four cells are still
# returned as integers, as before).
#
# x, y: paired results coded 0/1 or FALSE/TRUE (1 = the event, e.g. a positive call among
# diseased cases). Returns NULL when no pair is complete; otherwise the observed difference
# mean(x) - mean(y) with its 95% interval, n, and the four cells.
pairedProportionDifference <- function(x, y) {
    valid <- !is.na(x) & !is.na(y)
    n_pairs <- sum(valid)
    n <- as.numeric(n_pairs)
    if (n == 0) {
        return(NULL)
    }

    x <- x[valid]
    y <- y[valid]

    both_pos <- as.numeric(sum(x == 1 & y == 1))
    x_only <- as.numeric(sum(x == 1 & y == 0))
    y_only <- as.numeric(sum(x == 0 & y == 1))
    both_neg <- as.numeric(sum(x == 0 & y == 0))

    diff <- (x_only - y_only) / n

    wilson <- function(successes, total) {
        p <- successes / total
        z <- stats::qnorm(0.975)
        denominator <- 1 + z^2 / total
        center <- (p + z^2 / (2 * total)) / denominator
        half_width <- z * sqrt(
            (p * (1 - p) + z^2 / (4 * total)) / total
        ) / denominator
        c(lower = max(0, center - half_width),
          upper = min(1, center + half_width))
    }

    p_x <- (both_pos + x_only) / n
    p_y <- (both_pos + y_only) / n
    ci_x <- wilson(both_pos + x_only, n)
    ci_y <- wilson(both_pos + y_only, n)
    dl_x <- p_x - ci_x[["lower"]]
    du_x <- ci_x[["upper"]] - p_x
    dl_y <- p_y - ci_y[["lower"]]
    du_y <- ci_y[["upper"]] - p_y

    phi_denominator <- sqrt(
        (both_pos + x_only) * (y_only + both_neg) *
            (both_pos + y_only) * (x_only + both_neg)
    )
    phi_numerator <- both_pos * both_neg - x_only * y_only
    if (phi_numerator > 0) {
        phi_numerator <- max(phi_numerator - n / 2, 0)
    }
    phi <- if (phi_denominator == 0) 0 else phi_numerator / phi_denominator

    lower_distance <- sqrt(max(
        0, dl_x^2 - 2 * phi * dl_x * du_y + du_y^2
    ))
    upper_distance <- sqrt(max(
        0, du_x^2 - 2 * phi * du_x * dl_y + dl_y^2
    ))
    lower <- max(-1, diff - lower_distance)
    upper <- min(1, diff + upper_distance)

    list(
        diff = diff,
        lower = lower,
        upper = upper,
        n = n_pairs,
        counts = stats::setNames(as.integer(c(both_pos, x_only, y_only, both_neg)),
                                 c("both_pos", "x_only", "y_only", "both_neg"))
    )
}
