# Zero-cell rules for the 2x2 ratio statistics (LR+, LR-, DOR), shared by decision,
# decisioncalculator, decisioncompare and decisioncombine.
#
# Each analysis used to carry its own copy, and all four applied the 0.5 (Haldane-Anscombe)
# correction whenever a NUMERATOR cell was zero as well. With TP = 0, FN = 10, FP = 1, TN = 99
# that printed LR+ 3.06 and DOR 3.16 for a test that found no diseased patient (exact LR+ 0,
# DOR 0): the +0.5 weighs more in the smaller reference group, so the correction reversed the
# direction of the evidence (independent review 2026-10-21, XA-01).
#
# The rule: a ratio is corrected only when its DENOMINATOR is empty and its numerator is not
# (the ratio would otherwise be infinite). An empty numerator gives an exact 0. Empty numerator
# and denominator together (0/0) are not estimable and must be left blank.
#
#   LR+ = Se / (1 - Sp)   numerator TP, denominator FP; 0/0 when the test is never positive
#   LR- = (1 - Se) / Sp   numerator FN, denominator TN; 0/0 when the test is never negative
#   DOR = (TP*TN)/(FP*FN) numerator {TP, TN}, denominator {FP, FN}
#
# A corrected ratio replaces an infinite one, so it must stay above 1. When the empty denominator
# cell sits in a much smaller reference group the +0.5 can pull it below 1 (TP=99 FN=1 FP=10 TN=0
# gives a corrected LR- of 0.33 for a test that was never negative): that ratio is "undefined".
#
# Callers must already have both reference classes (TP + FN > 0 and FP + TN > 0).
# Returns, for each ratio, one of "raw", "zero", "corrected" or "undefined".
.diagRatioRules <- function(TP, FP, FN, TN) {
    h <- 0.5
    corrected <- c(
        lrp = ((TP + h) / (TP + FN + 2 * h)) / ((FP + h) / (FP + TN + 2 * h)),
        lrn = ((FN + h) / (TP + FN + 2 * h)) / ((TN + h) / (FP + TN + 2 * h)),
        dor = ((TP + h) * (TN + h)) / ((FP + h) * (FN + h))
    )
    rule <- function(num_zero, den_zero, value) {
        if (num_zero && den_zero) "undefined"
        else if (den_zero) { if (value > 1) "corrected" else "undefined" }
        else if (num_zero) "zero"
        else "raw"
    }
    list(
        lrp = rule(TP == 0, FP == 0, corrected[["lrp"]]),
        lrn = rule(FN == 0, TN == 0, corrected[["lrn"]]),
        dor = rule(TP == 0 || TN == 0, FP == 0 || FN == 0, corrected[["dor"]])
    )
}

# Upper confidence limit for a "zero" ratio (empty numerator cell). The point estimate stays
# exactly 0 and its interval is [0, U]: the log-scale interval has no lower end at log 0, and
# leaving it blank read as certainty in a small study (FN = 0 in 36% of tables at n1 = 20,
# Se 0.95; audit row F02C fell to 0.882 coverage over the intervals still shown). U is the upper
# limit of the interval the analyses already use - Simel, Samsa and Matchar (1991) for LR+ and
# LR-, Woolf for the DOR - on the table with 0.5 added to every cell (Haldane-Anscombe).
# ratio: "lrp", "lrn" or "dor".
.diagZeroRatioUpper <- function(ratio, TP, FP, FN, TN, conf = 0.95) {
    a <- TP + 0.5; b <- FP + 0.5; c <- FN + 0.5; d <- TN + 0.5
    n1 <- a + c; n0 <- b + d
    est_v <- switch(ratio,
        lrp = c((a / n1) / (b / n0), 1 / a - 1 / n1 + 1 / b - 1 / n0),
        lrn = c((c / n1) / (d / n0), 1 / c - 1 / n1 + 1 / d - 1 / n0),
        dor = c((a * d) / (b * c), 1 / a + 1 / b + 1 / c + 1 / d),
        stop("ratio must be \"lrp\", \"lrn\" or \"dor\""))
    exp(log(est_v[1]) + stats::qnorm(1 - (1 - conf) / 2) * sqrt(est_v[2]))
}
