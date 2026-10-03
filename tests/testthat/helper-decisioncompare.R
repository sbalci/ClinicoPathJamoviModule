# Calling decisioncompare() across the regeneration boundary.
#
# 1.0.4 added goldNegative, test1Negative, test2Negative and test3Negative so the user
# can name which level is a genuine negative (see NEWS). All four are `type: Level`,
# and the jamovi compiler FORBIDS `default:` on a Level option -- which means the
# generated wrapper in R/decisioncompare.h.R makes every one of them a REQUIRED
# argument.
#
# That creates a window with no single correct call:
#   * before `jmvtools::prepare()` recompiles the .h.R, the wrapper does not declare
#     them and rejects them with "unused arguments (goldNegative = NULL, ...)";
#   * after it does, omitting them fails with
#     'argument "goldNegative" is missing, with no default'.
#
# Rather than leave the suite red on one side of that boundary, route the tests through
# a call that passes exactly the arguments the *currently compiled* wrapper declares.
# The tests then assert real behaviour in both states, and nothing has to be edited
# again when the module is regenerated.
#
# This is a test-harness convenience only. Real user code and the roxygen @examples
# pass the arguments directly, because that is the API once the module is built.
call_decisioncompare <- function(...) {
    args <- list(...)
    accepted <- names(formals(ClinicoPath::decisioncompare))
    dropped <- setdiff(names(args), accepted)

    # Only ever silently drop the forward-compatible Level options. Anything else
    # being unrecognised is a genuine mistake in the test and must surface.
    forward_compatible <- c("goldNegative", "test1Negative", "test2Negative", "test3Negative")
    unexpected <- setdiff(dropped, forward_compatible)
    if (length(unexpected) > 0)
        stop("Unknown decisioncompare() argument(s): ", paste(unexpected, collapse = ", "))

    do.call(ClinicoPath::decisioncompare, args[names(args) %in% accepted])
}

# TRUE once jmvtools::prepare() has compiled the new Level options into the wrapper.
# Use to skip assertions that can only hold post-regeneration.
decisioncompare_has_negative_levels <- function() {
    "goldNegative" %in% names(formals(ClinicoPath::decisioncompare))
}

# McNemar oracles, transcribed from Fagerland, Lydersen & Laake (BMC Med Res Methodol 2013;13:91)
# and never from the module: b = cases only test 1 got right, c = cases only test 2 got right.
# Mid-p, eq. (5): 2 * (one-sided exact p - f/2), f the Bin(b + c, 1/2) probability of the observed
# count; eq. (6) at a tie: 1 - f/2. Point probabilities come from lchoose(), not pbinom()/dbinom().
# Without discordant pairs there is no information, and p = 1 by convention.
dc_mcnemar_midp <- function(b, c) {
    n <- b + c
    if (n == 0) return(1)
    f <- function(x) exp(lchoose(n, x) - n * log(2))
    if (b == c) return(1 - f(b) / 2)
    2 * (sum(f(0:min(b, c))) - f(b) / 2)
}
# Asymptotic test without continuity correction, as the z form: z = (b - c) / sqrt(b + c).
dc_mcnemar_asymptotic <- function(b, c) {
    if (b + c == 0) return(c(stat = NA_real_, p = 1))
    z <- (b - c) / sqrt(b + c)
    c(stat = z^2, p = 2 * stats::pnorm(-abs(z)))
}
