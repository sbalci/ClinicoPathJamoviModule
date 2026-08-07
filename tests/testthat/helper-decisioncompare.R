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
