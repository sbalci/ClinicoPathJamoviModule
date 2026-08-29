# Calling decisioncombine() across the regeneration boundary.
#
# The release review renamed the `addPatternToData` Bool to an `addedPattern` option of
# `type: Output`, because jmvcore's Output element resolves its own enabled state with
# options$get(<element name>) -- so an Output result item REQUIRES a same-named Output
# option, and without one the column was computed and then never written by jamovi.
#
# That rename only reaches the public wrapper when someone runs jmvtools::prepare(), which
# is module-wide and deliberately left to the user. Until then the wrapper still declares
# `addPatternToData` and rejects `addedPattern` with "unused arguments", and afterwards the
# reverse. Rather than leave the suite red on one side of that boundary, pass whichever of
# the two the CURRENTLY COMPILED wrapper actually declares. The assertions are unchanged
# and real in both states; nothing needs editing again after regeneration.
.dcomb_pattern_arg <- function(value = TRUE) {
    formals_now <- names(formals(ClinicoPath::decisioncombine))
    if ("addedPattern" %in% formals_now) return(list(addedPattern = value))
    if ("addPatternToData" %in% formals_now) return(list(addPatternToData = value))
    list()
}

# TRUE once the .a.yaml rename has been compiled into R/decisioncombine.h.R.
.dcomb_output_option_live <- function() {
    "addedPattern" %in% names(formals(ClinicoPath::decisioncombine))
}
