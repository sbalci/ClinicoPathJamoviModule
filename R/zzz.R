#' Package startup message
#'
#' Returns the package author / website banner. Called by `.onAttach()` (see
#' `R/zzz.R`) via `packageStartupMessage()`, which routes to the message stream
#' and respects `suppressPackageStartupMessages()`. Available as an exported
#' function so users can print the banner explicitly.
#'
#' @keywords internal
#' @return Invisible NULL (called for side effects).
#' @export
clinicopath_startup_message <- function() {
    packageStartupMessage(
        "Serdar Balci MD Pathologist\nhttps://www.serdarbalci.com/\n"
    )
    invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
    clinicopath_startup_message()
}
